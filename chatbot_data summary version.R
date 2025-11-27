library(jsonlite)



# from Json to RAG summary

# 处理CZ Json

library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

years <- 2010:2023

read_cz_json <- function(year) {
  f <- sprintf("www/CZData_%d.json", year)
  js <- fromJSON(f, flatten = TRUE)
  df <- js$features %>% 
    select(
      CZ20 = properties.CZ20,
      YEAR = properties.YEAR,
      total_postings = properties.total_postings,
      airea_job_posting = properties.airea_job_posting,
      pop = properties.pop,
      pct_green = properties.pct_green,
      per1000 = properties.per1000
    )
  df$cz_label <- paste0("CZ ", df$CZ20)
  df$year <- as.integer(df$YEAR)
  df$source_year <- year
  df
}

cz_raw <- map_df(years, read_cz_json)

# 处理Institute JSons

extract_lon_lat <- function(coords) {
  # 如果 coords 是长度 2 的 numeric vector：正常
  if (is.numeric(coords) && length(coords) == 2) {
    return(list(lon = coords[1], lat = coords[2]))
  }
  
  # 如果 coords 是 list，比如 list(c(lon,lat))
  if (is.list(coords)) {
    # 尝试找到第一个长度为 2 的 numeric 子对象
    for (el in coords) {
      if (is.numeric(el) && length(el) == 2) {
        return(list(lon = el[1], lat = el[2]))
      }
    }
  }
  
  # 如果还是不对，返回 NA
  return(list(lon = NA, lat = NA))
}




read_inst_json <- function(year) {
  f <- sprintf("www/InstituteData_%d.json", year)
  js <- jsonlite::fromJSON(f, flatten = TRUE)
  
  # geometry.coordinates might be nested or inconsistent
  coords_list <- lapply(js$features$geometry.coordinates, extract_lon_lat)
  
  df <- js$features %>%
    transmute(
      instnm  = properties.instnm,
      cz_label = properties.cz_label,
      year     = properties.year,
      inst_cmplt_tot = properties.inst_cmplt_tot,
      inst_green_cmplt_tot = properties.inst_green_cmplt_tot,
      inst_perc_green_tot = properties.inst_perc_green_tot,
      lon = sapply(coords_list, function(x) x$lon),
      lat = sapply(coords_list, function(x) x$lat)
    )
  
  df$source_year <- year
  df
}

inst_raw <- map_df(years, read_inst_json)





### A3. 清洗 + 标准化   清洗 CZ 数据

cz_clean <- cz_raw %>%
  mutate(
    airea_pct = pct_green,
    per_1000 = per1000
  ) %>%
  select(
    cz_label,
    year,
    total_postings,
    airea_job_posting,
    airea_pct,
    per_1000,
    population = pop
  )


inst_clean <- inst_raw %>%
  mutate(
    airea_pct = inst_perc_green_tot * 100
  ) %>%
  select(
    instnm,
    cz_label,
    year,
    total_credentials = inst_cmplt_tot,
    airea_credentials = inst_green_cmplt_tot,
    airea_pct,
    lon,
    lat
  )


#A4. 生成 Chatbot 用的 Summary 表

cz_summary <- cz_clean %>%
  group_by(cz_label) %>%   # ✔ 只按 CZ 聚合
  summarise(
    total_postings = sum(total_postings, na.rm = TRUE),
    airea_postings = sum(airea_job_posting, na.rm = TRUE),
    airea_pct = mean(airea_pct, na.rm = TRUE),
    per_1000 = mean(per_1000, na.rm = TRUE),
    population = mean(population, na.rm = TRUE),
    .groups = "drop"
  )


inst_summary <- inst_clean %>%
  group_by(instnm, cz_label) %>%  # 只按学校聚合（不按年份）
  summarise(
    total_credentials = sum(total_credentials, na.rm = TRUE),
    airea_credentials = sum(airea_credentials, na.rm = TRUE),
    airea_pct = mean(airea_pct, na.rm = TRUE),
    lon = mean(lon, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    .groups = "drop"
  )

#  ③ Name index（用于 Chatbot 检索）⚡ 两个 lookup lists：
cz_names <- unique(cz_summary$cz_label)
inst_names <- unique(inst_summary$instnm)

name_index <- list(
  cz = cz_names,
  institutions = inst_names
)


## A5. 生成 Chatbot-friendly 文档（RAG 文本字段），我们需要一个 text 字段，用于 embedding。，为 CZ 生成 RAG 文本


cz_documents <- cz_summary %>%
  mutate(
    text = paste0(
      "Commuting Zone: ", cz_label, ". ",
      "Year: ", year, ". ",
      "Total job postings: ", total_postings, ". ",
      "AIREA job postings: ", airea_postings, ". ",
      "AIREA percentage: ", round(airea_pct, 1), "%. ",
      "AIREA postings per 1,000 residents: ", round(per_1000, 1), ". ",
      "Population: ", population, "."
    )
  )


inst_documents <- inst_summary %>%
  mutate(
    text = paste0(
      "Institution: ", instnm, ". ",
      "Commuting zone: ", cz_label, ". ",
      "Year: ", year, ". ",
      "Total credentials awarded: ", total_credentials, ". ",
      "AIREA credentials: ", airea_credentials, ". ",
      "AIREA credential percentage: ", round(airea_pct, 1), "%. "
    )
  )

## A6. 最终对象（你后面步骤 B / C 会用到）

rag_kb <- list(
  cz = cz_documents,
  inst = inst_documents,
  index = name_index
)



###### 步骤B 我们要设计：

#文档格式（Document Schema）

#字段结构（Metadata + Text）

#Chatbot 如何根据 Query 检索文档

#检索的单位（Chunking Strategy）

##✔ 输入“Tell me about Austin Community College” → 找到对应 Institution Summary
## ✔ 输入“Which CZ has strong AIREA demand?” → 找到 CZ 信息并总结
## ✔ 输入“Compare Phoenix and Houston CZ for job demand” → 返回两个 CZ 文档的对比
## ✔ 输入“Recommend programs for solar energy jobs near Denver” → 找 CZ → 找高校 → 找 completions

cz_docs <- cz_summary %>%
  mutate(
    doc_id = paste0("CZ_", gsub("[^A-Za-z0-9]+","_", cz_label)),
    doc_type = "cz_summary",
    text = glue::glue(
      "Commuting Zone: {cz_label}

Total AIREA Job Postings (2010–2024): {scales::comma(total_postings)}
AIREA Job Postings Share: {round(airea_pct,1)}%
AIREA Posts per 1,000 Residents: {round(per_1000,1)}

Typical CZ Population: {scales::comma(population)} residents

This CZ demonstrates multi-year AIREA-related job demand patterns."
    )
  ) %>%
  select(doc_id, doc_type, cz_label, text)



inst_docs <- inst_summary %>%
  mutate(
    doc_id = paste0("INST_", gsub("[^A-Za-z0-9]+", "_", instnm)),
    doc_type = "institution",
    text = glue::glue(
      "Institution: {instnm}
Commuting Zone: {cz_label}

Total credentials awarded (2010–2023): {scales::comma(total_credentials)}
AIREA credentials awarded: {scales::comma(airea_credentials)}
AIREA credential percentage: {round(airea_pct,1)}%

Institution coordinates: ({round(lon,4)}, {round(lat,4)})

This institution contributes to workforce supply in AIREA-related programs."
    )
  ) %>%
  select(doc_id, doc_type, instnm, cz_label, text)



###Step 3 — 合并 rag_kb（RAG 知识库存放结构）
kb_documents <- bind_rows(
  cz_docs %>% mutate(source = "cz"),
  inst_docs %>% mutate(source = "institution")
)


## write rds
saveRDS(kb_documents, "www/rag_kb_documents.rds")

nrow(kb_documents)

###
## ============ 2. 设置 OpenAI API Key 到环境变量 ============
os.environ["OPENAI_API_KEY"] = "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"
openai.api_key = os.environ["OPENAI_API_KEY"]
#### 


### ================================================================
### B5 — Generate Embeddings for RAG (CZ + Institution summaries)
### ================================================================

library(dplyr)
library(purrr)
library(progress)
library(openai)

# --------------------------------------------------
# 1. Load your kb_documents (already prepared)
# --------------------------------------------------
# If your kb_documents is already in environment, skip this.
# Otherwise, load it:
# kb_documents <- readRDS("path/to/kb_documents.rds")

# Check structure
print(paste("Documents:", nrow(kb_documents)))

# --------------------------------------------------
# 2. Setup OpenAI API key
# --------------------------------------------------
# If already set in Renviron, skip
 Sys.setenv(OPENAI_API_KEY = "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")

embedding_model <- "text-embedding-3-small"

# --------------------------------------------------
# 1. Setup embedding model
# --------------------------------------------------
embedding_model <- "text-embedding-3-small"

# --------------------------------------------------
# 2. Progress bar
# --------------------------------------------------
pb <- progress_bar$new(
  total = nrow(kb_documents),
  format = "Creating embeddings [:bar] :percent :eta"
)

# --------------------------------------------------
# 3. Compute embeddings
# --------------------------------------------------
kb_embeddings <- kb_documents %>%
  mutate(
    embedding = purrr::map(text, ~{
      pb$tick()
      emb <- openai::create_embedding(
        model = embedding_model,
        input = .x
      )
      # correct format:
      emb$data$embedding[[1]]
    })
  )

# --------------------------------------------------
# 4. Save RAG vector database
# --------------------------------------------------
saveRDS(kb_embeddings, "www/rag_embeddings.rds")

message("Embedding generation complete! Saved to www/rag_embeddings.rds")
message("Embedding generation completed!")
message("Saved to: www/rag_embeddings.rds")




