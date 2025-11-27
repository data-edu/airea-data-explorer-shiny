library(dplyr)
library(purrr)
library(glue)

# inst_clean: 你的 institution 原始清洗数据（正确）
# 必须包含: instnm, cz_label, year, total_credentials, airea_credentials, airea_pct, lon, lat

# --------- Generate institution-year RAG documents ---------

#STEP A1 — Institution-Year 文档生成代码
inst_year_docs <- inst_clean %>%
  mutate(
    doc_id = paste0("INST_", gsub("[^A-Za-z0-9]", "_", instnm), "_", year),
    doc_type = "institution_year",
    text = pmap_chr(
      list(instnm, cz_label, year, total_credentials, airea_credentials, airea_pct, lon, lat),
      function(name, cz, yr, tot, airea, pct, lo, la) {
        glue("
Institution: {name}
Commuting Zone: {cz}
Year: {yr}

Total Credentials Awarded: {tot}
AIREA Credentials: {airea}
AIREA Percentage: {round(pct,1)}%

Location: ({round(lo,4)}, {round(la,4)})

This document describes a single institution in a single year.
        ")
      }
    )
  )


#🌟 STEP A2 — 添加趋势 summary（AI 模型会用到）

# compute trend for each institution
inst_trend <- inst_clean %>%
  group_by(instnm) %>%
  summarise(
    trend = paste0(year, ": ", total_credentials, collapse = "\n")
  )

inst_year_docs <- inst_year_docs %>%
  left_join(inst_trend, by = "instnm") %>%
  mutate(
    text = paste0(text, "\n\nTrend (2010–2023):\n", trend)
  ) %>%
  select(-trend)

#🌟 STEP A3 — CZ-Year 文档生成代码
cz_year_docs <- cz_clean %>%
  mutate(
    doc_id = paste0("CZ_", gsub("[^A-Za-z0-9]", "_", cz_label), "_", year),
    doc_type = "cz_year",
    text = pmap_chr(
      list(cz_label, year, total_postings, airea_job_posting, airea_pct, per_1000, population),
      function(cz, yr, tot, airea, pct, p1000, pop) {
        glue("
Commuting Zone: {cz}
Year: {yr}

Total Job Postings: {tot}
AIREA Job Postings: {airea}
AIREA Share: {round(pct,1)}%
AIREA Posts per 1,000 Residents: {round(p1000,1)}
Population: {pop}

This document describes AIREA job demand in a single CZ in a single year.
        ")
      }
    )
  )

#🌟 STEP A4 — 添加 trend summary（CZ）
cz_trend <- cz_clean %>%
  group_by(cz_label) %>%
  summarise(
    trend = paste0(year, ": ", airea_job_posting, collapse = "\n")
  )

cz_year_docs <- cz_year_docs %>%
  left_join(cz_trend, by = "cz_label") %>%
  mutate(
    text = paste0(text, "\n\nTrend (2010–2023):\n", trend)
  ) %>%
  select(-trend)


#STEP A5 — 合并所有文档生成最终 RAG 存储库
rag_documents <- bind_rows(
  inst_year_docs,
  inst_docs,        # ← 你原来的 institution summary
  cz_year_docs,
  cz_docs           # ← 你原来的 CZ summary
)

saveRDS(rag_documents, "www/rag_documents_full.rds")


# STEP A6 — 生成 embeddings（更新版 B5）
###############################################
#  AIREA — Embedding Builder (Full RAG)
#  Production-grade • Safe • Auto-resume
###############################################

library(dplyr)
library(purrr)
library(openai)
library(progress)
library(jsonlite)

# ---------------------------------------------
# 0. Load documents
# ---------------------------------------------
rag_documents <- readRDS("www/rag_documents_full.rds")

message("Loaded documents: ", nrow(rag_documents))

# ---------------------------------------------
# 1. Select model
# ---------------------------------------------
embedding_model <- "text-embedding-3-small"

# ---------------------------------------------
# 2. Retry wrapper for API calls
# ---------------------------------------------
safe_embed <- function(text, retries = 5, wait = 2) {
  for (i in 1:retries) {
    try({
      res <- openai::create_embedding(
        model = embedding_model,
        input = text
      )
      return(res$data$embedding[[1]])
    }, silent = TRUE)
    
    message("Retry ", i, "/", retries, " after failure…")
    Sys.sleep(wait * i)
  }
  stop("Embedding failed after retries.")
}

# ---------------------------------------------
# 3. Initialize progress bar
# ---------------------------------------------
pb <- progress_bar$new(
  total = nrow(rag_documents),
  format = "Embedding [:bar] :percent | ETA: :eta | :current/:total"
)

# ---------------------------------------------
# 4. Auto-resume support
# ---------------------------------------------
emb_path <- "www/rag_embeddings_full.rds"

if (file.exists(emb_path)) {
  message("Resuming from previous progress…")
  rag_embeddings <- readRDS(emb_path)
} else {
  rag_embeddings <- rag_documents %>%
    mutate(embedding = vector("list", n()))
}

# ---------------------------------------------
# 5. Main embedding loop
# ---------------------------------------------
for (i in seq_len(nrow(rag_embeddings))) {
  
  # Skip if already done
  if (!is.null(rag_embeddings$embedding[[i]]) &&
      length(rag_embeddings$embedding[[i]]) > 0) {
    pb$tick()
    next
  }
  
  txt <- rag_embeddings$text[[i]]
  
  emb <- safe_embed(txt)
  
  rag_embeddings$embedding[[i]] <- emb
  
  pb$tick()
  
  # Save every 200 rows
  if (i %% 200 == 0) {
    saveRDS(rag_embeddings, emb_path)
    message("== Auto-saved at row ", i)
  }
}

# ---------------------------------------------
# 6. Final save
# ---------------------------------------------
saveRDS(rag_embeddings, emb_path)
message("\n\n🎉 Embedding completed successfully!")
message("Saved to: ", emb_path)

