###############################################################################
# A. LOAD LIBRARIES
###############################################################################
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(glue)
library(scales)


###############################################################################
# A1. READ & CLEAN RAW JSON DATA FOR CZ (2010–2023)
###############################################################################

years <- 2010:2023

# ---- Function: Read CZ JSON for a single year ----
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

# Load all CZ data across years
cz_raw <- map_df(years, read_cz_json)


###############################################################################
# A2. READ & CLEAN INSTITUTE JSON DATA
###############################################################################

# ---- Helper: Extract lon/lat safely from nested geometry ----
extract_lon_lat <- function(coords) {
  # Case 1: Numeric vector of length 2
  if (is.numeric(coords) && length(coords) == 2) {
    return(list(lon = coords[1], lat = coords[2]))
  }
  
  # Case 2: List structure (e.g., list(c(lon, lat)))
  if (is.list(coords)) {
    for (el in coords) {
      if (is.numeric(el) && length(el) == 2) {
        return(list(lon = el[1], lat = el[2]))
      }
    }
  }
  
  # Fallback: Missing or malformed
  return(list(lon = NA, lat = NA))
}

# ---- Function: Read Institute JSON for a single year ----
read_inst_json <- function(year) {
  f <- sprintf("www/InstituteData_%d.json", year)
  js <- jsonlite::fromJSON(f, flatten = TRUE)
  
  # Extract coordinates
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

# Load all institution data across years
inst_raw <- map_df(years, read_inst_json)


###############################################################################
# A3. CLEAN & STANDARDIZE VARIABLES
###############################################################################

# ---- Clean CZ data ----
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

# ---- Clean Institution data ----
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


###############################################################################
# A4. BUILD SUMMARY TABLES FOR CHATBOT USE (Aggregated)
###############################################################################

# ---- CZ summary (multi-year aggregation) ----
cz_summary <- cz_clean %>%
  group_by(cz_label) %>%
  summarise(
    total_postings = sum(total_postings, na.rm = TRUE),
    airea_postings = sum(airea_job_posting, na.rm = TRUE),
    airea_pct = mean(airea_pct, na.rm = TRUE),
    per_1000 = mean(per_1000, na.rm = TRUE),
    population = mean(population, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Institution summary (multi-year aggregation) ----
inst_summary <- inst_clean %>%
  group_by(instnm, cz_label) %>%
  summarise(
    total_credentials = sum(total_credentials, na.rm = TRUE),
    airea_credentials = sum(airea_credentials, na.rm = TRUE),
    airea_pct = mean(airea_pct, na.rm = TRUE),
    lon = mean(lon, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Name index for retrieval ----
cz_names <- unique(cz_summary$cz_label)
inst_names <- unique(inst_summary$instnm)

name_index <- list(
  cz = cz_names,
  institutions = inst_names
)


###############################################################################
# A5. BUILD RAG DOCUMENTS (TEXT FIELDS USED FOR EMBEDDINGS)
###############################################################################

# ---- CZ documents for retrieval ----
cz_documents <- cz_summary %>%
  mutate(
    text = paste0(
      "Commuting Zone: ", cz_label, ". ",
      "Total job postings: ", total_postings, ". ",
      "AIREA job postings: ", airea_postings, ". ",
      "AIREA percentage: ", round(airea_pct, 1), "%. ",
      "AIREA postings per 1,000 residents: ", round(per_1000, 1), ". ",
      "Population: ", population, "."
    )
  )

# ---- Institution documents for retrieval ----
inst_documents <- inst_summary %>%
  mutate(
    text = paste0(
      "Institution: ", instnm, ". ",
      "Commuting zone: ", cz_label, ". ",
      "Total credentials awarded: ", total_credentials, ". ",
      "AIREA credentials: ", airea_credentials, ". ",
      "AIREA credential percentage: ", round(airea_pct, 1), "%. "
    )
  )

# Pack into RAG knowledge base
rag_kb <- list(
  cz = cz_documents,
  inst = inst_documents,
  index = name_index
)


###############################################################################
# B. ADVANCED DOCUMENT FORMATTING FOR CHATBOT RETRIEVAL (Chunking)
###############################################################################

# ---- CZ formatted documents ----
cz_docs <- cz_summary %>%
  mutate(
    doc_id = paste0("CZ_", gsub("[^A-Za-z0-9]+", "_", cz_label)),
    doc_type = "cz_summary",
    text = glue(
      "Commuting Zone: {cz_label}

Total AIREA Job Postings (2010–2024): {comma(total_postings)}
AIREA Job Postings Share: {round(airea_pct,1)}%
AIREA Posts per 1,000 Residents: {round(per_1000,1)}

Typical CZ Population: {comma(population)} residents

This CZ demonstrates multi-year AIREA-related job demand patterns."
    )
  ) %>%
  select(doc_id, doc_type, cz_label, text)

# ---- Institution formatted documents ----
inst_docs <- inst_summary %>%
  mutate(
    doc_id = paste0("INST_", gsub("[^A-Za-z0-9]+", "_", instnm)),
    doc_type = "institution",
    text = glue(
      "Institution: {instnm}
Commuting Zone: {cz_label}

Total credentials awarded (2010–2023): {comma(total_credentials)}
AIREA credentials awarded: {comma(airea_credentials)}
AIREA credential percentage: {round(airea_pct,1)}%

Institution coordinates: ({round(lon,4)}, {round(lat,4)})

This institution contributes to workforce supply in AIREA-related programs."
    )
  ) %>%
  select(doc_id, doc_type, instnm, cz_label, text)

# ---- Combined KB documents ----
kb_documents <- bind_rows(
  cz_docs %>% mutate(source = "cz"),
  inst_docs %>% mutate(source = "institution")
)

# Save KB RDS
saveRDS(kb_documents, "www/rag_kb_documents.rds")


###############################################################################
# C. GENERATE EMBEDDINGS (RAG VECTOR DATABASE)
###############################################################################

library(progress)
library(openai)

message("Documents loaded: ", nrow(kb_documents))

# Set API key
Sys.setenv(OPENAI_API_KEY = "YOUR-API-KEY-HERE")

embedding_model <- "text-embedding-3-small"

# Progress bar
pb <- progress_bar$new(
  total = nrow(kb_documents),
  format = "Creating embeddings [:bar] :percent :eta"
)

# Create embeddings
kb_embeddings <- kb_documents %>%
  mutate(
    embedding = purrr::map(text, ~{
      pb$tick()
      emb <- openai::create_embedding(
        model = embedding_model,
        input = .x
      )
      emb$data$embedding[[1]]
    })
  )

# Save vector DB
saveRDS(kb_embeddings, "www/rag_embeddings.rds")

message("Embedding generation complete!")
message("Saved to www/rag_embeddings.rds")
