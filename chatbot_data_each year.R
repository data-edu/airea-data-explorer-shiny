###############################################################################
# LOAD REQUIRED LIBRARIES
###############################################################################
library(dplyr)
library(purrr)
library(glue)


###############################################################################
# A1. GENERATE INSTITUTION-YEAR DOCUMENTS
# These documents represent a single institution in a single year.
###############################################################################

# inst_clean must include:
# instnm, cz_label, year, total_credentials, airea_credentials,
# airea_pct, lon, lat

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


###############################################################################
# A2. ADD TREND SUMMARY TO INSTITUTION-YEAR DOCUMENTS
# Trends summarize credentials awarded over time for each institution.
###############################################################################

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


###############################################################################
# A3. GENERATE CZ-YEAR DOCUMENTS
# These represent AIREA job demand for a single CZ in a single year.
###############################################################################

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


###############################################################################
# A4. ADD TREND SUMMARY TO CZ-YEAR DOCUMENTS
# Trends summarize multi-year postings for each CZ.
###############################################################################

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


###############################################################################
# A5. MERGE ALL DOCUMENT TYPES INTO A SINGLE RAG CORPUS
# Includes:
#   - Institution-year documents
#   - Institution-level summaries (inst_docs)
#   - CZ-year documents
#   - CZ-level summaries (cz_docs)
###############################################################################

rag_documents <- bind_rows(
  inst_year_docs,
  inst_docs,        # your original institution summary documents
  cz_year_docs,
  cz_docs           # your original CZ summary documents
)

saveRDS(rag_documents, "www/rag_documents_full.rds")


###############################################################################
# A6. FULL RAG EMBEDDING GENERATOR (AUTO-RESUME + RETRIES)
# Production-grade, failure-resistant embedding pipeline.
###############################################################################

library(openai)
library(progress)
library(jsonlite)

# ---------------------------------------------
# 0. Load RAG corpus
# ---------------------------------------------
rag_documents <- readRDS("www/rag_documents_full.rds")
message("Loaded documents: ", nrow(rag_documents))


# ---------------------------------------------
# 1. Select embedding model
# ---------------------------------------------
embedding_model <- "text-embedding-3-small"


# ---------------------------------------------
# 2. Retry-safe embedding wrapper
# ---------------------------------------------
safe_embed <- function(text, retries = 5, wait = 2) {
  for (i in seq_len(retries)) {
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
  stop("Embedding failed after multiple retries.")
}


# ---------------------------------------------
# 3. Initialize progress bar
# ---------------------------------------------
pb <- progress_bar$new(
  total = nrow(rag_documents),
  format = "Embedding [:bar] :percent | ETA: :eta | :current/:total"
)


# ---------------------------------------------
# 4. Enable auto-resume from previous progress
# ---------------------------------------------
emb_path <- "www/rag_embeddings_full.rds"

if (file.exists(emb_path)) {
  message("Resuming from previous embedding progress…")
  rag_embeddings <- readRDS(emb_path)
} else {
  rag_embeddings <- rag_documents %>%
    mutate(embedding = vector("list", n()))
}


# ---------------------------------------------
# 5. Main embedding loop
# ---------------------------------------------
for (i in seq_len(nrow(rag_embeddings))) {
  
  # Skip records where embedding already exists
  if (!is.null(rag_embeddings$embedding[[i]]) &&
      length(rag_embeddings$embedding[[i]]) > 0) {
    pb$tick()
    next
  }
  
  txt <- rag_embeddings$text[[i]]
  emb <- safe_embed(txt)
  
  rag_embeddings$embedding[[i]] <- emb
  pb$tick()
  
  # Auto-save every 200 entries
  if (i %% 200 == 0) {
    saveRDS(rag_embeddings, emb_path)
    message("== Auto-saved progress at row ", i)
  }
}

# ---------------------------------------------
# 6. Final save
# ---------------------------------------------
saveRDS(rag_embeddings, emb_path)

message("\n\n🎉 Embedding completed successfully!")
message("Saved to: ", emb_path)
