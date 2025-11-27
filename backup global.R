library(openai)
#Sys.setenv(OPENAI_API_KEY = "your API key")
# or write in your .Renviron file( Wei write Josh's API key into our project .Renviron file)
library(dplyr)
library(purrr)

# ----------------------------
# Load RAG embeddings at startup
# ----------------------------
rag_embeddings <- readRDS("www/rag_embeddings_full.rds")

rag_embeddings <- rag_embeddings %>% 
  mutate(embedding = purrr::map(embedding, as.numeric))

embedding_model <- "text-embedding-3-small"

# ----------------------------
# Cosine similarity function
# ----------------------------
cos_sim <- function(vec1, vec2) {
  sum(vec1 * vec2) / (sqrt(sum(vec1 * vec1)) * sqrt(sum(vec2 * vec2)))
}

# ----------------------------
# Retrieve top-k relevant documents
# ----------------------------
rag_retrieve <- function(query, k = 5) {
  
  q <- tolower(trimws(query))
  
  # ======================================================
  # 1. Compute embedding
  # ======================================================
  emb <- openai::create_embedding(
    model = embedding_model,
    input = query
  )
  query_emb <- unlist(emb$data$embedding[[1]])
  
  # ======================================================
  # 2. Embedding similarity
  # ======================================================
  sims <- sapply(rag_embeddings$embedding, function(e) {
    e <- unlist(e)
    sum(query_emb * e) /
      (sqrt(sum(query_emb^2)) * sqrt(sum(e^2)))
  })
  
  docs <- rag_embeddings
  docs$sim_embedding <- sims
  
  # ======================================================
  # 3. Fuzzy institution match
  # ======================================================
  inst_list <- unique(na.omit(docs$instnm))
  q_clean <- gsub("[^a-zA-Z0-9 ]", "", q)
  
  inst_dist <- stringdist::stringdist(tolower(inst_list), q_clean, method="jw")
  
  matched_inst <- ifelse(min(inst_dist) < 0.25,
                         inst_list[which.min(inst_dist)],
                         "")
  
  # ======================================================
  # 4. Fuzzy CZ match
  # ======================================================
  cz_list <- unique(na.omit(docs$cz_label))
  cz_dist <- stringdist::stringdist(tolower(cz_list), q_clean, method="jw")
  
  matched_cz <- ifelse(min(cz_dist) < 0.25,
                       cz_list[which.min(cz_dist)],
                       "")
  
  # ======================================================
  # 5. Extract year if present
  # ======================================================
  extracted_year <- suppressWarnings(as.numeric(stringr::str_extract(q, "(19|20)[0-9]{2}")))
  if (is.na(extracted_year)) extracted_year <- -1
  
  # Detect intent flags
  want_inst <- str_detect(q, "college|school|institution|community college| cc ")
  want_cz   <- str_detect(q, "cz|commuting|job|jobs|posting")
  
  # ======================================================
  # 6. Narrowing — FINAL SAFE VERSION
  # ======================================================
  docs <- docs %>%
    mutate(
      narrow_inst = ifelse(safe_equals(instnm, matched_inst), 1, 0),
      narrow_cz   = ifelse(safe_equals(cz_label, matched_cz), 1, 0),
      narrow_year = ifelse(safe_equals(year, extracted_year), 1, 0),
      narrow_type = ifelse(
        (want_inst & doc_type %in% c("institution", "institution_year")) |
          (want_cz   & doc_type %in% c("cz", "cz_year")),
        1, 0
      )
    )
  
  # ======================================================
  # 7. Final scoring
  # ======================================================
  docs <- docs %>%
    mutate(
      score = sim_embedding * 0.70 +
        narrow_inst   * 0.12 +
        narrow_cz     * 0.10 +
        narrow_year   * 0.05 +
        narrow_type   * 0.03
    ) %>%
    arrange(desc(score)) %>%
    slice_head(n = k)
  
  return(docs)
}



rag_answer <- function(query, k = 5) {
  
  # 1. RAG retrieval
  retrieved <- rag_retrieve(query, k = k)
  
  context_block <- paste(retrieved$text, collapse = "\n\n---\n\n")
  
  # 2. Construct prompt
  prompt <- paste0(
    "Use ONLY the facts from the following AIREA data context.\n",
    "If the question cannot be answered from this data, say so.\n\n",
    "=== AIREA DATA CONTEXT START ===\n",
    context_block,
    "\n=== AIREA DATA CONTEXT END ===\n\n",
    "User question: ", query, "\n\n",
    "Answer:"
  )
  
  # 3. Call LLM
  completion <- openai::create_chat_completion(
    model = "gpt-4o-mini",
    messages = list(list(role="user", content=prompt))
  )
  
  # 4. Extract answer
  answer_text <- completion$choices$message.content[[1]]
  
  return(answer_text)
}



