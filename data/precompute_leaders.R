# ==============================================================================
# Precompute leader lists for AIREA Data Explorer
# ------------------------------------------------------------------------------
# This script generates small CSVs used by the Shiny app to populate "Top" lists
# without recomputing them at runtime. Run it anytime the underlying data changes.
#
# Outputs (written into the same data/ directory):
# - leaders_supply_airea.csv  (instnm, mean_airea_completions)
# - leaders_supply_pct.csv    (instnm, pct_airea_completions)
# - leaders_cz.csv            (cz_label, mean_airea_posts, mean_pct, mean_per1000)
#
# Usage:
#   Rscript data/precompute_leaders.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(DBI)
  library(duckdb)
})

data_dir <- "data"

# ------------------------------------------------------------------------------
# 1) Supply leaders from supply-table.csv
# ------------------------------------------------------------------------------

message("[1/3] Building supply leaders from supply-table.csv ...")

supply_table_path <- file.path(data_dir, "supply-table.csv")
stopifnot(file.exists(supply_table_path))

supply_tbl_df <- readr::read_csv(supply_table_path, show_col_types = FALSE)

# Ensure column names we need exist and types are numeric
if (!"pct_airea_completions" %in% names(supply_tbl_df)) {
  supply_tbl_df <- supply_tbl_df %>%
    mutate(pct_airea_completions = ifelse(mean_completions > 0,
                                          mean_airea_completions / mean_completions,
                                          0))
}

# Top by mean AIREA completions (keep all rows; app will slice to 150)
leaders_supply_airea <- supply_tbl_df %>%
  select(instnm, mean_airea_completions) %>%
  arrange(desc(mean_airea_completions))

# Top by pct AIREA completions
leaders_supply_pct <- supply_tbl_df %>%
  select(instnm, pct_airea_completions) %>%
  arrange(desc(pct_airea_completions))

readr::write_csv(leaders_supply_airea, file.path(data_dir, "leaders_supply_airea.csv"))
readr::write_csv(leaders_supply_pct,   file.path(data_dir, "leaders_supply_pct.csv"))

message("  -> Wrote leaders_supply_airea.csv and leaders_supply_pct.csv")

# ------------------------------------------------------------------------------
# 2) Demand leaders from demand.duckdb
# ------------------------------------------------------------------------------

message("[2/3] Building CZ leaders from demand.duckdb ...")

demand_db_path <- file.path(data_dir, "demand.duckdb")
stopifnot(file.exists(demand_db_path))

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = demand_db_path, read_only = TRUE)
on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)

demand_tbl <- dplyr::tbl(con, "demand")

leaders_cz <- demand_tbl %>%
  filter(year != 2025) %>%
  select(cz_label, year, total_job_postings, airea, mean_population) %>%
  collect() %>%
  group_by(cz_label, year) %>%
  summarise(
    posts_total = sum(total_job_postings, na.rm = TRUE),
    posts_airea = sum(dplyr::if_else(airea == 1, total_job_postings, 0), na.rm = TRUE),
    pop_year    = mean(mean_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(cz_label) %>%
  summarise(
    mean_airea_posts = mean(posts_airea, na.rm = TRUE),
    mean_pct         = mean(dplyr::if_else(posts_total > 0, posts_airea / posts_total, NA_real_), na.rm = TRUE),
    mean_per1000     = mean(dplyr::if_else(!is.na(pop_year) & pop_year > 0, (posts_airea / pop_year) * 1000, NA_real_), na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(leaders_cz, file.path(data_dir, "leaders_cz.csv"))

message("  -> Wrote leaders_cz.csv")

message("[3/3] Done.")


