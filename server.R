# ==============================================================================
# Load Data
# ==============================================================================

library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(stringr)
# library(arrow)  # Removed: now using DuckDB
library(ggplot2)
library(plotly)
library(DT)
library(ggiraph)

# NEW: DuckDB + DBI
library(DBI)
library(duckdb)
library(dbplyr)

# Helper: convert labelled -> factor using labels; else plain factor
label_to_factor <- function(x) {
  if (inherits(x, "labelled")) {
    haven::as_factor(x, levels = "labels")
  } else {
    factor(x)
  }
}

# CIP Group mapping (by first two digits of CIP code)
get_cip_group <- function(cip_code) {
  code_chr <- gsub("[^0-9]", "", as.character(cip_code))
  # Vectorized handling: derive two-digit prefixes, NA if empty
  prefix <- substr(stringr::str_pad(code_chr, 2, pad = "0"), 1, 2)
  prefix[nchar(code_chr) == 0 | is.na(code_chr)] <- NA_character_
  groups <- c(
    "01" = "Agriculture, Agriculture Operations, and Related Sciences",
    "03" = "Natural Resources and Conservation",
    "04" = "Architecture and Related Services",
    "05" = "Area, Ethnic, Cultural, and Gender Studies",
    "09" = "Communication, Journalism, and Related Programs",
    "10" = "Communications Technologies/Technicians and Support Services",
    "11" = "Computer and Information Sciences and Support Services",
    "12" = "Personal and Culinary Services",
    "13" = "Education",
    "14" = "Engineering",
    "15" = "Engineering Technologies/Technicians",
    "16" = "Foreign Languages, Literatures, and Linguistics",
    "19" = "Family and Consumer Sciences/Human Sciences",
    "22" = "Legal Professions and Studies",
    "23" = "English Language and Literature/Letters",
    "24" = "Liberal Arts and Sciences, General Studies and Humanities",
    "25" = "Library Science",
    "26" = "Biological and Biomedical Sciences",
    "27" = "Mathematics and Statistics",
    "28" = "Reserve Officer Training Corps (JROTC, ROTC)",
    "29" = "Military Technologies",
    "30" = "Multi/Interdisciplinary Studies",
    "31" = "Parks, Recreation, Leisure, and Fitness Studies",
    "32" = "Basic Skills",
    "33" = "Citizenship Activities",
    "34" = "Health-Related Knowledge and Skills",
    "35" = "Interpersonal and Social Skills",
    "36" = "Leisure and Recreational Activities",
    "37" = "Personal Awareness and Self-Improvement",
    "38" = "Philosophy and Religious Studies",
    "39" = "Theology and Religious Vocations",
    "40" = "Physical Sciences",
    "41" = "Science Technologies/Technicians",
    "42" = "Psychology",
    "43" = "Security and Protective Services",
    "44" = "Public Administration and Social Service Professions",
    "45" = "Social Sciences",
    "46" = "Construction Trades",
    "47" = "Mechanic and Repair Technologies/Technicians",
    "48" = "Precision Production",
    "49" = "Transportation and Materials Moving",
    "50" = "Visual and Performing Arts",
    "51" = "Health Professions and Related Clinical Sciences",
    "52" = "Business, Management, Marketing, and Related Support Services",
    "53" = "High School/Secondary Diplomas and Certificates",
    "54" = "History",
    "60" = "Residency Programs"
  )
  out <- unname(groups[prefix])
  out[is.na(out)] <- "Other"
  out
}
# SOC Group mapping function
get_soc_group <- function(soc_code) {
  # Extract the 2-digit SOC group code
  soc_group <- substr(as.character(soc_code), 1, 2)
  
  # Map to shortened group names for better chart readability
  soc_group_names <- c(
    "11" = "Management",
    "13" = "Business & Financial Ops", 
    "15" = "Computer & Math",
    "17" = "Architecture & Engineering",
    "19" = "Life/Physical/Social Sci",
    "21" = "Community & Social Service",
    "23" = "Legal",
    "25" = "Education & Library",
    "27" = "Arts, Media & Sports",
    "29" = "Healthcare Practitioners & Tech",
    "31" = "Healthcare Support",
    "33" = "Protective Service",
    "35" = "Food Prep & Serving",
    "37" = "Building & Grounds Maint",
    "39" = "Personal Care & Service",
    "41" = "Sales & Related",
    "43" = "Office & Admin Support",
    "45" = "Farming/Fishing/Forestry",
    "47" = "Construction & Extraction",
    "49" = "Install/Maint/Repair",
    "51" = "Production",
    "53" = "Transportation & Material Moving"
  )
  
  # Return the group name, or "Other" if not found
  ifelse(soc_group %in% names(soc_group_names), 
         soc_group_names[soc_group], 
         "Other")
}

# Function to get full SOC group name for tooltips
get_soc_group_full <- function(soc_code) {
  # Extract the 2-digit SOC group code
  soc_group <- substr(as.character(soc_code), 1, 2)
  
  # Map to full group names for tooltips
  soc_group_names_full <- c(
    "11" = "Management Occupations",
    "13" = "Business and Financial Operations Occupations", 
    "15" = "Computer and Mathematical Occupations",
    "17" = "Architecture and Engineering Occupations",
    "19" = "Life, Physical, and Social Science Occupations",
    "21" = "Community and Social Service Occupations",
    "23" = "Legal Occupations",
    "25" = "Education, Training, and Library Occupations",
    "27" = "Arts, Design, Entertainment, Sports, and Media Occupations",
    "29" = "Healthcare Practitioners and Technical Occupations",
    "31" = "Healthcare Support Occupations",
    "33" = "Protective Service Occupations",
    "35" = "Food Preparation and Serving Related Occupations",
    "37" = "Building and Grounds Cleaning and Maintenance Occupations",
    "39" = "Personal Care and Service Occupations",
    "41" = "Sales and Related Occupations",
    "43" = "Office and Administrative Support Occupations",
    "45" = "Farming, Fishing, and Forestry Occupations",
    "47" = "Construction and Extraction Occupations",
    "49" = "Installation, Maintenance, and Repair Occupations",
    "51" = "Production Occupations",
    "53" = "Transportation and Material Moving Occupations"
  )
  
  # Return the full group name, or "Other" if not found
  ifelse(soc_group %in% names(soc_group_names_full), 
         soc_group_names_full[soc_group], 
         "Other")
}

# Declare global variables to appease linters for NSE in dplyr/ggplot2
utils::globalVariables(c(
  "instnm", "year", "cz_label", "total_completions", "total_students_enrolled",
  "airea_completions", "ciptitle", "award_level", "TOTAL_JOB_POSTS", "AIREA",
  "YEAR", "CZ_label", "population_estimate_sum", "total_posts", "airea_posts",
  "population", "posts_per_1000", "airea_percentage", "total_job_postings",
  "mean_population", "pop_year", "posts_total", "posts_airea", "soc_title",
  "ed_req", "total_postings", "total_soc", "mean_completions", "mean_airea_completions",
  "pct_airea_completions", "mean_students_enrolled", "rural", "tribal", "soc", "cip",
  "soc_group", "soc_group_full", "soc_group_postings", "cip_group", "cip_group_total", "tooltip", "total_airea_completions", "airea_pct",
  "nat_value", "posts_per_100k", "AIREA%", "AIREA%_num", "Rural", "Tribal",
  "Mean Completions (per year)", "Mean AIREA Completions (per year)", "Mean Enrollment (per year)",
  "mean_airea_posts", "mean_pct", "mean_per1000"
))

# ==============================================================================
# Load supply data (institution completions)
# ==============================================================================

## map supply data for tab 1 only (Don't delete, used in map)
mapsupply <- readRDS("data/mapsupply.rds")

# Tab 2 table source (replace interactive table with this CSV)
supply_table_df <- read_csv("data/supply-table.csv", show_col_types = FALSE)

# Precomputed leader lists (optional). Fallback logic below will compute if missing
leaders_supply_airea <- tryCatch(read_csv("data/leaders_supply_airea.csv", show_col_types = FALSE), error = function(e) NULL)
leaders_supply_pct   <- tryCatch(read_csv("data/leaders_supply_pct.csv", show_col_types = FALSE),   error = function(e) NULL)

# National averages for Tab 2 (Degree Completions) and Tab 3 (Job Postings)
supply_nat <- read_csv("data/supply-nat-ave.csv", show_col_types = FALSE)
demand_nat <- read_csv("data/demand-nat-ave.csv", show_col_types = FALSE)

# ------------------------------------------------------------------------------
# REPLACEMENT: use DuckDB instead of Arrow partitioned datasets
# ------------------------------------------------------------------------------

# Open read-only DuckDB databases built offline
con_supply <- dbConnect(duckdb(), dbdir = "data/supply.duckdb", read_only = TRUE)
con_demand <- dbConnect(duckdb(), dbdir = "data/demand.duckdb", read_only = TRUE)

# Lazy tables via dbplyr
supply_tbl <- tbl(con_supply, "supply")
demand_tbl <- tbl(con_demand, "demand")

# On app stop, close connections
onStop(function() {
  try(dbDisconnect(con_supply, shutdown = TRUE), silent = TRUE)
  try(dbDisconnect(con_demand, shutdown = TRUE), silent = TRUE)
})

# Tab 3 table source (CSV summary for CZs)
cz_table_df <- read_csv("data/cz-summary-table.csv")

# Precomputed CZ leaders (optional)
leaders_cz <- tryCatch(read_csv("data/leaders_cz.csv", show_col_types = FALSE), error = function(e) NULL)




# ==============================================================================
# App color palette and plot theme (match CSS)
# ==============================================================================

ccrc_colors <- list(
  green = "#5ca060",
  blue = "#0065a4",
  teal = "#31a2b6",
  purple = "#864f83",
  gray = "#424c49",
  orange = "#E69F00",
  teal_dark = "#278191",
  green_dark = "#4a824e",
  light_teal = "#dff3f6",
  light_green = "#f2f8f2"
)

ccrc_palette <- c(
  ccrc_colors$teal,
  ccrc_colors$green,
  ccrc_colors$blue,
  ccrc_colors$purple,
  ccrc_colors$teal_dark,
  ccrc_colors$green_dark
)

ccrc_theme <- 
  theme_minimal(base_size = 13) +
  theme(
    text = element_text(color = ccrc_colors$gray),
    plot.title = 
      element_text(
        color = ccrc_colors$purple, 
        face = "bold",
        size = 24,
        hjust = 0),
    axis.title = element_text(color = ccrc_colors$gray, face = "bold"),
    axis.text = element_text(color = ccrc_colors$gray),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e8f5e8")
  )




# ==============================================================================
# Server Logic
# ==============================================================================

server <- function(input, output, session) {
  # Centralized selections driven by search or leader dropdowns
  selected_inst_name <- reactiveVal(NULL)
  selected_cz_name <- reactiveVal(NULL)
  
  # Map functionality
  observeEvent(input$cz_metric, {
    session$sendCustomMessage("updateCZMetric", input$cz_metric)
  })
  
  
  
  # ============================================================================
  # Panel 1: Map
  # ============================================================================
  
  # --- 1. Map tab's year selection & search ---
  observe({
    req(input$selected_year_map)
    
    # Update institution dropdown for the selected year
    inst_choices <- sort(
      unique(mapsupply$instnm[mapsupply$year == input$selected_year_map])
    )
    
    updateSelectizeInput(
      session, "search_term",
      choices = inst_choices,
      server  = TRUE
    )
    
    # After UI updates, trigger data load in JS
    session$onFlushed(function() {
      session$sendCustomMessage(
        "loadInstituteYear",
        isolate(input$selected_year_map)
      )
    }, once = TRUE)
    
    # Notify frontend to load CZ data and redraw map
    session$sendCustomMessage("loadYear", input$selected_year_map)
  })
  
  # Handle institution search button click
  observeEvent(input$search_btn, {
    req(input$search_term)
    req(input$selected_year_map)
    
    search_result <- mapsupply %>%
      filter(
        grepl(input$search_term, instnm, ignore.case = TRUE),
        year == input$selected_year_map
      ) %>%
      slice(1)
    
    if (nrow(search_result) > 0) {
      popup_text <- paste0(
        "<strong>", search_result$instnm, "</strong><br>",
        "<strong>CZ:</strong> ", search_result$cz_label, "<br>",
        "<strong>Year:</strong> ", search_result$year, "<br>",
        "<strong>Total Completions:</strong> ",
        format(search_result$inst_cmplt_tot, big.mark = ",", scientific = FALSE), "<br>",
        "<strong>AIREA Completions:</strong> ",
        format(search_result$inst_perc_airea_tot * search_result$inst_cmplt_tot, big.mark = ",", scientific = FALSE), "<br>",
        "<strong>AIREA Percentage:</strong> ",
        sprintf("%.1f%%", search_result$inst_perc_airea_tot * 100)
      )
      coords <- list(
        lng   = search_result$longitud,
        lat   = search_result$latitude,
        popup = popup_text
      )
      session$sendCustomMessage("updateSearch", coords)
    } else {
      showNotification("No Institution Found This Year!", type = "error")
    }
  })
  
  # Do not sync leader selection back into the search UI; mirror Tab 3 behavior
  
  # --- 2. Force map resize when switching to Map tab ---
  observeEvent(input$tabs, {
    if (input$tabs == "mainmap") {
      session$sendCustomMessage("resizeMap", list())
    }
  })
  
  # ============================================================================
  # Panel 2: Degree Completions
  # ============================================================================
  
  # Populate and react to Institution search (Tab 2)
  observe({
      name_map <- names(supply_table_df)
      lower_names <- tolower(name_map)
      inst_candidates <- c("instnm", "institution", "school", "name")
      inst_idx <- match(inst_candidates, lower_names)
      inst_idx <- inst_idx[!is.na(inst_idx)]
      validate(need(length(inst_idx) > 0, "Institution name column not found in supply table"))
      inst_col <- name_map[inst_idx[1]]
      updateSelectizeInput(
        session, "supply_search",
        choices = sort(unique(as.character(supply_table_df[[inst_col]]))), server = TRUE
      )
      
      leaders_airea <- if (!is.null(leaders_supply_airea)) {
        leaders_supply_airea %>%
          mutate(label = paste0(instnm, " — ", scales::comma(round(mean_airea_completions))))
      } else {
        supply_table_df %>%
          select(instnm, mean_airea_completions) %>%
          arrange(desc(mean_airea_completions)) %>%
          slice_head(n = 150) %>%
          mutate(label = paste0(instnm, " — ", scales::comma(round(mean_airea_completions))))
      }
      updateSelectInput(session, "supply_leader_airea",
                        choices = setNames(leaders_airea$instnm, leaders_airea$label))
      
      leaders_pct <- if (!is.null(leaders_supply_pct)) {
        leaders_supply_pct %>%
          mutate(label = paste0(instnm, " — ", round(pct_airea_completions * 100, 1), "%"))
      } else {
        supply_table_df %>%
          select(instnm, pct_airea_completions) %>%
          arrange(desc(pct_airea_completions)) %>%
          slice_head(n = 150) %>%
          mutate(label = paste0(instnm, " — ", round(pct_airea_completions * 100, 1), "%"))
      }
      updateSelectInput(session, "supply_leader_pct",
                        choices = setNames(leaders_pct$instnm, leaders_pct$label))
  })
  
  # Sync institution selection from any control into a single reactiveVal
  observeEvent(input$supply_search, {
    if (!is.null(input$supply_search) && nzchar(input$supply_search)) {
      selected_inst_name(input$supply_search)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$supply_leader_airea, {
    if (!is.null(input$supply_leader_airea) && nzchar(input$supply_leader_airea)) selected_inst_name(input$supply_leader_airea)
  }, ignoreInit = TRUE)
  observeEvent(input$supply_leader_pct, {
    if (!is.null(input$supply_leader_pct) && nzchar(input$supply_leader_pct)) selected_inst_name(input$supply_leader_pct)
  }, ignoreInit = TRUE)
  
  # Selected institution from search (fallback to first in table if empty)
  selected_institution <- reactive({
    if (!is.null(selected_inst_name()) && nzchar(selected_inst_name())) {
      data.frame(instnm = as.character(selected_inst_name()), stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })
  
  # All institutions table now displays the CSV contents directly
  output$supply_table <- DT::renderDT({
    table_display <- supply_table_df %>%
      select(
        `Institution` = instnm,
        `Commuting Zone` = cz_label,
        `Mean Completions (per year)` = mean_completions,
        `Mean AIREA Completions (per year)` = mean_airea_completions,
        `AIREA%` = pct_airea_completions,
        `Mean Enrollment (per year)` = mean_students_enrolled,
        `Rural` = rural,
        `Tribal` = tribal
      ) %>%
      mutate(
        `AIREA%_num` = `AIREA%` * 100,
        `AIREA%` = paste0(round(`AIREA%_num`, 2), "%"),
        `Mean Completions (per year)` = scales::comma(round(`Mean Completions (per year)`)),
        `Mean AIREA Completions (per year)` = scales::comma(round(`Mean AIREA Completions (per year)`)),
        `Mean Enrollment (per year)` = scales::comma(round(`Mean Enrollment (per year)`)),
        `Rural` = ifelse(is.na(`Rural`) | `Rural` == 0, "No", "Yes"),
        `Tribal` = ifelse(is.na(`Tribal`) | `Tribal` == 1, "No", "Yes")
      )
    
    DT::datatable(
      table_display,
      selection = list(mode = 'single', selected = 1),
      options = list(
        pageLength = 10,
        lengthChange = FALSE,
        order = list(list(8, 'desc')),
        columnDefs = list(list(visible = FALSE, targets = c(8)))
      ),
      style = "bootstrap",
      rownames = FALSE,
      escape = FALSE
    )
  })
  
  # Download handler for supply table (export the displayed data)
  output$download_supply_table <- downloadHandler(
    filename = function() paste0("airea_institutions_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- supply_table_df %>%
        dplyr::select(
          Institution = instnm,
          `Commuting Zone` = cz_label,
          `Mean Completions (per year)` = mean_completions,
          `Mean AIREA Completions (per year)` = mean_airea_completions,
          `AIREA%` = pct_airea_completions,
          `Mean Enrollment (per year)` = mean_students_enrolled,
          Rural = rural,
          Tribal = tribal
        ) %>%
        dplyr::mutate(
          `AIREA%` = round(`AIREA%` * 100, 2)
        )
      readr::write_csv(df, file)
    }
  )
  
  # Institution time series plot (DuckDB)
  output$supply_degrees_by_institution <- renderGirafe({
    validate(need(!is.null(selected_institution()), "Select an institution above to view trends."))
    
    my_inst <- selected_institution()
    
    # Load selected institution records from DuckDB
    selected_instnm <- supply_tbl %>%
      filter(instnm == !!my_inst$instnm) %>%
      collect()
    
    if (nrow(selected_instnm) == 0) return(NULL)
    
    plot_df <- selected_instnm %>%
      group_by(year) %>%
      summarize(
        total_completions = sum(total_completions, na.rm = TRUE),
        total_students_enrolled = sum(total_students_enrolled, na.rm = TRUE),
        total_airea_completions = sum(airea_completions, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (nrow(plot_df) == 0) return(NULL)
    
    year_min <- suppressWarnings(min(plot_df$year, na.rm = TRUE))
    year_max <- suppressWarnings(max(plot_df$year, na.rm = TRUE))
    year_breaks <- if (is.finite(year_min) && is.finite(year_max) && year_min <= year_max)
      seq(year_min, year_max, 1) else NULL
    
    # Choose metric based on toggle
    metric <- input$supply_metric
    if (is.null(metric) || !(metric %in% c("airea", "pct"))) metric <- "airea"
    
    plot_df <- plot_df %>%
      mutate(
        airea_pct = ifelse(total_completions > 0,
                           (total_airea_completions / total_completions) * 100,
                           NA_real_),
        tooltip = if (metric == "pct") {
          paste0("Year: ", year, "\nAIREA %: ", round(airea_pct, 1), "%")
        } else {
          paste0("Year: ", year, "\nAIREA Credentials: ", scales::comma(total_airea_completions))
        }
      )
    
    y_col <- if (metric == "pct") "airea_pct" else "total_airea_completions"
    y_label <- if (metric == "pct") "AIREA Credentials Percentage (%)" else "Number of AIREA Credentials"
    title_txt <- if (metric == "pct") "AIREA Credentials Percentage Over Time" else "AIREA Credentials Over Time"
    title_txt <- paste(title_txt, "—", my_inst$instnm)
    
    # Join national average appropriate to metric
    nat_df <- supply_nat %>%
      dplyr::filter(year >= year_min, year <= year_max) %>%
      transmute(year,
                nat_value = if (metric == "pct") pct_airea_completions * 100 else mean_airea_completions)
    
    p <- 
      ggplot(plot_df, aes(x = year, y = .data[[y_col]])) +
      geom_line(linewidth = 1.2, color = ccrc_colors$teal) +
      geom_point_interactive(aes(tooltip = tooltip), size = 3, color = ccrc_colors$purple) +
      geom_line(data = nat_df, aes(x = year, y = nat_value),
                inherit.aes = FALSE, linewidth = 1, linetype = "dashed", color = ccrc_colors$orange) +
      ccrc_theme +
      { if (!is.null(year_breaks)) scale_x_continuous(breaks = year_breaks) else NULL } +
      labs(
        title = title_txt,
        x = NULL,
        y = y_label,
        caption = "Dashed line shows U.S. national average"
      )
    
    if (metric == "pct") {
      p <- p + scale_y_continuous(labels = function(x) paste0(x, "%"))
    } else {
      p <- p + scale_y_continuous(labels = scales::comma)
    }
    
    girafe(ggobj = p, height_svg = 6, width_svg = 10)
  })
  
  # CIP by award level stacked bar (most recent year for selected institution) using DuckDB
  output$supply_cip_award_bar <- renderGirafe({
    validate(need(!is.null(selected_institution()), "Select an institution above to view credentials awarded by CIP."))
    
    my_inst <- selected_institution()
    
    # Year selection: "Overall" aggregates across all years; else filter to chosen year
    year_choice <- input$supply_bar_year
    base_tbl <- supply_tbl %>% filter(instnm == !!my_inst$instnm)
    title_suffix <- " — Overall"
    if (!is.null(year_choice) && !identical(year_choice, "Overall")) {
      suppressWarnings({ yr_num <- as.integer(year_choice) })
      if (is.na(yr_num)) return(NULL)
      base_tbl <- base_tbl %>% filter(year == !!yr_num)
      title_suffix <- paste0(" — ", yr_num)
    }
    plot_df <- base_tbl %>%
      group_by(cip, ciptitle, award_level) %>%
      summarize(total_airea_completions = sum(airea_completions, na.rm = TRUE), .groups = "drop") %>%
      collect() %>%
      filter(total_airea_completions > 0) %>%
      mutate(
        award_level = label_to_factor(award_level),
        award_level = forcats::fct_rev(award_level),
        cip_group = get_cip_group(cip)
      ) %>%
      group_by(cip_group) %>%
      mutate(cip_group_total = sum(total_airea_completions)) %>%
      ungroup() %>%
      arrange(desc(cip_group_total), desc(total_airea_completions)) %>%
      group_by(cip_group) %>%
      arrange(desc(total_airea_completions), .by_group = TRUE) %>%
      ungroup() %>%
      mutate(
        ciptitle = factor(ciptitle, levels = rev(unique(ciptitle))),
        tooltip = paste0(
          "Field: ", cip_group,
          "\nProgram: ", ciptitle,
          "\nAward Level: ", award_level,
          "\nCredentials: ", scales::comma(total_airea_completions)
        )
      )
    if (nrow(plot_df) == 0) return(NULL)
    
    bar_style <- input$supply_bar_style
    if (is.null(bar_style)) bar_style <- "single"
    
    
    # Dynamic height based on number of programs and groups
    num_bars <- length(unique(plot_df$ciptitle))
    num_groups <- length(unique(plot_df$cip_group))
    base <- 5
    height_per_bar <- 0.35
    h <- max(8, base + height_per_bar * num_bars + 0.4 * num_groups)

    if (bar_style == "filled") {
      p <- ggplot(plot_df, aes(x = ciptitle, y = total_airea_completions, fill = award_level)) +
        geom_col_interactive(aes(tooltip = tooltip), position = "fill") +
        coord_flip() +
        ccrc_theme +
        scale_y_continuous(position = "right") +
        scale_x_discrete(position = "bottom", labels = function(x) stringr::str_wrap(x, width = 35)) +
        labs(
          title = paste("AIREA Credentials by Program and Award Level —", my_inst$instnm, title_suffix),
          y = "Share of AIREA Award Types",
          x = NULL,
          fill = "Award level"
        ) +
        scale_fill_manual(values = ccrc_palette) +
        theme(
          legend.position = "top",
          legend.justification = "left",
          legend.box.just = "left",
          legend.box = "horizontal",
          strip.placement = "outside",
          strip.background = element_blank(),
          strip.text.y.left = element_text(angle = 0, face = "bold", size = 12),
          plot.margin = margin(60, 30, 10, 140)
        ) +
        facet_grid(cip_group ~ ., scales = "free_y", space = "free_y", switch = "y")
      girafe(ggobj = p, height_svg = h, width_svg = 16)
    } else {
      p <- ggplot(plot_df, aes(x = ciptitle, y = total_airea_completions, fill = award_level)) +
        geom_col_interactive(aes(tooltip = tooltip), position = "stack") +
        coord_flip() +
        ccrc_theme +
        scale_x_discrete(position = "bottom", labels = function(x) stringr::str_wrap(x, width = 35)) +
        scale_y_continuous(labels = scales::comma, position = "right") +
        labs(
          title = paste("AIREA Credentials by CIP —", my_inst$instnm, title_suffix),
          y = "Number of AIREA Credentials",
          x = NULL,
          fill = "Award level"
        ) +
        scale_fill_manual(values = ccrc_palette) +
        theme(
          legend.position = "top",
          legend.justification = "left",
          legend.box.just = "left",
          legend.box = "horizontal",
          strip.placement = "outside",
          strip.background = element_blank(),
          strip.text.y.left = element_text(angle = 0, face = "bold", size = 12),
          plot.margin = margin(60, 30, 10, 140)
        ) +
        facet_grid(cip_group ~ ., scales = "free_y", space = "free_y", switch = "y")
      girafe(ggobj = p, height_svg = h, width_svg = 16)
    }
  })
  
  # ============================================================================
  # Panel 3: Job Postings
  # ============================================================================
  
  
  # Populate and react to CZ search (Tab 3)
  observe({
      name_map <- names(cz_table_df)
      lower_names <- tolower(name_map)
      candidates <- c("cz_label", "cz label", "commuting zone", "commuting_zone", "czlabel")
      match_idx <- match(candidates, lower_names)
      match_idx <- match_idx[!is.na(match_idx)]
      if (length(match_idx) > 0) {
        label_col <- name_map[match_idx[1]]
        updateSelectizeInput(
          session, "cz_search",
          choices = sort(unique(as.character(cz_table_df[[label_col]]))), server = TRUE
        )
        
        # Build leader lists from precomputed CSV
        prettify_cz <- function(x) gsub("^\\d+ ", "", gsub(" CZ$", "", x))
        leaders <- leaders_cz
        leaders_posts <- leaders %>%
          arrange(desc(mean_airea_posts)) %>% slice_head(n = 150) %>%
          mutate(label = paste0(prettify_cz(cz_label), " — ", scales::comma(round(mean_airea_posts))))
        updateSelectInput(session, "cz_leader_posts",
                          choices = setNames(as.character(leaders_posts$cz_label), leaders_posts$label))
        leaders_pct <- leaders %>%
          arrange(desc(mean_pct)) %>% slice_head(n = 150) %>%
          mutate(label = paste0(prettify_cz(cz_label), " — ", round(mean_pct * 100, 1), "%"))
        updateSelectInput(session, "cz_leader_pct",
                          choices = setNames(as.character(leaders_pct$cz_label), leaders_pct$label))
        leaders_per1k <- leaders %>%
          arrange(desc(mean_per1000)) %>% slice_head(n = 150) %>%
          mutate(label = paste0(prettify_cz(cz_label), " — ", scales::comma_format(accuracy = 0.1)(mean_per1000)))
        updateSelectInput(session, "cz_leader_per1000",
                          choices = setNames(as.character(leaders_per1k$cz_label), leaders_per1k$label))
      }
  })
  
  # Sync CZ selection from any control into a single reactiveVal
  observeEvent(input$cz_search, {
    if (!is.null(input$cz_search) && nzchar(input$cz_search)) selected_cz_name(input$cz_search)
  }, ignoreInit = TRUE)
  observeEvent(input$cz_leader_posts, {
    if (!is.null(input$cz_leader_posts) && nzchar(input$cz_leader_posts)) selected_cz_name(input$cz_leader_posts)
  }, ignoreInit = TRUE)
  observeEvent(input$cz_leader_pct, {
    if (!is.null(input$cz_leader_pct) && nzchar(input$cz_leader_pct)) selected_cz_name(input$cz_leader_pct)
  }, ignoreInit = TRUE)
  observeEvent(input$cz_leader_per1000, {
    if (!is.null(input$cz_leader_per1000) && nzchar(input$cz_leader_per1000)) selected_cz_name(input$cz_leader_per1000)
  }, ignoreInit = TRUE)
  
  # Selected CZ from search
  selected_cz <- reactive({
    req(selected_cz_name())
    data.frame(CZ_label = as.character(selected_cz_name()), stringsAsFactors = FALSE)
  })
  
  # All CZs table: display the CSV contents directly
  output$demand_table <- DT::renderDT({
      df <- cz_table_df
      # Identify label and percent columns if present
      lower_names <- tolower(names(df))
      label_idx <- match(c("cz_label", "cz label", "commuting zone", "commuting_zone", "czlabel"), lower_names)
      label_idx <- label_idx[!is.na(label_idx)]
      pct_idx <- match(c("airea %", "airea%", "pct_airea", "pct_airea_posts", "airea pct", "airea_percentage"), lower_names)
      pct_idx <- pct_idx[!is.na(pct_idx)]
      
      # Reorder columns to put label first if found
      if (length(label_idx) > 0) {
        first_col <- label_idx[1]
        df <- df[, c(first_col, setdiff(seq_len(ncol(df)), first_col)), drop = FALSE]
        names(df)[1] <- "Commuting Zone"
      }
      
      # Format numeric columns
      for (col in names(df)) {
        if (is.numeric(df[[col]])) {
          is_pct_col <- length(pct_idx) > 0 && match(tolower(col), lower_names[pct_idx], nomatch = 0) > 0
          if (is_pct_col) {
            vals <- df[[col]]
            vals_scaled <- if (all(vals <= 1, na.rm = TRUE)) vals * 100 else vals
            df[[col]] <- paste0(round(vals_scaled, 2), "%")
          } else if (all(df[[col]] == floor(df[[col]]), na.rm = TRUE)) {
            df[[col]] <- scales::comma(df[[col]])
          } else {
            df[[col]] <- round(df[[col]], 2)
          }
        }
      }
      
      order_list <- list()
      if (length(pct_idx) > 0) {
        pct_col_name <- names(cz_table_df)[pct_idx[1]]
        pct_display_idx <- match(pct_col_name, names(df)) - 1  # DataTables is 0-based
        if (!is.na(pct_display_idx) && pct_display_idx >= 0) {
          order_list <- list(list(pct_display_idx, 'desc'))
        }
      }
      
      # Rename common columns for readability if present
      col_map <- c(
        total_posts = "Mean Total Job Postings",
        "tot job posts" = "Mean Total Job Postings",
        airea_posts = "Mean AIREA Job Postings",
        "airea job posts" = "Mean AIREA Job Postings",
        airea_percentage = "AIREA%",
        posts_per_1000 = "Mean AIREA postings/1,000 residents",
        "posts per 1,000" = "Mean AIREA postings/1,000 residents",
        posts_per_100k = "Mean Posts per 100,000",
        population = "Commuting Zone Population"
      )
      for (nm in names(col_map)) {
        idx <- which(tolower(names(df)) == nm)
        if (length(idx) == 1) names(df)[idx] <- col_map[[nm]]
      }
      
      # If a per-1,000 column exists, format with commas and one decimal
      per1k_idx <- match(c("posts per 1,000", "posts per 1000", "posts_per_1000"), tolower(names(cz_table_df)))
      per1k_idx <- per1k_idx[!is.na(per1k_idx)]
      if (length(per1k_idx) > 0) {
        raw_name <- names(cz_table_df)[per1k_idx[1]]
        display_idx <- match(raw_name, names(df))
        if (!is.na(display_idx) && is.numeric(df[[display_idx]])) {
          df[[display_idx]] <- scales::comma_format(accuracy = 0.1)(df[[display_idx]])
        }
      }
      
      DT::datatable(
        df,
        selection = list(mode = 'single', selected = 1),
        options = list(
          pageLength = 10,
          lengthChange = FALSE,
          order = order_list
        ),
        style = "bootstrap",
        rownames = FALSE
      )
  })
  
  # Download handler for demand table (export the displayed data)
  output$download_demand_table <- downloadHandler(
    filename = function() paste0("airea_cz_job_postings_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- cz_table_df
      lower_names <- tolower(names(df))
      label_idx <- match(c("cz_label", "cz label", "commuting zone", "commuting_zone", "czlabel"), lower_names)
      label_idx <- label_idx[!is.na(label_idx)]
      if (length(label_idx) > 0) {
        first_col <- label_idx[1]
        df <- df[, c(first_col, setdiff(seq_len(ncol(df)), first_col)), drop = FALSE]
        names(df)[1] <- "Commuting Zone"
      }
      # Convert percent columns to 0-100
      pct_idx <- match(c("airea %", "airea%", "pct_airea", "pct_airea_posts", "airea pct", "airea_percentage"), lower_names)
      pct_idx <- pct_idx[!is.na(pct_idx)]
      if (length(pct_idx) > 0) {
        for (i in pct_idx) {
          nm <- names(cz_table_df)[i]
          if (nm %in% names(df) && is.numeric(df[[nm]])) {
            vals <- df[[nm]]
            df[[nm]] <- ifelse(is.na(vals), NA, ifelse(vals <= 1, vals * 100, vals))
          }
        }
      }
      readr::write_csv(df, file)
    }
  )
  
  # CZ time series plot (with metric toggle) using DuckDB
  output$demand_cz_trend <- renderGirafe({
    validate(need(!is.null(selected_cz()), "Select a commuting zone above to view trends."))
    
    my_cz <- selected_cz()
    
    # Pull time series from DuckDB, exclude 2025
    demand_selected <- demand_tbl %>%
      filter(cz_label == !!my_cz$CZ_label) %>%
      collect() %>%
      filter(year != 2025) %>%
      group_by(year) %>%
      summarise(
        posts_total = sum(total_job_postings, na.rm = TRUE),
        posts_airea = sum(total_job_postings[airea == 1], na.rm = TRUE),
        pop_year    = mean(mean_population, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        posts_per_100k = ifelse(is.na(pop_year) | pop_year == 0, NA_real_, (posts_airea / pop_year) * 100000),
        airea_pct = ifelse(posts_total > 0, (posts_airea / posts_total) * 100, NA_real_)
      )
    
    if (nrow(demand_selected) == 0) return(NULL)
    
    metric <- input$demand_metric
    if (is.null(metric) || !(metric %in% c("airea", "pct", "per100k"))) metric <- "airea"
    
    y_col <- switch(metric,
                    airea = "posts_airea",
                    pct = "airea_pct",
                    per100k = "posts_per_100k")
    y_label <- switch(metric,
                      airea = "AIREA job posts",
                      pct = "AIREA job posts (%)",
                      per100k = "AIREA job posts per 100,000")
    title_txt <- paste("AIREA Job Posts Over Time —", gsub("^[0-9]+ ", "", gsub(" CZ$", "", my_cz$CZ_label)))
    
    # Add tooltip
    demand_selected <- demand_selected %>%
      mutate(
        tooltip = switch(metric,
          airea = paste0("Year: ", year, "\nAIREA Posts: ", scales::comma(posts_airea)),
          pct = paste0("Year: ", year, "\nAIREA %: ", round(airea_pct, 1), "%"),
          per100k = paste0("Year: ", year, "\nPer 100k: ", scales::comma(round(posts_per_100k)))
        )
      )
    
    # National averages mapped to metric (unchanged logic)
    nat_min <- suppressWarnings(min(demand_selected$year, na.rm = TRUE))
    nat_max <- suppressWarnings(max(demand_selected$year, na.rm = TRUE))
    nat_df <- {
      df <- demand_nat %>% dplyr::filter(!is.na(year), year >= nat_min, year <= nat_max)
      cn <- tolower(names(df))
      first_col <- function(cands) {
        idx <- match(cands, cn)
        idx <- idx[!is.na(idx)]
        if (length(idx) == 0) return(NA_integer_)
        idx[1]
      }
      if (metric == "airea") {
        idx <- first_col(c("tot_airea_posts", "airea_posts", "total_airea_posts", "mean_airea_posts", "mean_airea_job_postings"))
        val <- if (!is.na(idx)) df[[idx]] else NA_real_
        tibble::tibble(year = df$year, nat_value = as.numeric(val))
      } else if (metric == "pct") {
        idx <- first_col(c("pct_airea_posts", "airea_pct", "pct_airea", "airea_percentage"))
        val <- if (!is.na(idx)) df[[idx]] else NA_real_
        scaled <- suppressWarnings(if (all(val <= 1, na.rm = TRUE)) val * 100 else val)
        tibble::tibble(year = df$year, nat_value = as.numeric(scaled))
      } else if (metric == "per100k") {
        idx100k <- first_col(c("posts_per_100k", "posts_per_100000"))
        idx1k   <- first_col(c("posts_per_1000"))
        if (!is.na(idx100k)) {
          val <- df[[idx100k]]
          tibble::tibble(year = df$year, nat_value = as.numeric(val))
        } else if (!is.na(idx1k)) {
          val <- df[[idx1k]] * 100
          tibble::tibble(year = df$year, nat_value = as.numeric(val))
        } else {
          tibble::tibble(year = df$year, nat_value = NA_real_)
        }
      } else {
        tibble::tibble(year = df$year, nat_value = NA_real_)
      }
    }
    
    p <- 
      ggplot(demand_selected, aes(x = year, y = .data[[y_col]])) +
      geom_line(linewidth = 1.2, color = ccrc_colors$blue) +
      geom_point_interactive(aes(tooltip = tooltip), size = 3, color = ccrc_colors$purple) +
      geom_line(data = nat_df, aes(x = year, y = nat_value),
                inherit.aes = FALSE, linewidth = 1, linetype = "dashed", color = ccrc_colors$orange) +
      ccrc_theme +
      labs(
        title = title_txt,
        x = NULL,
        y = y_label,
        caption = "Dashed line shows U.S. national average"
      )
    
    if (metric == "airea") { p <- p + scale_y_continuous(labels = scales::comma) }
    else if (metric == "pct") { p <- p + scale_y_continuous(labels = function(x) paste0(x, "%")) }
    else if (metric == "per100k") { p <- p + scale_y_continuous(labels = scales::comma) }
    
    girafe(ggobj = p, height_svg = 6, width_svg = 10)
  })
  
  # Build plot data once for reuse (also drives dynamic height)
  demand_soc_df <- reactive({
    req(selected_cz())
    my_cz <- selected_cz()
    year_choice <- input$demand_bar_year
    base_tbl <- demand_tbl %>%
      filter(cz_label == !!my_cz$CZ_label, year != 2025, airea == 1)
    if (!is.null(year_choice) && !identical(year_choice, "Overall")) {
      suppressWarnings({ selected_year <- as.integer(year_choice) })
      validate(need(!is.na(selected_year), "Invalid year selection"))
      base_tbl <- base_tbl %>% filter(year == !!selected_year)
    }
    # Use user-selected limit if provided; otherwise default to 10
    n_to_show <- if (!is.null(input$num_socs) && is.finite(input$num_socs)) as.integer(input$num_socs) else 10
    
    # Group by soc, soc_title, and ed_req to get SOC codes for grouping
    plot_data <- base_tbl %>%
      group_by(soc, soc_title, ed_req) %>%
      summarise(total_postings = sum(total_job_postings, na.rm = TRUE), .groups = "drop") %>%
      collect() %>%
      filter(!is.na(soc_title), !is.na(soc)) %>%
      mutate(
        # Add SOC group information (short names for display, full names for tooltips)
        soc_group = get_soc_group(soc),
        soc_group_full = get_soc_group_full(soc),
        ed_req = label_to_factor(ed_req),
        ed_req = forcats::fct_rev(ed_req),
        ed_req = forcats::fct_na_value_to_level(ed_req, "Missing")
      ) %>%
      # Calculate totals by occupation and by SOC group
      group_by(soc_title) %>%
      mutate(total_soc = sum(total_postings)) %>%
      group_by(soc_group) %>%
      mutate(soc_group_postings = sum(total_postings)) %>%
      ungroup() %>%
      # Select top occupations
      slice_max(order_by = total_soc, n = n_to_show, with_ties = FALSE) %>%
      # Apply SOC grouping logic
      arrange(desc(soc_group_postings), desc(total_soc)) %>%
      # Create properly ordered factors for plotting (within-group sorting)
      group_by(soc_group) %>%
      arrange(desc(total_soc), .by_group = TRUE) %>%
      mutate(
        # Create factor with levels ordered by magnitude within each group
        # Reverse the order so largest appears at top when using coord_flip()
        soc_title = factor(soc_title, levels = rev(unique(soc_title)))
      ) %>%
      ungroup() %>%
      # Create group ordering (use existing soc_group_postings)
      arrange(desc(soc_group_postings)) %>%
      mutate(
        # Create ordered factor for SOC groups (largest groups first)
        soc_group = factor(soc_group, levels = unique(soc_group))
      )
    
    return(plot_data)
  })

  output$demand_soc_edreq_bar <- renderGirafe({
    req(selected_cz())
    my_cz <- selected_cz()
    year_choice <- input$demand_bar_year
    title_suffix <- if (!is.null(year_choice) && !identical(year_choice, "Overall")) paste0(" — ", year_choice) else " — Overall"
    title_txt <- paste(
      "Top AIREA Occupations —",
      gsub("^[0-9]+ ", "", gsub(" CZ$", "", my_cz$CZ_label)),
      title_suffix
    )
    plot_df <- demand_soc_df()
    validate(need(nrow(plot_df) > 0, "No data for selection"))
    bar_style <- input$demand_bar_style
    if (is.null(bar_style)) bar_style <- "single"
    
    # Add tooltips including SOC group information
    plot_df <- plot_df %>%
      mutate(
        tooltip = paste0("SOC Group: ", soc_group_full, "\nOccupation: ", soc_title, "\nEducation: ", ed_req, "\nPostings: ", scales::comma(total_postings))
      )
    
    # Dynamic height based on number of bars plus space for group labels and title
    base <- 6  # increased base for title space
    height_per_bar <- 0.4
    num_groups <- length(unique(plot_df$soc_group))
    h <- max(11, base + height_per_bar * nrow(plot_df) + 0.5 * num_groups)
    
    if (bar_style == "filled") {
      p <- ggplot(plot_df, aes(x = soc_title, y = total_postings, fill = ed_req)) +
        geom_col_interactive(aes(tooltip = tooltip), position = "fill") +
        coord_flip() +
        scale_y_continuous(labels = scales::percent) +
        labs(
          x = NULL,
          y = "Share of Job Postings",
          fill = "Education Requirement",
          title = title_txt
        ) +
        ccrc_theme +
        scale_y_continuous(position = "right") +
        scale_x_discrete(position = "top") +
        theme(
          legend.position = "top",
          legend.justification = "left",
          legend.box.just = "left",
          legend.box = "horizontal",
          strip.placement = "outside",
          strip.background = element_blank(),
          strip.text.y.left = element_text(angle = 0, face = "bold", size = 12),
          plot.margin = margin(80, 30, 10, 160)  # give more room for title (top) and strips (left)
        ) +
        scale_fill_manual(values = ccrc_palette) +
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 35)) +
        guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
        facet_grid(soc_group ~ ., scales = "free_y", space = "free_y", switch = "y",
                   labeller = labeller(soc_group = label_wrap_gen(18)))
      girafe(ggobj = p, height_svg = h, width_svg = 16)
    } else {
      p <- ggplot(plot_df, aes(x = soc_title, y = total_postings, fill = ed_req)) +
        geom_col_interactive(aes(tooltip = tooltip), position = "stack") +
        coord_flip() +
        scale_y_continuous(labels = scales::comma) +
        labs(
          x = NULL,
          y = "Number of Job Postings",
          fill = "Education Requirement",
          title = title_txt
        ) +
        ccrc_theme +
        scale_y_continuous(position = "right") +
        scale_x_discrete(position = "top") +
        theme(
          legend.position = "top",
          legend.justification = "left",
          legend.box.just = "left",
          legend.box = "horizontal",
          strip.placement = "outside",
          strip.background = element_blank(),
          strip.text.y.left = element_text(angle = 0, face = "bold", size = 12),
          plot.margin = margin(80, 30, 10, 160)  # give more room for title (top) and strips (left)
        ) +
        scale_fill_manual(values = ccrc_palette) +
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 35)) +
        guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
        facet_grid(soc_group ~ ., scales = "free_y", space = "free_y", switch = "y",
                   labeller = labeller(soc_group = label_wrap_gen(18)))
      girafe(ggobj = p, height_svg = h, width_svg = 16)
    }
  })
}