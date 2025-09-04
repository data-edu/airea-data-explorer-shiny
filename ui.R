# ==============================================================================
# Load Required Libraries and Initialize Settings
# ==============================================================================

library(shiny)                 # Shiny framework for interactive web applications
library(plotly)                # For interactive plotting
source("mapboxtoken_setup.R")  # Loads Mapbox_token used for Mapbox access
library(shinyBS)               # For interactive info buttons
library(ggiraph)

source("info-button-notes.R")



# ==============================================================================
# Launch the Shiny app and Navigation Bar
# ==============================================================================

navbarPage(
  id = "tabs",
  
  
  # ============================================================================
  # CSS for styling
  # ============================================================================
  
  tags$head(
    includeCSS("custom-style.css"),
    tags$style(HTML('
      /* Center contents inside our selector boxes */
      .selector-box { text-align: center; }
      .selector-box .shiny-input-container { margin-left: auto; margin-right: auto; }
      /* Center the value inside numeric and select inputs */
      .selector-box input.form-control { text-align: center; }
      .selector-box .selectize-input { text-align: center; }
      .selector-box select.form-control { text-align: center; }
      /* Keep radio choices neatly centered as a block */
      .selector-box .shiny-input-radiogroup .shiny-options-group { display: inline-block; text-align: left; }

      /* Center values in the Supply tab search selects */
      .center-selects { text-align: center; }
      .center-selects .shiny-input-container { margin-left: auto; margin-right: auto; }
      .center-selects .selectize-input { text-align: center; }
      .center-selects select.form-control { text-align: center; text-align-last: center; }
      .center-selects input.form-control { text-align: center; }
    ')),
    
    # Mapbox & JS Include Mapbox GL JS and custom scripts
    tags$link(rel="stylesheet", 
              href="https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.css"),
    tags$script(src="https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.js"),
    tags$script(src="mapbox.js"),
    tags$script(HTML(paste0("const mapboxToken = '", mapbox_token, "';"))),
    tags$script(src="https://cdn.jsdelivr.net/npm/@turf/turf@6/turf.min.js")
  ),
  
  
  
  
  
  # ============================================================================
  # Title
  # ============================================================================
  
  title = 
    div(
      div(style = "height: 40px;"),
      div(
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        h1(
          style = "margin: 0; flex-grow: 1; padding-right: 20px;",
          "Advanced Infrastructure, Energy, and Agriculture (AIREA) Data Explorer"
        ),
        img(
          src = "logo-airea.png", 
          height = "80px",
          style = "flex-shrink: 0;"
        )
      ),
      div(style = "height: 10px;")
    ),
  
  
  
  
  
  # ============================================================================
  # Header: Navigation Bar
  # ============================================================================
  
  header = 
    div(
      class = "app-header",
      p(tags$em("These interactive maps and data visualizations are for exploring how community college green program completions align with AIREA-sector job postings by commuting zone.")),
      
      hr()
    ),
  
  
  
  
  
  # ============================================================================
  # Footer
  # ============================================================================
  
  footer = 
    div(
      class = "app-footer",
      hr(),
      p(
        tags$b("Advanced Infrastructure, Energy, and Agriculture (AIREA) Data Explorer: "),
        "The AIREA Data Explorer was created by Wei Wang, Joshua Rosenberg, ",
        "Cameron Sublett, Matias Fresard, and Bret Staudt Willet, in partnership ",
        "with the ",
        tags$a(href="https://ccrc.tc.columbia.edu/", "Community College Research Center"),
        "at Teachers College, Columbia University. Source code available on ",
        tags$a(href="https://github.com/data-edu/airea-data-explorer-shiny", "GitHub"),
        ". Funding for this project was provided by JPMorganChase and ",
        "the National Renewable Energy Lab.",
        
        br(),
        br(),
        tags$image(style = "height:3.0em; vertical-align:center;", src = "logo-ccrc.png", alt = "CCRC logo"),
        HTML("&emsp;"),
        tags$image(style = "height:3.0em; vertical-align:center;", src = "logo-utk.jpg", alt = "CCRC logo"),
        HTML("&emsp;"),
        tags$image(style = "height:3.0em; vertical-align:center;", src = "logo-fccc.svg", alt = "CCRC logo")
      ),
      
      hr(),
      p(tags$b(paste("\u00A9", format(Sys.Date(), "%Y"))), "by CCRC")
    ),
  
  
  
  
  
  # ============================================================================
  # Panel 1: Map
  # ============================================================================
  
  tabPanel(title = 
             div(
               "Map",
               create_info_button("map_panel_info", "")
             ),
           value = "mainmap",
           
           
           # Map container
           fluidRow(
             column(12,
                    
                    div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      p("Which areas of the country have strong job demand in AIREA fields? Which community colleges confer a large percentage of their credentials in AIREA fields?", 
                        icon("hand-point-up"),
                        "Use the controls to filter by year or to search for an institution.",
                        br(),
                        br(),
                        "Curious to see AIREA job postings represented in different ways? ",
                        icon("hand-point-up"),
                        "Use the color by filter to see the raw number of AIREA job postings, the percent of all job postings that are in AIREA fields, or the number of AIREA job postings per 1000 residents."
                      )
                      
                    ),
                    
                    
                    div(id="map", style="height:600px; width:100%;")
             )
           ),
           
           # Interactive input container
           absolutePanel(id = "controls", 
                         class = "panel panel-default", 
                         fixed = TRUE,
                         draggable = TRUE, 
                         top = "50%",
                         left = 40,
                         width = 360, height = "auto",
                         
                         selectInput("selected_year_map",
                                     div(style = "margin-top: 15px; margin-left: -35px;", 
                                         h3("Select Year:",
                                            create_info_button("map_year_info", "")
                                         )),
                                     choices = rev(2010:2023),
                                     selected = 2023
                         ),
                         selectInput("cz_metric", 
                                     div(style = "margin-top: 15px; margin-left: -35px;",
                                         h3("Color By:",
                                            create_info_button("map_color_info", "")
                                         )), 
                                     choices = c(
                                       "Number of AIREA Job Postings" = "airea_job_posting",
                                       "% AIREA Postings" = "pct_green",
                                       "AIREA Jobs / 1,000 Residents" = "per1000"
                                     )
                         ),
                         selectizeInput("search_term",
                                        div(style = "margin-top: 15px; margin-left: -35px;",
                                            h3("Search by Institution:",
                                               create_info_button("map_institution_info", "")
                                            )),
                                        choices = NULL, 
                                        options = list(), 
                                        width="100%"
                         ),
                         
                         actionButton("search_btn", 
                                      "Search", 
                                      class = "btn-primary"),
                         tags$button("Clear", 
                                     onclick = "clearMap()",
                                     class = "btn btn-clear", 
                                     style = "margin-left: 10px;")
           )
  ),
  
  
  
  
  
  # ============================================================================
  # Panel 2: Credentials Awarded
  # ============================================================================
  
  tabPanel(title =
             div(
               "Credentials Awarded", 
               create_info_button("credentials_info", "")
             ),
           value = "treemap",
           
           
           h2("Credentials Awarded at Community Colleges in AIREA Fields"),
           
           # Top Row: Centered search for institution
           fluidRow(
             column(12,
                    div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      
                      p("Which community colleges confer the most credentials in AIREA fields? For any given college, what types of credentials are awarded and in what fields? How has this changed over time?",
                        br(),
                        br(),
                        icon("hand-point-up"),
                        "Search for a community college to see its top AIREA programs and trends over time in the visualizations below."
                      ),
                      
                      br(),
                      
                      
                      
                      div(
                        class = "center-selects",
                        style = "background-color:#ffffff; border:2px solid #5ca060; border-radius:10px; padding:12px 14px; box-shadow:0 1px 4px rgba(0,0,0,0.06); width:90%; margin:0 auto;",
                        
                        h3("Search By:", style = "text-align: center;"),
                        br(),
                        
                        div(
                          style = "display: flex; align-items: start; gap: 10px; flex-wrap: wrap;",
                          
                          div(
                            style = "flex: 1; min-width: 200px; max-width: 32%;",
                            selectizeInput(
                              inputId = "supply_search",
                              label = tags$strong("All institutions", 
                                                  create_info_button("supply_search_info", "")
                              ),
                              choices = NULL,
                              selected = NULL,
                              options = list(placeholder = 'Search institution...', create = FALSE, selectOnTab = FALSE, allowEmptyOption = TRUE, persist = FALSE)
                            )
                          ),
                          div(
                            style = "margin-top: 20px; font-weight: bold;",
                            "or"
                          ),
                          div(
                            style = "flex: 1; min-width: 200px; max-width: 32%;",
                            selectInput(
                              inputId = "supply_leader_airea",
                              label = tags$strong("Top 50 colleges by total AIREA credentials",
                                                  create_info_button("supply_leader_airea_info", "")
                              ),
                              choices = NULL
                            )
                          ),
                          div(
                            style = "margin-top: 20px; font-weight: bold;",
                            "or"
                          ),
                          div(
                            style = "flex: 1; min-width: 200px; max-width: 32%;",
                            selectInput(
                              inputId = "supply_leader_pct",
                              label = tags$strong("Top 50 colleges by AIREA % of credentials",
                                                  create_info_button("supply_leader_pct_info", "")
                              ),
                              choices = NULL
                            )
                          )
                        )
                      )
                      
                    )
             )
           ),
           
           hr(),
           
           
           
           # Second Row: Time series plot (full width)
           fluidRow(
             column(12,
                    br(),
                    girafeOutput("supply_degrees_by_institution", width = "80%"),
                    br(),
                    div(
                      class = "selector-box",
                      style = "background-color:#ffffff; border:2px solid #5ca060; border-radius:10px; padding:12px; box-shadow:0 1px rgba(0,0,0,0.06); width:50%; margin:0 auto;",
                      radioButtons(
                        inputId = "supply_metric",
                        label = h3("Select:", style = "margin: -15px;",
                                   create_info_button("supply_metric_info", "")
                        ),
                        choices = c("Number of AIREA Credentials" = "airea", "AIREA Credentials Percentage" = "pct"),
                        selected = "airea",
                        inline = FALSE,
                        width = "90%"
                      )
                    ),
                    br()
             )
           ),
           
           hr(),
           
           
           
           # Third Row: CIP by Award Level (Stacked Bar)
           fluidRow(
             column(12,
                    br(),
                    girafeOutput("supply_cip_award_bar", width = "90%"),
                    br(),
                    div(
                      class = "selector-box",
                      style = "background-color:#ffffff; border:2px solid #5ca060; border-radius:10px; padding:12px; box-shadow:0 1px rgba(0,0,0,0.06); width:50%; margin:0 auto;",
                      radioButtons(
                        inputId = "supply_bar_style",
                        label = h3("Select view:", style = "margin: -15px;",
                                   create_info_button("supply_bar_style_info", "")
                        ),
                        choices = c(
                          "Share of Award Types within Programs" = "filled",
                          "Number of Credentials by Program" = "single"
                        ),
                        selected = "single",
                        inline = FALSE,
                        width = "90%"
                      ),
                      
                      br(),
                      
                      selectInput(
                        inputId = "supply_bar_year",
                        label = h3("Select year:", style = "margin: -15px;",
                                   create_info_button("supply_bar_year_info", "")
                        ),
                        choices = c("Overall", 2010:2023),
                        selected = "Overall",
                        width = "40%"
                      )
                    ),
                    br()
             )
           )
  ),
  
  
  
  
  
  # ============================================================================
  # Panel 3: Job Postings
  # ============================================================================
  
  tabPanel(title = 
             div(
               "Job Postings",
               create_info_button("jobs_info", "")
             ),
           value = "demand", 
           
           
           h2("AIREA Job Postings by Commuting Zone"),
           
           # Top Row: Centered search for commuting zone
           fluidRow(
             column(12,
                    div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      p("What parts of the country have strong occupational demand in AIREA fields? How does the number of AIREA jobs posted compare to the total number of job postings? How has this changed over time?",
                        br(),
                        br(),
                        icon("hand-point-up"),
                        "Search for a commuting zone to see its top AIREA job postings, education requirements, and trends over time in the visualizations below."
                      ),
                      
                      br(),
                      
                      
                      
                      div(
                        class = "center-selects",
                        style = "background-color:#ffffff; border:2px solid #5ca060; border-radius:10px; padding:12px 14px; box-shadow:0 1px 4px rgba(0,0,0,0.06); width:100%; margin:0 auto;",
                        
                        h3("Search By:", style = "text-align: center;"),
                        br(),
                        
                        div(
                          style = "display: flex; align-items: start; gap: 10px; flex-wrap: wrap;",
                          
                          div(
                            style = "flex: 1; min-width: 150px; max-width: 24%;",
                            selectizeInput(
                              inputId = "cz_search",
                              label = tags$strong("All commuting zone", 
                                                  create_info_button("cz_search_info", "")
                              ),
                              choices = NULL,
                              selected = NULL,
                              options = list(placeholder = 'Search commuting zone...', create = FALSE, selectOnTab = FALSE)
                            )
                          ),
                          div(
                            style = "margin-top: 20px; font-weight: bold;",
                            "or"
                          ),
                          div(
                            style = "flex: 1; min-width: 150px; max-width: 24%;",
                            selectInput(
                              inputId = "cz_leader_posts",
                              label = tags$strong("Top 50 zones by total AIREA postings", 
                                                  create_info_button("cz_leader_posts_info", "")
                              ),
                              choices = NULL
                            )
                          ),
                          div(
                            style = "margin-top: 20px; font-weight: bold;",
                            "or"
                          ),
                          div(
                            style = "flex: 1; min-width: 150px; max-width: 24%;",
                            selectInput(
                              inputId = "cz_leader_pct",
                              label = tags$strong("Top 50 zones by AIREA % of all postings", 
                                                  create_info_button("cz_leader_pct_info", "")
                              ),
                              choices = NULL
                            )
                          ),
                          div(
                            style = "margin-top: 20px; font-weight: bold;",
                            "or"
                          ),
                          div(
                            style = "flex: 1; min-width: 150px; max-width: 24%;",
                            selectInput(
                              inputId = "cz_leader_per1000",
                              label = tags$strong("Top 50 zones by AIREA postings per 1,000 residents", 
                                                  create_info_button("cz_leader_per1000_info", "")
                              ),
                              choices = NULL
                            )
                          )
                        )
                      )
                      
                    )
             )
           ),
           
           hr(),
           
           # Second Row: Time series plot (full width)
           fluidRow(
             column(12,
                    br(),
                    girafeOutput("demand_cz_trend", width = "80%"),
                    br(),
                    div(
                      class = "selector-box",
                      style = "background-color:#ffffff; border:2px solid #5ca060; border-radius:10px; padding:12px; box-shadow:0 1px rgba(0,0,0,0.06); width:50%; margin:0 auto;",
                      
                      radioButtons(
                        inputId = "demand_metric",
                        label = h3("Select:", style = "margin: -15px;", 
                                   create_info_button("demand_metric_info", "")
                        ),
                        choices = c(
                          "Number of AIREA Job Postings" = "airea",
                          "AIREA % of All Postings" = "pct",
                          "AIREA Postings per 100,000" = "per100k"
                        ),
                        selected = "airea",
                        inline = FALSE
                      )
                    ),
                    br()
             )
           ),
           
           tags$hr(),
           
           
           # Third Row: Occupations by education requirement (Stacked Bar)
           fluidRow(
             column(12,
                    br(),
                    girafeOutput("demand_soc_edreq_bar", width = "90%"),
                    br(),
                    div(
                      class = "selector-box",
                      style = "background-color:#ffffff; border:2px solid #5ca060; border-radius:10px; padding:12px; box-shadow:0 1px rgba(0,0,0,0.06); width:50%; margin:0 auto;",
                      
                      radioButtons(
                        inputId = "demand_bar_style",
                        label = h3("Select:", style = "margin: -15px;", 
                                   create_info_button("demand_bar_style_info", "")
                        ),
                        choices = c(
                          "Share of Job Postings" = "filled",
                          "Number of Job Postings" = "single"
                        ),
                        selected = "single",
                        inline = FALSE,
                        width = "90%"
                      ),
                      
                      br(),
                      div(class = "selector-box",
                        radioButtons(
                          inputId = "num_socs",
                          label = h3("Number of Top Occupations to Display:", style = "margin: -15px;",
                                     create_info_button("num_socs_info", "")
                          ),
                          choices = c(
                            "5" = "5",
                            "10" = "10",
                            "15" = "15",
                            "20" = "20",
                            "25" = "25",
                            "All Available" = "all"
                          ),
                          selected = "15",
                          inline = TRUE
                        )
                      ),
                      
                      br(),
                      selectInput(
                        inputId = "demand_bar_year",
                        label = h3("Year:", style = "margin: -15px;", 
                                   create_info_button("demand_bar_year_info", "")
                        ),
                        choices = c("Overall", 2010:2023),
                        selected = "Overall",
                        width = "50%"
                      ),
                      br()
                    ),
                    br()
             )
           )
  ),
  
  
  
  
  
  # ============================================================================
  # Panel 4: About the Data Explorer
  # ============================================================================
  
  tabPanel(title =
             div(
               "About the Data Explorer",
               create_info_button("about_info", "")
             ),
           value = "about",
           
           
           h2("About the AIREA Data Explorer"),
           
           fluidRow(
             column(12,
                    div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      p("The AIREA Data Explorer's interactive maps and data visualizations show how community college AIREA program completions align with nearby job postings by commuting zone.")
                    ),
                    div(
                      style = "background-color: #ffffff; padding: 20px; border-radius: 6px;",
                      
                      hr(),
                      h3("Frequently Asked Questions"),
                      br(),
                      
                      
                      
                      bsCollapse(
                        id = "collapse_panel",
                        bsCollapsePanel(
                          title = h4("What are AIREA jobs?"),
                          value = "qa1",
                          p("To identify Advanced Infrastructure, Energy, and Agriculture (AIREA) occupations, we began with the ",
                            tags$a(href="https://www.onetcenter.org/reports/Green.html", 
                                   tags$em("Greening of the World of Work")),
                            " framework advanced by ",
                            tags$a(href="https://www.onetonline.org/", 
                                   "O*NET"),
                            ". This framework highlights occupations connected to the green economy, organized by the Bureau of Labor Statistics' ",
                            tags$a(href="https://www.bls.gov/soc/", 
                                   tags$em("Standard Occupational Classification")),
                            " (SOC) codes. These occupations span a wide range of industries",
                            HTML("&mdash;"),
                            "from renewable energy and environmental protection to construction, agriculture, and advanced manufacturing."),
                          
                          br(),
                          p("However, a narrow focus on green jobs misses critical roles where skill sets overlap significantly with sustainability-related work. For example, occupations such as solar photovoltaic installers or wind turbine technicians are classic green jobs, but roles like construction managers, electrical lineworkers, or precision agriculture technicians share many of the same technical competencies and are equally central to building the workforce of the future."),
                          
                          br(),
                          p("Hewing exclusively to the O*NET framework meant excluding a swath of critical high-wage, high-opportunity degree programs in construction trades, precision production, and engineering. We mapped these programs to related occupations using the ",
                            tags$a(href="https://nces.ed.gov/ipeds/cipcode/post3.aspx?y=56", 
                                   "CIP SOC Crosswalk"),
                            ", which matches each 6-digit ",
                            tags$a(href="https://nces.ed.gov/ipeds/cipcode/default.aspx?y=55", 
                                   "Classification of Instructional Programs (CIP)"),
                            " code to all associated SOC codes. Next, we conducted a manual review of each SOC code and flagged those without a clear or direct connection to the AIREA framework. To strengthen this process, we also loaded SOC job titles, descriptions, and skills data from O*NET into several LLMs for additional review, flagging, and cross-checking."),
                          
                          br(),
                          p("Through this multi-step process, we identified 283 distinct SOC codes that define the set of AIREA jobs. By combining the Greening the World of Work occupations with this broader set of related roles, AIREA jobs capture both the core green economy and the adjacent occupations that are vital to supporting infrastructure, energy, and agriculture systems in a sustainable and economically inclusive way."),
                          
                          br(),
                          p("Download the complete list of AIREA occupations and corresponding programs of study here."),
                          
                          br(),
                          p("AIREA jobs are vital not only for addressing climate change and advancing a sustainable future, but also for expanding pathways to economic opportunity. AIREA jobs consistently provide family-sustaining wages. This is true for occupations that require a bachelor's degree or higher, as well as for those requiring less than a bachelor's degree. In fact, our analysis of wage data from the Bureau of Labor Statistics' Occupation, Employment and Wage Statistics program shows that middle-skills workers in AIREA jobs earn about 30.6 percent more annually ($61,011 vs. $46,710) than equivalently skilled workers in non-AIREA roles."),
                          
                          br(),
                          p("To identify the demand for workers in AIREA fields, we used 2010-2023 job postings data from ",
                            tags$a(href="https://lightcast.io/", "Lightcast"),
                            ", the nation's largest source of proprietary economic and workforce data. Lightcast provides job postings data for every SOC code, with geographic detail down to the county level.")
                        )
                      ),
                      
                      
                      
                      br(),
                      bsCollapse(
                        id = "collapse_panel",
                        bsCollapsePanel(
                          title = h4("What are AIREA Credentials?"),
                          value = "qa2",
                          p("For AIREA credentials, we used the U.S. Department of Education's ",
                            tags$a(href="https://nces.ed.gov/ipeds", 
                                   "Integrated Postsecondary Education Data System (IPEDS)"), 
                            " degree completions and institutional directory files covering the years 2010-2023. Building from the 283 SOC codes described above and using the CIP SOC Crosswalk, we include 6-digit CIP codes within the following 2-digit categories:"),
                          tags$ul(
                            tags$li("(01) Agriculture and Agricultural Operations"),
                            tags$li("(02) Natural Resources and Conservation"),
                            tags$li("(03) Architecture"),
                            tags$li("(14) Engineering"),
                            tags$li("(15) Engineering Technologies and Technicians"),
                            tags$li("(26) Biological Sciences"),
                            tags$li("(40) Physical Sciences"),
                            tags$li("(41) Science Technologies and Technicians"),
                            tags$li("(46) Construction Trades"),
                            tags$li("(47) Mechanic and Repair Technologies and Technicians"),
                            tags$li("(48) Precision Production"),
                            tags$li("(49) Transportation and Materials Moving")
                          ),
                          p("We manually reviewed and flagged every degree program and their descriptions, and used multiple LLMs to help flag additional 6-digit CIP codes that did not align with the green economy or with skills needed to sustain it."),
                          
                          br(),
                          p("This process allowed us to balance a structured, data-driven methodology with a careful review to ensure that the degree programs included in AIREA reflect both sustainability-focused pathways and related fields with overlapping skills. The resulting dataset includes 429 distinct 6-digit CIP codes (degree) programs."),
                          
                          br(),
                          p("Download the complete list of AIREA occupations and corresponding programs of study here.")
                        )
                      ),
                      
                      
                      
                      br(),
                      bsCollapse(
                        id = "collapse_panel",
                        bsCollapsePanel(
                          title = h4("What Institutions and Award Types are Included in the AIREA Data Explorer?"),
                          value = "qa3",
                          p("The dataset includes 920 institutions ",
                            tags$a(href="https://ccrc.tc.columbia.edu/easyblog/shifting-sectors-community-colleges-undercounting.html", "classified by CCRC as community colleges"),
                            " based on their funding sources and their provision (primarily, but not exclusively) of sub-baccalaureate degrees and certificates."),
                          
                          br(),
                          p("Using IPEDS, credentials are classified into four award types:",
                            tags$ul(
                              tags$li("Bachelors degree"),
                              tags$li("Award of at least 2 but less than 4 years"),
                              tags$li("Associates degree"),
                              tags$li("Long certificates (awards of at least 1 but less than 2 years)"),
                              tags$li("Short certificates (awards of less than 1 year)")
                            )
                          )
                        )
                      ),
                      
                      
                      
                      br(),
                      bsCollapse(
                        id = "collapse_panel",
                        bsCollapsePanel(
                          title = h4("What is a Commuting Zone?"),
                          value = "qa4",
                          p("A commuting zone is a geographic unit developed by the U.S. Department of Agriculture to represent local labor markets. Commuting zones group together counties based on patterns of daily commuting, capturing the areas where people live and work. Unlike state or county boundaries, commuting zones reflect the actual flow of workers across county lines, making them especially useful for analyzing the alignment between education and labor market demand. By mapping both degree completions (from IPEDS) and job postings (from Lightcast) to commuting zones, the AIREA Data Explorer allows users to compare local supply and demand dynamics within consistent, labor-market–based regions.")
                        )
                      )
                      
                      
                      
                    )
             )
           ),
           
           
           
           hr(),
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin: 15px 0;",
                      div(
                        h3("Institutions Table", 
                           style = "margin: -10px;",
                           create_info_button("institutions_table_info", "")
                      )),
                      
                      tags$br(),
                      downloadButton("download_supply_table", "Download CSV", class = "btn btn-primary", style = "margin-bottom:10px;"),
                      tags$style(HTML(
                        "table.dataTable tr.active td, table.dataTable tr.active {background-color: #31a2b6  !important;}")),
                      DT::dataTableOutput("supply_table")
                    )
             )
           ),
           
           
           
           br(),
           hr(),
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin: 15px 0;",
                      div(
                        h3("Job Postings Table (Commuting Zones)", 
                           style = "margin: -10px;",
                           create_info_button("job_postings_table_info", "")
                    )),
                    
                    tags$br(),
                    downloadButton("download_demand_table", "Download CSV", class = "btn btn-primary", style = "margin-bottom:10px;"),
                    tags$style(HTML(
                      "table.dataTable tr.active td, table.dataTable tr.active {background-color: #31a2b6  !important;}")),
                    DT::dataTableOutput("demand_table")
                    )
             )
           ),
  
  
  br(),
  fluidRow(
    column(12,
           tags$div(
             style = "background-color: #ffffff; padding: 15px; border: 2px solid #5ca060; border-radius: 5px; margin: 15px 0; width:50%; margin-left:auto; margin-right:auto;",
             h3("Research Data Access:", style = "text-align: center;"),
             p(
               "Researchers and analysts can access underlying datasets via our OSF repository:",
               tags$a(href = "https://osf.io/x9jz6/", target = "_blank", "AIREA Data Explorer OSF Repository")
             )
           )
    )
  )
)
)