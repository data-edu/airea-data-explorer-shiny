# ==============================================================================
# Load Required Libraries and Initialize Settings
# ==============================================================================

library(shiny)                 # Shiny framework for interactive web applications
library(plotly)                # For interactive plotting
source("mapboxtoken_setup.R")  # Loads Mapbox_token used for Mapbox access



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
    tags$div(
      tags$h1(img(src = "logo-airea.png", height = "36px"),
              "Advanced Infrastructure, Energy, and Agriculture (AIREA) Data Explorer")
    ),
  
  
  
  # ============================================================================
  # Header: Navigation Bar
  # ============================================================================

  header = NULL,
  
  
    
  # ============================================================================
  # Footer
  # ============================================================================
  
  footer = 
    tags$div(
      class = "app-footer",
      tags$hr(),
      tags$p(
        tags$b("Advanced Infrastructure, Energy, and Agriculture (AIREA) Data Explorer: "),
        "The AIREA Data Explorer was created by Wei Wang, Joshua Rosenberg, ",
        "Cameron Sublett, Matias Fresard, and Bret Staudt Willet, in partnership ",
        "with the ",
        tags$a(href="https://ccrc.tc.columbia.edu/", "Community College Research Center"),
        "at Teachers College, Columbia University. Source code available on ",
        tags$a(href="https://github.com/data-edu/airea-data-explorer", "GitHub"),
        ". Funding for this project was provided by JPMorganChase and ",
        "the National Renewable Energy Lab.",
        
        tags$br(),
        tags$br(),
        tags$image(style = "height:3.0em; vertical-align:center;", src = "logo-ccrc.png", alt = "CCRC logo"),
        HTML("&emsp;"),
        tags$image(style = "height:3.0em; vertical-align:center;", src = "logo-utk.jpg", alt = "CCRC logo"),
        HTML("&emsp;"),
        tags$image(style = "height:3.0em; vertical-align:center;", src = "logo-fccc.svg", alt = "CCRC logo"),
      ),
      
      tags$hr(),
      tags$p(tags$b(paste("\u00A9", format(Sys.Date(), "%Y"))), "by CCRC")
    ),
  
  
  
  
  
  # ============================================================================
  # Panel 1: Map
  # ============================================================================
  
  tabPanel("Map", value = "mainmap",
           
           tags$div(
             style = "background-color: #f2f8f2; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
             tags$h4("These interactive maps and data visualizations are for exploring how community college green program completions align with AIREA-sector job postings by commuting zone."),
             tags$hr(),
             tags$h3("How to use the AIREA Data Explorer:"),
             tags$ul(
               style = "margin-bottom: 0;",
               tags$li(tags$strong("Map Tab"), "— Use the controls on the map to change year and color metric, or search for a specific institution. You can click and drag to reposition the input box anywhere on the map."),
               tags$li(tags$strong("Credentials Awarded Tab"), "— Click on an institution in the table to see its AIREA completion trends and top programs."),
               tags$li(tags$strong("Job Postings Tab"), "— Click on a commuting zone in the table to see its AIREA job posting trends and top occupations.")
             ),
             tags$hr()
           ),
           
           # Map container
           fluidRow(
             column(12,
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
                                    tags$div(style = "margin-top: 15px;", tags$h3("Select Year:")),
                                    choices = rev(2010:2023),
                                    selected = 2023
                        ),
                         selectInput("cz_metric", 
                                     tags$h3("Color by:"), 
                                     choices = c(
                                       "AIREA Job Postings" = "airea_job_posting",
                                       "% AIREA Postings" = "pct_green",
                                       "AIREA Jobs / 1,000 Residents" = "per1000"
                                     )
                         ),
                         selectizeInput("search_term", 
                                        tags$h3("Search by Institution:"),
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
  
  tabPanel("Credentials Awarded", value = "treemap",
           
           tags$h2("Institutions by Mean Completions (All Years)"),
           
           # Top Row: Centered search for institution
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px; text-align:center;",
                      div(
                        style = "background-color:#ffffff; border:2px solid #4CAF50; border-radius:10px; padding:12px 14px; box-shadow:0 1px 4px rgba(0,0,0,0.06); width:60%; margin:0 auto;",
                        selectizeInput(
                          inputId = "supply_search",
                          label   = tags$span("Search Institution", style = "font-size:1.25em; font-weight:700; color:#1a5276;"),
                          choices = NULL,
                          options = list(placeholder = 'Search institution...', create = FALSE),
                          width   = "100%"   # fills the 80% container
                        )
                      ),
                      div(style = "display:flex; gap:10px; justify-content:center; align-items:flex-end; margin-top:10px;",
                        selectInput(
                          inputId = "supply_leader_airea",
                          label = "Leaders: Mean AIREA Completions (per year)",
                          choices = NULL,
                          width = "30%"
                        ),
                        selectInput(
                          inputId = "supply_leader_pct",
                          label = "Leaders: AIREA % of completions",
                          choices = NULL,
                          width = "30%"
                        )
                      )
                    )
             )
           ),
           
           tags$hr(),
           
           # Second Row: Time series plot (full width)
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      tags$h4(
                        icon("hand-point-up"),
                        "Click on an institution above to see its AIREA completion trends over time")),
                    radioButtons(
                      inputId = "supply_metric",
                      label = NULL,
                      choices = c("AIREA Completions" = "airea", "AIREA Percentage" = "pct"),
                      selected = "airea",
                      inline = TRUE
                    ),
                    plotOutput("supply_degrees_by_institution", height = "400px")
             )
           ),
           
           tags$hr(),
           
           # Third Row: CIP by Award Level (Stacked Bar)
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      tags$h4(
                        icon("hand-point-up"),
                        "Click on an institution above to see its AIREA completions by CIP. Use the toggle to switch between total counts and shares by award level (most recent year)")),
                    radioButtons(
                      inputId = "supply_bar_style",
                      label = NULL,
                      choices = c(
                        "Filled" = "filled",
                        "Stacked" = "single"
                      ),
                      selected = "filled",
                      inline = TRUE
                    ),
                    plotOutput("supply_cip_award_bar", height = "500px")
             )
           )
  ),
  
  
  
  
  
  # ============================================================================
  # Panel 3: Job Postings
  # ============================================================================
  
  tabPanel("Job Postings", value = "demand",
           
           tags$h2("Commuting Zones by AIREA Job Posting Percentage (All Years)"),
           
           # Top Row: Centered search for commuting zone
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px; text-align:center;",
                      div(
                        style = "background-color:#ffffff; border:2px solid #4CAF50; border-radius:10px; padding:12px 14px; box-shadow:0 1px 4px rgba(0,0,0,0.06); width:60%; margin:0 auto;",
                        selectizeInput(
                          inputId = "cz_search",
                          label   = tags$span("Search Commuting Zone", style = "font-size:1.25em; font-weight:700; color:#1a5276;"),
                          choices = NULL,
                          options = list(placeholder = 'Search Commuting Zone...', create = FALSE),
                          width   = "100%"   # fills the 80% container
                        )
                      ),
                        div(style = "display:flex; gap:10px; justify-content:center; align-items:flex-end;",
                          selectInput(
                            inputId = "cz_leader_posts",
                            label = "Leaders: Mean AIREA Job Postings (per year)",
                            choices = NULL,
                            width = "25%"
                          ),
                          selectInput(
                            inputId = "cz_leader_pct",
                            label = "Leaders: AIREA % of all postings",
                            choices = NULL,
                            width = "25%"
                          ),
                          selectInput(
                            inputId = "cz_leader_per1000",
                            label = "Leaders: Mean AIREA postings per 1,000 residents",
                            choices = NULL,
                            width = "25%"
                          )
                        )
                    )
             )
           ),
           
           tags$hr(),
           
           # Second Row: Time series plot (full width)
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      tags$h4(
                        icon("hand-point-up"),
                        "Click on a Commuting Zone above to see its AIREA job posting percentage over time")),
                    radioButtons(
                      inputId = "demand_metric",
                      label = NULL,
                      choices = c(
                        "AIREA job posts" = "airea",
                        "AIREA % of all posts" = "pct",
                        "AIREA posts per 100,000" = "per100k"
                      ),
                      selected = "airea",
                      inline = TRUE
                    ),
                    plotOutput("demand_cz_trend", height = "400px")
             )
           ),
           
           tags$hr(),
           
           # Third Row: Occupations by education requirement (Stacked Bar)
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    tags$h4(
                      icon("hand-point-up"),
                      "Click on a Commuting Zone above to see the top occupations by AIREA job postings. Use the controls to adjust the number of occupations and switch between total bars and shares by education requirement (most recent year)")),
                    numericInput(
                      inputId = "num_socs",
                      label = "Number of occupations (1–40) to display",
                      value = 10,
                      min = 1,
                      max = 40,
                      step = 1
                    ),
                    radioButtons(
                      inputId = "demand_bar_style",
                      label = NULL,
                      choices = c(
                        "Filled" = "filled",
                        "Stacked" = "single"
                      ),
                      selected = "single",
                      inline = TRUE
                    ),
                    plotOutput("demand_soc_edreq_bar", height = "500px")
             )
           )
  ),
  
  # ============================================================================
  # Panel 4: About the Data Explorer
  # ============================================================================
  
  tabPanel("About the Data Explorer", value = "about",
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #ffffff; padding: 20px; border-radius: 6px;",
                      tags$h2("About the Data Explorer"),
                      tags$p("The AIERA Data Explorer's interactive maps and data visualizations show how community college  AIREA program completions align with nearby Advanced Infrastructure, Energy, and Agriculture (AIREA) job postings by commuting zone."),
                      tags$br(),
                      tags$h3("What are AIREA Degrees?"),
                      tags$p("To identify degree programs that prepare students for careers in Advanced Infrastructure, Energy, and Agriculture (AIREA), we began with the Greening the World of Work framework advanced by O*NET. This framework highlights occupations connected to the green economy, organized by Standard Occupational Classification (SOC) codes."),
                      tags$br(),
                      tags$p("We then used a SOC-to-CIP crosswalk to link these occupations to academic programs, classified by the Classification of Instructional Programs (CIP) codes. However, we recognized that 'green jobs' represent only part of the U.S. workforce connected to sustainability and future-ready skills. Hewing exclusively to O*NET framework meant excluding a swath of critical high-wage, high-opportunity degree programs in Construction Trades, Precision Production, and Engineering."),
                      tags$br(),
                      tags$p("To capture this broader landscape, we expanded our approach to include 6-digit CIP codes within the following 2-digit categories: (01) Agriculture and Agricultural Operations; (02) Natural Resources and Conservation; (03) Architecture; (14) Engineering; (15) Engineering Technologies and Technicians; (26) Biological Sciences; (40) Physical Sciences; (41) Science Technologies and Technicians; (46) Construction Trades; (47) Mechanic and Repair Technologies and Technicians; (48) Precision Production; and (49) Transportation and Materials Moving. We manually reviewed and flagged every degree program and their descriptions, and used multiple LLMs to help flag additional  6-digit CIP codes that did not align with the green economy or with skills needed to sustain it. We determined, for example, that Epidemiology had only an indirect connection to the AIREA framework."),
                      tags$br(),
                      tags$p("This process allowed us to balance a structured, data-driven methodology with a careful review to ensure that the degree programs included in AIREA reflect both sustainability-focused pathways and related fields with overlapping skills. This process identified 429 distinct 6-digit CIP codes (degree) programs."),
                      tags$br(),
                      tags$h3("What are AIREA jobs?"),
                      tags$p("On the jobs side, we also began with O*NET's Greening the World of Work framework, which classifies 'green' occupations across the U.S. labor market. These occupations span a wide range of industries—from renewable energy and environmental protection to construction, agriculture, and advanced manufacturing. But just as with degrees, we recognized that a narrow focus on 'green jobs' might miss critical roles where skill sets overlap significantly with sustainability-related work. For example, occupations such as solar photovoltaic installers or wind turbine technicians are classic green jobs, but roles like construction managers, electrical lineworkers, or precision agriculture technicians share many of the same technical competencies and are equally central to building the workforce of the future."),
                      tags$br(),
                      tags$p("By combining the Greening the World of Work occupations with this broader set of related roles, AIREA jobs capture both the core green economy and the adjacent occupations that are vital to supporting infrastructure, energy, and agriculture systems in a sustainable and economically inclusive way."),
                      tags$br(),
                      tags$p("Our process for identifying AIREA jobs began with first identifying the degree programs described above. We then mapped these programs to related occupations using the widely established NCES CIP-to-SOC crosswalk, which often connects multiple occupations to a single degree program. Next, we conducted a manual review of each SOC code and flagged those without a clear or direct connection to the AIREA framework. To strengthen this process, we also loaded SOC job titles, descriptions, and skills data from O*NET into several LLMs for additional review, flagging, and cross-checking. Through this multi-step process, we identified 283 distinct SOC codes that define the set of AIREA jobs."),
                      tags$br(),
                      tags$p("Users will note a high degree of variety in AIREA SOC occupations. First, AIREA jobs require different levels of education—many can be accessed with some college or sub-baccalaureate training, while others require bachelor's degrees or higher. Second, the jobs span a wide range of industries, from construction, energy, and agriculture to engineering, transportation, and advanced manufacturing. Third, they encompass both traditional 'green' occupations (such as wind turbine technicians or solar installers) and related roles (such as electrical lineworkers or construction managers) that share overlapping skills and are essential to building sustainable infrastructure."),
                      tags$br(),
                      tags$p("AIREA jobs are vital not only for addressing climate change and advancing a sustainable future, but also for expanding pathways to economic opportunity. As the graph illustrates, these jobs consistently provide family-sustaining wages. This is true for occupations that require a bachelor's degree or higher, as well as for those requiring less than a bachelor's degree. In fact, for sub-baccalaureate roles, AIREA occupations pay higher average annual wages than comparable non-AIREA occupations."),
                      tags$br(),
                      tags$p("Download the complete list of AIREA occupations and corresponding programs of study here."),
                      tags$br(),
                      tags$h3("Data Sources"),
                      tags$h4("AIREA Credentials Awarded"),
                      tags$p("For AIREA credentials awarded, we used the U.S. Department of Education's Integrated Postsecondary Education Data System (IPEDS). Specifically, we drew from IPEDS credentials awarded and institutional directory files covering the years 2010-2023. These data provide the foundation for tracking how many AIREA-related credentials community colleges have awarded over time, and where those programs are located."),
                      tags$br(),
                      tags$h4("AIREA Job Postings"),
                      tags$p("For AIREA jobs, we used job postings data from Lightcast, the nation's largest source of proprietary economic and workforce data. Lightcast provides job postings data for every BLS SOC code, with geographic detail down to the county level. This granularity allowed us to aggregate postings data to commuting zones, ensuring comparability with the geographic structure used in IPEDS. Like IPEDS, the Lightcast data include detailed geographic identifiers, which enabled us to directly map both degree completion and labor market demand into the same commuting zone framework."),
                      tags$br(),
                      tags$h3("What is a commuting zone?"),
                      tags$p("A commuting zone is a geographic unit developed by the U.S. Department of Agriculture to represent local labor markets. Commuting zones group together counties based on patterns of daily commuting, capturing the areas where people live and work. Unlike state or county boundaries, commuting zones reflect the actual flow of workers across county lines, making them especially useful for analyzing the alignment between education and labor market demand. By mapping both credentials awarded (from IPEDS) and job postings (from Lightcast) to commuting zones, the AIREA Data Explorer allows users to compare local supply and demand dynamics within consistent, labor-market–based regions.")
                    )
             )
           )
           ,
           tags$hr(),
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin: 15px 0;",
                      tags$h3("Institutions Table"),
                      tags$br(),
                      downloadButton("download_supply_table", "Download CSV", class = "btn btn-primary", style = "margin-bottom:10px;"),
                      tags$style(HTML(
                        "table.dataTable tr.active td, table.dataTable tr.active {background-color: #31a2b6  !important;}")),
                      DT::dataTableOutput("supply_table")
                    )
             )
           ),
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin: 15px 0;",
                      tags$h3("Job Postings Table (Commuting Zones)"),
                      tags$br(),
                      downloadButton("download_demand_table", "Download CSV", class = "btn btn-primary", style = "margin-bottom:10px;"),
                      tags$style(HTML(
                        "table.dataTable tr.active td, table.dataTable tr.active {background-color: #31a2b6  !important;}")),
                      DT::dataTableOutput("demand_table")
                    )
             )
           )
  )
)
