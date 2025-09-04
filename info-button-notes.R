################################################################################
### Load Packages
################################################################################

library(tidyverse)
library(shinyBS)



################################################################################
### Info button notes for Shiny app
### Store all popover content in a named list
################################################################################


# Helper function to create info buttons with popovers
create_info_button <- 
  function(id, label_name) {
    tagList(
      bsButton(id, 
               label = label_name, 
               icon = icon("info", lib = "font-awesome"), 
               size = "extra-small"),
      do.call(bsPopover, c(list(id = id), info_notes[[id]]))
    )
  }

info_notes <- 
  list(
    
    
    ############################################################################
    map_info = list(
      title = "More Information",
      content = HTML(paste0(
        ""
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    map_panel_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Explore the number and share of AIREA job postings by year in each commuting zone. For every community college, see what proportion of their awarded credentials are in AIREA fields."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    credentials_info = list(
      title = "More Information",
      content = HTML(paste0(
        "See a list of the community colleges that award the most credentials in AIREA fields. Select an individual college for more detail on the types of AIREA credentials by award level and trends over time."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    jobs_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Find the commuting zones with the most AIREA job postings. Select an individual commuting zone for more detail on the top AIREA occupations and trends over time."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    about_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Learn more about what jobs and credentials are included in the AIREA Data Explorer and as well as the data sources for this analysis."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    map_year_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    map_color_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    map_institution_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    supply_search_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    supply_leader_airea_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    supply_leader_pct_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    supply_metric_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    supply_bar_style_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    supply_bar_year_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    cz_search_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    cz_leader_posts_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    cz_leader_pct_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    cz_leader_per1000_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    demand_metric_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    demand_bar_style_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    num_socs_info = list(
      title = "Occupation Display Options",
      content = HTML(paste0(
        "<strong>Top 5/10/15/20/25:</strong> Shows the occupations with the highest number of job postings in the selected area.<br><br>",
        "<strong>All Available:</strong> Displays all occupations that have AIREA-related job postings (no limit).<br><br>",
        "<em>Note: The actual number shown may be less than selected if fewer occupations exist with AIREA job postings.</em>"
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    demand_bar_year_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    institutions_table_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    job_postings_table_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Notes to be added here."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    NULL
  )
    