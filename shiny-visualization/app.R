# Install required packages (uncomment to install)
# install.packages("shiny")  
# install.packages("readr") 
# install.packages("dplyr")  
# install.packages("ggplot2")
# install.packages("bslib")  
# install.packages("bsicons")  
# install.packages("plotly")   
# install.packages("leaflet") 
# install.packages("htmltools") 
# install.packages("DT")      
# install.packages("stringr") 
# install.packages("tidyr")    

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(bslib)
library(bsicons)
library(plotly)
library(leaflet)
library(htmltools)
library(DT)
library(stringr)
library(tidyr)

# Custom variables for file paths, organization, and dates
setwd("ENTER YOUR WORKING DIRECTORY HERE")  
file_path <- "ENTER YOUR FILE PATH HERE"    
organization_name <- "ENTER YOUR ORGANIZATION NAME HERE"  
start_date <- "ENTER START DATE HERE"        
update_date <- "ENTER UPDATE DATE HERE"       


# Generate subtitle panel for displaying dates
generateSubtitlePanel <- function(org_name = organization_name, start = start_date, end = update_date) {
  div(
    class = "subtitle",
    HTML(paste("<h3>","(", start, " - ", end, ") ", " Data pulled on ", end, "</h3>", sep = ""))
  )
}

# Generate title panel for the UI
generateTitlePanel <- function(org_name = organization_name, start = start_date, end = update_date) {
  tagList(
    titlePanel(paste(organization_name, " (", start, " - ", end, ")", sep = "")),
    div(
      class = "subtitle",
      HTML(paste("<h3>Data pulled on", end, "</h3>"))
    )
  )
}

# UI function
ui <- page_navbar(
  theme = bs_theme(version = 5, preset = "bootstrap"),
  title = "ORCID Collaboration Dashboard",
  underline = TRUE,
  nav_panel(
    title = "Summary Dashboard",
    generateTitlePanel(),
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Article Collaborations",
        value = textOutput("collab_num_updated"),
        showcase = bsicons::bs_icon("people")
      ),
      value_box(
        title = "Collaborations Cities",
        value = textOutput("collab_cities_num_updated"),
        showcase = bsicons::bs_icon("geo-alt")
      ),
      value_box(
        title = "ORCID IDs",
        value = textOutput("orcid_id_num_updated"),
        showcase = bsicons::bs_icon("person-square")
      )),card(min_height = 200, 
            card_header("Highest number of collaborations with the following organizations",
                        class="bg-dark"),
            plotlyOutput("barPlot"))),
  nav_panel(
    title = "Collaborations map",
    generateTitlePanel(),
    card(
      min_height = 500,
      selectInput("institution_filter", "Filter Institutions:", 
                  choices = c("Show All", 
                              organization_name, 
                              paste("Excluding", organization_name)),
                  selected = "Show All"),
      fluidRow(
        column(6, textInput("search_institution", "Search for an institution:", "", width='100%')),
        column(6, textInput("search_article", "Search for a DOI:", "", width='100%'))
      ),
      leafletOutput("map")
    ),
    card(
      min_height = 350, 
      card_header("List of Institutions", class="bg-dark"),
      DTOutput("institutionTable")
    )
  ),
  nav_panel(
    title = "Individual search",
    fluidRow(
      column(10, tagList(
        titlePanel("Search for your own collaborations"),
        generateSubtitlePanel())),
      column(3, actionButton("open_modal", "Why can't I find my ORCID iD?",class = "btn btn-sm"))
    ),
    textInput("search", "Search ORCID iD:", "", width='100%'),
    card(min_height = 450, h4("Collaborating Institutions"),
    DTOutput("collabTable")),
    card(min_height = 450, h4("Article Collaborations"),
    DTOutput("articleTable"))
  ),
  nav_spacer()
)

# Server function
server <- function(input, output, session) {
  # Create a reactive file reader to read the CSV file
  reader <- reactiveFileReader(intervalMillis = 1000, session, filePath = file_path, readFunc = read.csv)
  
  # Reactive data frame to read and debug
  data <- reactive({
    df <- reader()
    print("Data read. Number of rows:")
    print(nrow(df))
    print("Column names:")
    print(names(df))
    df
  })
  
  # Count unique collaborations by DOI
  collab_num <- reactive({
    df <- data()
    n <- n_distinct(df$doi) 
  })
  
  # Render the number of collaborations
  output$collab_num_updated <- renderText({
    collab_num()
  })
  
  # Count unique cities of collaborations
  collab_cities_num <- reactive({ 
    data <- reader()
    n_distinct(data$city2) 
  })
  
  output$collab_cities_num_updated <- renderText({
    collab_cities_num()
  })
  
  # Count unique ORCID IDs
  orcid_id_num <- reactive({ 
    data <- reader()
    n_distinct(data$orcid1)  
  })
  
  output$orcid_id_num_updated <- renderText({
    orcid_id_num()
  })
  
  ###### Top Institutions Bar Plot ############################################
  top_collabs_orgs <- reactive({
    req(data())
    data() %>%
      mutate(org2 = str_trim(org2)) %>%
      filter(!str_detect(org2, organization_name)) %>%
      filter(!is.na(org2) & org2 != "" & !str_detect(org2, "^\\s*$")) %>%
      group_by(org2) %>%
      summarize(org2_occurrence = n(), .groups = 'drop') %>%
      arrange(desc(org2_occurrence)) %>%
      slice_head(n = 5)
  })
  
  output$barPlot <- renderPlotly({
    plot_data <- top_collabs_orgs()
    p <- ggplot(plot_data, aes(x = reorder(org2, org2_occurrence), y = org2_occurrence, text = paste("Number of collaborations:", org2_occurrence))) +
      geom_bar(stat = "identity", fill = "#d45359") +
      xlab("") +
      theme(
        # Set plot panel background to be transparent
        panel.background = element_rect(fill = "transparent", color = NA),
        # Remove gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Set axis lines to be white and remove ticks
        axis.line = element_line(color = "white"),
        axis.ticks = element_blank(),
        # make text bigger
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 16)
      ) +
      ylab("Number of article collaborations") +
      coord_flip()

    # Convert ggplot object to plotly
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(title = "",
                     tickmode = "linear",
                     tick0 = 0,
                     dtick = 1),
        margin = list(l = 120)  # Adjust left margin for longer y-axis labels
      )
  })
  
  ###### END Top Institutions Bar Plot ###########################################
  
  ################## MAP Data and Functions ####################################
  # Reactive function for processing (and filtering) institution data
  process_institution_data <- reactive({
    req(data())
    
    # Get unique locations with their associated organizations
    unique_institutions <- data() %>%
      select(org2, city2, region2, country2, lat2, lng2, title) %>%
      filter(!is.na(lat2) & !is.na(lng2)) %>%
      group_by(lat2, lng2) %>%
      summarise(
        org2 = paste(unique(org2), collapse = "; "),  # Combine all orgs at same location
        city2 = first(city2),
        region2 = first(region2),
        country2 = first(country2),
        .groups = 'drop'
      )
    
    # Count institutions and create list of DOIs by location
    institutions_count_and_dois <- data() %>%
      filter(!is.na(lat2) & !is.na(lng2)) %>%
      group_by(lat2, lng2) %>%
      summarise(
        `org2 occurrence` = n(),
        dois = list(unique(doi)),
        titles = list(unique(title)),
        .groups = 'drop'
      )
    
    # Join data using latitude and longitude
    filtered_institutions_info <- left_join(
      unique_institutions, 
      institutions_count_and_dois,
      by = c("lat2", "lng2")) %>% 
      arrange(org2)
    
    # Add content for popups
    filtered_institutions_info <- filtered_institutions_info %>%
      mutate(cntnt = paste0('<strong>Institution:</strong> ', org2,
                            '<br><strong>Number of article collaborations: </strong>', `org2 occurrence`,
                            '<br><strong>Location:</strong> ', city2, ', ', region2,
                            '<br><strong>Country:</strong> ', country2))
    
    # Apply filters based on user input
    if (!is.null(input$institution_filter)) {
      if (input$institution_filter == organization_name) {
        filtered_institutions_info <- filtered_institutions_info %>%
          filter(str_detect(org2, regex(organization_name, ignore_case = TRUE)))
      } else if (input$institution_filter == paste("Excluding", organization_name)) {
        filtered_institutions_info <- filtered_institutions_info %>%
          filter(!str_detect(org2, regex(organization_name, ignore_case = TRUE)))
      }
    }
    
    # Apply institution search filter
    if (!is.null(input$search_institution) && input$search_institution != "") {
      filtered_institutions_info <- filtered_institutions_info %>%
        filter(str_detect(org2, regex(input$search_institution, ignore_case = TRUE)))
    }
    
    # Apply article (DOI) search filter
    if (!is.null(input$search_article) && input$search_article != "") {
      filtered_institutions_info <- filtered_institutions_info %>%
        filter(sapply(dois, function(x) any(str_detect(x, regex(input$search_article, ignore_case = TRUE)))))
    }

    filtered_institutions_info
  })
  
  output$map <- renderLeaflet({

      # Filter data by institution if a selection is made
      filtered_data <- process_institution_data()

      # Render map
      leaflet(data=filtered_data) %>%
        addTiles() %>%
        addCircles(lng = ~lng2, lat = ~lat2) %>%
        addCircleMarkers(data=filtered_data,lng = ~lng2, lat = ~lat2,
                         radius = ~sqrt(filtered_data$`org2 occurrence`) * 4,
                         label = lapply(as.character(filtered_data$cntnt), HTML),
                         stroke = FALSE, fillOpacity = 0.8,
                         color = "#d45359",
                         fillColor = "#d45359",
                         labelOptions = labelOptions(
                           direction = "auto",  # Automatically adjust the direction of labels
                           style = list(
                             "font-size" = "14px",  # Adjust font size as desired
                             pane = "markerPane",  # Specify the pane where labels should be attached
                             zIndexOffset = 400    # Increase zIndexOffset to ensure labels appear above other map layers
                           )
                         )) %>%
        setView(lng = 0, lat = 0, zoom = 2)
    })

    output$institutionTable <- renderDataTable({
      # Apply the filtering logic based on the user's selection
      filtered_data <- process_institution_data() %>%
        select(org2, `org2 occurrence`) %>%
        rename(Institution = org2, 
               Collaborations = `org2 occurrence`)

      # Render the filtered data as a DataTable
      DT::datatable(
        filtered_data,
        options = list(
          dom = 'Bt',                  # Show only buttons and table (no search, no pagination)
          searching = FALSE,           # Disable the search bar
          scrollY = "400px",           # Enable vertical scrolling with a fixed height
          scrollCollapse = TRUE,       # Collapse the table when it's smaller than the defined height
          paging = FALSE               # Disable pagination
        ),
        rownames = FALSE               # Hide the row index
      )
    })
    
    ######################### SEARCH FOR OWN COLLABS ###########################
      # Reactive expression for institution-level data
      collabs_data <- reactive({
        req(data())
        search_term <- input$search
        data() %>%
          filter(!is.na(org2) & org2 != "" & !str_detect(org2, "^\\s*$")) %>%
          filter(grepl(search_term, orcid1, ignore.case = TRUE)) %>%
          filter(!is.na(lat2) & !is.na(lng2)) %>%  # Filter out invalid coordinates
          group_by(lat2, lng2) %>%
          summarise(
            Institution = paste(sort(unique(org2)), collapse = "; "),
            Collaborations = n(),
            .groups = 'drop'
          ) %>%
          filter(!is.na(Institution)) %>%
          arrange(Institution) %>%
          select(Institution, Collaborations)
      })

      output$collabTable <- renderDataTable({
        datatable(
          collabs_data(),
          options = list(
            dom = 'Bt',
            searching = FALSE,
            scrollY = "300px",
            scrollCollapse = TRUE,
            paging = FALSE
          ),
          rownames = FALSE
        )
      })

      # Reactive expression for article-level data
      article_data <- reactive({
        req(data())
        search_term <- input$search

        data() %>%
          filter(!is.na(org2) & org2 != "" & !str_detect(org2, "^\\s*$")) %>%
          filter(grepl(search_term, orcid1, ignore.case = TRUE)) %>%
          select(doi, title) %>%
          distinct() %>%
          filter(!is.na(doi) & !is.na(title)) %>%
          rename(DOI = doi, 
                 Title = title)
      })


      # Render article-level table
      output$articleTable <- renderDataTable({
        datatable(
          article_data(),
          options = list(
            dom = 'Bt',
            searching = FALSE,
            scrollY = "300px",
            scrollCollapse = TRUE,
            paging = FALSE
          ),
          rownames = FALSE
        )
      })
    ######################### END SEARCH FOR OWN COLLABS ###########################

    ######### Why can't I find my ORCID ID? - Pop-Up ###############################
      observeEvent(input$open_modal, {
        showModal(
          modalDialog(
            title = "Why can't I find my ORCID ID?",
            tagList(
              p("If you're having trouble finding your ORCID iD in the search, here are a few things you may want to check:"),
              
              h4("1. Do you have an ORCID profile set up?"),
              p("If you have not yet created an ORCID iD, please visit ",
                a("www.orcid.org", href = "https://www.orcid.org", target = "_blank"), 
                " to set up your ORCID profile. ORCID sets up a persistent digital identifier (also called an ORCID iD) to distinguish you from other researchers."
              ),
              
              h4("2. Did you set up your ORCID iD after the data pull?"),
              p("If you set up your ORCID iD after the data supporting this dashboard were pulled, then your ORCID iD will not show up in the dashboard until the data has been pulled again."
              ),
              
              h4("3. Is all of the information in your ORCID profile accurate?"),
              p("Take a moment to verify that your current institution and location are accurately listed in your ORCID profile -- typos happen! If you work remotely for an institution, you will have to list the institution's primary location in order to show up in the data. If you correct any information in your ORCID profile after the data supporting this dashboard were pulled, then your ORCID iD will not show up in the dashboard until the data has been pulled again."
              ),
              
              h4("4. Have you made any article collaborations during the data pull period?"),
              p(paste("The script behind this dashboard only includes article collaborations that existed from ", 
                      start_date, " to ", update_date,
                      ". If you did not make any article collaborations in that time frame, your ORCID iD will not appear in the list of collaborations.", sep="")),
              
              h4("5. Still not sure?"),
              p("Reach out to your campus ORCID administrator [contact information] or [organization] for further troubleshooting.")
            ),
            
            # Footer with Close button
            footer = modalButton("Close"),
            
            # Optionally, you can specify the size of the modal
            size = "l",  # Large size
            easyClose = TRUE  # Allow closing by clicking outside the modal
          )
        )
      })
    ######### END Why can't I find my ORCID ID? - Pop-Up ############################
}

# Run the application
shinyApp(ui, server)