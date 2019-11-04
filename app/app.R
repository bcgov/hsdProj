# This is a Shiny web application. You can run the application locally by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#   http://shiny.rstudio.com/
#
#   http://rstudio.github.io/shinydashboard/get_started.html
#
# To deploy an update, update code and data, then load >library(rsconnect), set working
# directory to app.R directory and >deployApp(appName = "hsdProjApp", appId =  1421868)

#####
# METADATA for app
dataVersion <- "Households 2019"
updateDate <- "September 2019"

## load libraries  ----
## installs any missing packages this script uses
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('shiny')) install.packages('shiny')
if (!require('shinydashboard')) install.packages('shinydashboard')
if (!require('rsconnect')) install.packages('rsconnect')
if (!require('DT')) install.packages('DT')
if (!require('GAlogger')) devtools::install_github("bnosac/GAlogger")

ga_set_tracking_id("UA-150850915-3")
ga_set_approval(consent = TRUE)
ga_collect_pageview(page = "/hsdProjApp")

## read data ----
data1 <- readRDS("data/data.rds")  ## by single-year intervals

# UI demonstrating column layouts
ui <- fluidPage(title = "BC Household Projections",
  theme = "bootstrap.css",
  HTML("<html lang='en'>"),
  fluidRow(
    column(width = 12, 
           style = "background-color:#003366; border-bottom:2px solid #fcba19;",
           tags$header(class="header", style="padding:0 0px 0 0px; display:flex; height:80px; width:100%;",
             tags$div(class="banner", style="display:flex; justify-content:flex-start; align-items:center; margin: 0 10px 0 10px",
               a(href="https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats",
                 img(src = "bcstats_logo_rev.png", title = "BC Stats", height = "80px", alt = "British Columbia - BC Stats"),
                 onclick="gtag"
               ),
               h1("British Columbia - Household Projections", style="font-weight:400; color:white; margin: 5px 5px 0 18px;")
             )
           )
    ),
    column(width=12,
            tags$fieldset(
                  tags$legend(h2("How to use the household projections application")),
                  p("Select a region type, and then the region(s) and year(s) of interest.
                  Use the Ctrl or Shift key to select multiple entries. Then click
                  'Generate output'. You can view the results on screen or download a CSV file.",
                  style="font-size:14px; color:#494949"),
                  br()
            )
    ),
    column(width = 12,
           sidebarLayout(
             sidebarPanel(style="background-color:#F2F2F2;",
               tags$fieldset(
                 tags$legend(h3("Data selection")),
                 uiOutput("Region.Type"),
                 uiOutput("Region.Name"),
                 uiOutput("Year")
               ),
               br(),
               tags$fieldset(
                 tags$legend(h4("Additional information")),
                 HTML(paste0("All figures are as of July 1 and are adjusted for census net undercoverage (including adjustment for incompletely enumerated Indian Reserves).", "<br><br>" , "Produced by BC Stats ", "<br>", "Data version: ", 
                             dataVersion, " <br>", "Last updated: ", updateDate))
               )
             ),
             mainPanel(
                 tags$fieldset(
                   tags$legend(h3("Actions")),
                   column(width=12,
                          actionButton(inputId = "goButton", label = "Generate output"),
                          actionButton(inputId = "resetButton", label = "Reset selection"),
                          downloadButton(outputId = "downloadData", label = "Download data as csv")
                   )
                 ),
                 br(),br(),
                 DTOutput("table"),
                 br()
             )
           )
    ),
    column(width = 12,
           style = "background-color:#003366; border-top:2px solid #fcba19;",
           
            tags$footer(class="footer",
              tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                )
              )
             )
    )
  )
)

## Define server logic ----
server <- function(input, output, session) {

  ## selections ----
  ## defaults: selectInput(inputId, label, choices, selected = NULL, multiple = FALSE,
  ##                       selectize = TRUE, width = NULL, size = NULL)
  ## size = how many items to show in box, requires selectize = FALSE

  ## select Region.Type, just one
  output$Region.Type <- renderUI({
    selectInput(inputId = "Region.Type",
                label = h4("Select a region type"),
                choices = unique(data1$Region.Type),
                selected = "Local Health Area"
                , selectize = FALSE, size = 9    ## forces all 9 options to be shown at once (not drop-down)
                )
  })

  ## select Region(s) within selected Region.Type, multiples OK
  output$Region.Name <- renderUI({
    selectInput(inputId = "Region.Name",
                label = h4("Select region(s)"),
                choices = NULL,
                multiple = TRUE,
                selectize = FALSE, size = 7)
  })

  ## update Region.Name choices based on selected Region.Type
  observeEvent(input$Region.Type,{
    
    unique_num <- unique(data1$Region[data1$Region.Type == input$Region.Type])
    unique_name <- unique(data1$Region.Name[data1$Region.Type == input$Region.Type])
    display_name <- as.list(paste0(unique_num, " - ", unique_name))
    
    choices_list <- as.list(unique_name)
    names(choices_list) <- display_name
    
    updateSelectInput(session,
                      inputId = "Region.Name",
                      choices = choices_list)
  })

  ## select Year(s), multiples OK
  output$Year <- renderUI({
    selectInput(inputId = "Year",
                label = h4("Select year(s)"),
                choices = unique(data1$Year),
                multiple = TRUE,
                selectize = FALSE, size = 7)
  })

  ## example table for custom age groups (as text to keep decimals out)
  output$example_table <- renderTable({
    matrix(data = c("15", "24",  "25", "54",  "55", "64",  "65", "74"),
           nrow = 4, ncol = 2, byrow = TRUE, dimnames = list(c(1:4), c("From", "To")))
  })


  ## reactive resetButton send analytics when reset ----
  observeEvent(input$resetButton, {
    
    ga_collect_event(event_category = "resetButton", event_label = "Reset", event_action = "Reset application")
    
    ## just reload the session
    session$reload()

  })

  ## reactive send analytics when download ----
  rv <- reactiveValues(download_flag = 0)
  
  observeEvent(rv$download_flag, {
    
    ga_collect_event(event_category = "downloadButton", event_label = paste0("Download: ", input$Region.Type), event_action = "Download data")
    
  }, ignoreInit = TRUE)
  
  ## reactive send analytics when query table ----
  observeEvent(input$goButton, {
    
    ga_collect_event(event_category = "goButton", event_label = paste0("Query: ", input$Region.Type), event_action = "Generate data")
    
  })
  
  
  ## reactive data table and download ----
  ## Create reactive values for input data to create table and download data
  data_df <- eventReactive(input$goButton, {
    ## with input$goButton in eventReactive(), nothing will happen until button clicked
    
    ## A. set df as appropriate dataset depending on age group type chosen
    df <- data1

    ## B. make selections
    Reg.Type <- c(input$Region.Type)  ## to be able to use as dynamic name in select
    df[df$Region.Type == input$Region.Type, ] %>%
      filter(Region.Name %in% input$Region.Name) %>%
      filter(Year %in% input$Year) %>%
      select(Region, !!Reg.Type := Region.Name, everything(), -Region.Type)

    ## C. call data_df() in renderDataTable to create table in app
    ## D. call data_df() in downloadHandler to download data

  })

  output$table <- DT::renderDataTable(datatable({
    
      ## call function to create specified data table
      data_df()
      
    },
    filter="none",
    ## table options: https://shiny.rstudio.com/articles/datatables.html
    options = list(
      pageLength = 10,       ## show only X rows/page; https://datatables.net/reference/option/pageLength
      lengthMenu = c(10, 20, 25, 50), ## choices of pageLength to display
      scrollX = TRUE,        ## allows horizontal scrolling; https://datatables.net/reference/option/scrollX
      dom ="ltpi"
    )
  )
  )
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      c("Household_Projections.csv")
    },

    content = function(file) {
      write.csv(data_df(), file, row.names = FALSE, na = "")  ## col.names = FALSE, append = TRUE,
      rv$download_flag <- rv$download_flag + 1
    }
  )

}

## Knit together ui and server ----
shinyApp(ui = ui, server = server)