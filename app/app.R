# Copyright 2023 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.



# This is a Shiny web application. You can run the application locally by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#   http://shiny.rstudio.com/
#   http://rstudio.github.io/shinydashboard/get_started.html
#
# To deploy an update:
#   1. update code and data, and !! likely dataVersion !!
#   2. load library(rsconnect)
#   3. set working directory to app.R directory (setwd("I:/PEOPLEPROJECTIONS/00 - R_code/shiny_apps/Production/hsdProjApp/app"))
#   4. deployApp(appName = "hsdProjApp", appId = 1421868)
# 
# https://bcstats.shinyapps.io/hsdProjApp/

## metadata for app ----
dataVersion <- "Households 2021"

## load libraries  ----
## installs any missing packages this script uses
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('shiny')) install.packages('shiny')
if (!require('shinydashboard')) install.packages('shinydashboard')
if (!require('rsconnect')) install.packages('rsconnect')
if (!require('DT')) install.packages('DT')
if (!require('GAlogger')) devtools::install_github("bnosac/GAlogger")
if (!require('markdown')) install.packages('markdown')
if (!require('janitor')) install.packages('janitor')

## Google Analytics ----
ga_set_tracking_id("UA-150850915-3")
ga_set_approval(consent = TRUE)
ga_collect_pageview(page = "/hsdProjApp")

## read data ----
data1 <- readRDS("data/data.rds")  ## by single-year intervals
data1 <- data1 %>% 
  mutate(HOUSEHOLDS = format(HOUSEHOLDS, big.mark = ","),
         PERSONS_PER_HOUSEHOLD = round_half_up(PERSONS_PER_HOUSEHOLD, digits = 3)) %>%
  rename(Year = YEAR, Type = TYPE, Region = REGION_ID, 
         Region.Type = REGION_TYPE, Region.Name = REGION_NAME,
         `Number of households` = HOUSEHOLDS, `Persons per household` = PERSONS_PER_HOUSEHOLD)

initVals <- c("Development Region", "British Columbia", max(data1$Year)) ## c(Region.Type, Region.Name, Year)
switch_wording <- "Estimates above, Projections below"  ## text in Years selection AFTER switch-year


## Define ui layout ----
# UI demonstrating column layouts
ui <- fluidPage(title = "BC Household Projections",
  theme = "bootstrap.css",
  HTML("<html lang='en'>"),
  fluidRow(
    
    bcsapps::bcsHeaderUI(id = "header", appname = "Household Estimates & Projections for British Columbia"),
    
    column(width = 12,
           style = "margin-top:100px",
           
           ## creating tabs here
           tabsetPanel(
             id = "tabs",
             
             ## Main tab ----
             tabPanel(title = "Main",
                      tags$head(tags$style(type='text/css', ".nav-tabs {font-size: 20px} ")),
                      
                      sidebarLayout(
                        sidebarPanel(style="background-color:#F2F2F2;",
                                     tags$fieldset(
                                       tags$legend(h3("Step 1: Select data")),
                                       selectInput(inputId = "Region.Type",
                                                   label = h4("Select a region type"),
                                                   choices = unique(data1$Region.Type),
                                                   selected = initVals[1],  ## default selection: "Local Health Area"
                                                   selectize = FALSE, size = 4    ## forces all 9 options to be shown at once (not drop-down)
                                       ),
                                       uiOutput("Region.Name"),
                                       uiOutput("Switch.Year"),
                                       br(),
                                       uiOutput("Year")
                                       ),
                                     br(),
                                     tags$fieldset(
                                       HTML(paste0("Produced by BC Stats ", "<br>", 
                                                   "Data available on the <a href = 'https://catalogue.data.gov.bc.ca/dataset/2a8ddf6c-dfb9-4187-a66d-9bb15b15ea83' target = '_blank'>BC Data Catalogue</a>"))
                                     )
                        ),  ## end of sidebarPanel
                        
                        mainPanel(
                          ## Actions and table ----
                          br(),
                          ### action buttons ----
                          tags$fieldset(
                            tags$legend(h3("Step 2: Action")),
                            column(width = 12,
                                   actionButton(inputId = "goButton", label = "Generate output"),
                                   actionButton(inputId = "resetButton", label = "Reset selection"),
                                   downloadButton(outputId = "downloadData", label = "Download data as csv")
                                   )
                            ),
                          br(),br(),
                          DTOutput("default_table"),  ## only shows until "Generate Output" is clicked (and again on reset)
                          DTOutput("table"),
                          br(),
                          ## end of Actions and table
                          
                          ## Notes ----
                          tags$fieldset(
                            tags$legend(h3("Notes")),
                            HTML(paste0("<ul><li>All figures are as of July 1 and are adjusted for 
                                                 census net undercoverage (including adjustment for 
                                                 incompletely enumerated Indian Reserves).</li>",
                                             # "<li>As of January 2020, Local Health Area (LHA) 
                                             #      numbering has been updated to reflect the latest 
                                             #      version of the boundaries released by the Ministry 
                                             #      of Health. Translation between old and new LHA 
                                             #      identifiers can be downloaded <b>", 
                                             #      downloadLink(outputId = "downloadTranslation", label = "here"),
                                             #     "</b>.</li>",
                                             "<li>Data obtained through this application is 
                                                  distributed under the ", "<b>
                                                  <a href='https://www2.gov.bc.ca/gov/content/data/open-data/open-government-licence-bc'>
                                                  Open Government License</a></b>.</li>",
                                        "<li>Wondering about the location of a particular region or its boundaries? Check out the <b><a href = 'https://www2.gov.bc.ca/gov/content/data/geographic-data-services/land-use/administrative-boundaries'>Administrative Boundaries</a></b> page for more information.</li>",
                                        "<li>Don't see what you need? See our Custom 
                                                        Population Products <b>
                                                        <a href='https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats/custom-products-services/custom-population-products'>page</a>
                                                        </b> for more information.</li>","</ul><br>"))
                          )  ## end of tags$fieldset (Notes)
                        )  ## end of mainPanel
                      )  ## end of sidbarLayout
             ),  ## end of tabPanel "Main"
             
             ## Methods tab ----
             tabPanel(title = "Methods",
                      column(width = 12,
                             style = "margin-top:25px",
                             tags$fieldset(
                               tags$legend(h3("Population Information")),
                               includeMarkdown("Methods.md")
                             )
                      )
             ) ## end of tabPanel "Methods"
           )  ## end of tabsetPanel
    ), ## end of column
    ## footer ----
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
    ) ## end of column "footer"
  )  ## end of fluidrow
)

## Define server logic ----
server <- function(input, output, session) {
  
  bcsapps::bcsHeaderServer(id = 'header', links = TRUE)

  ## selections ----
  ## defaults: selectInput(inputId, label, choices, selected = NULL, multiple = FALSE,
  ##                       selectize = TRUE, width = NULL, size = NULL)
  ## size = how many items to show in box, requires selectize = FALSE

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
                      choices = choices_list,
                      selected = initVals[2]  ## default selection: "British Columbia"
                      )
  })
  
  ### switch_year ----
  switch_year <- reactive({
    data1 %>%
      filter(Region.Type == input$Region.Type) %>%
      filter(Type == "Estimate") %>%
      summarize(switch_year = max(Year)) %>%
      pull(switch_year)
  })
  
  ### Proj_Years ----
  Proj_Years <- reactive({
    data1 %>%
      filter(Region.Type == input$Region.Type,
             Type == "Projection") %>%
      summarize(min = min(Year),
                max = max(Year),
                Proj_Years = paste(min, max, sep = "-")) %>%
      pull(Proj_Years)
  })
  
  ### Switch.Year text ----
  output$Switch.Year <- renderUI({
    
    HTML(paste0("Estimate and projection figures can be updated
                                                  independently at different times of the year.",
                "<br><br>",
                "<strong>Estimates:</strong> Years ", switch_year(),
                " and earlier", "<br>",
                "<strong>Projections:</strong> Years ", Proj_Years()
    ))
    
  })
  
  ### Year ----
  ## select Year(s), multiples OK
  # HTML(paste0("<strong>",switch_wording,"</strong>"))
  output$Year <- renderUI({
    
    years <- data1 %>%
      filter(Region.Type == input$Region.Type) %>%
      distinct(Year)
    
    years_fmtd <- c(min(years$Year):switch_year(), paste("--",switch_wording,"--"), (switch_year() + 1):max(years$Year))
    
    selectInput(inputId = "Year",
                label = h4("Select year(s)"),
                choices = years_fmtd, #unique(data1$Year),
                selected = initVals[[3]],
                multiple = TRUE,
                selectize = FALSE, size = 7)
  })
  
  

  ## select Year(s), multiples OK
  # output$Year <- renderUI({
  #   selectInput(inputId = "Year",
  #               label = h4("Select year(s)"),
  #               choices = unique(data1$Year),
  #               selected = initVals[3],  ## default selection: max year
  #               multiple = TRUE,
  #               selectize = FALSE, size = 7)
  # })

  ## example table for custom age groups (as text to keep decimals out)
  output$example_table <- renderTable({
    matrix(data = c("15", "24",  "25", "54",  "55", "64",  "65", "74"),
           nrow = 4, ncol = 2, byrow = TRUE, dimnames = list(c(1:4), c("From", "To")))
  })

  ## initial table with default selections ----
  
  ## initVals <- c(Region.Type, Region.Name, Year)
  data_init <- function(data1, initVals) {
    
    data1[data1$Region.Type == initVals[1], ] %>%
      filter(Region.Name == initVals[2]) %>%
      filter(Year == initVals[3]) %>%
      select(`Region ID` = Region, !!initVals[1] := Region.Name, Year, Type, `Number of households`, `Persons per household`)
  }
  
  # https://stackoverflow.com/questions/54393592/hide-plot-when-action-button-or-slider-changes-in-r-shiny
  ## initial setting to show the table
  showDefaultTable <- reactiveVal(TRUE)
  
  ## make default_table with data_init()
  output$default_table <- DT::renderDataTable(datatable({
    
    ## show table only initially (before "Generate Output" button is clicked)
    if(showDefaultTable()) {
      data_init(data1, initVals)
    } else {
      NULL
    }
  },
  filter = "none",
  ## table options: https://shiny.rstudio.com/articles/datatables.html
  options = list(
    columnDefs = list(list(className = 'dt-right', targets = c(5,6))),
    pageLength = 10,       ## show only X rows/page; https://datatables.net/reference/option/pageLength
    lengthMenu = c(10, 20, 25, 50), ## choices of pageLength to display
    scrollX = TRUE,        ## allows horizontal scrolling; https://datatables.net/reference/option/scrollX
    dom ="ltpi"
  )
  )
  )
  
  ## note: showDefaultTable changes to FALSE whenever goButton is clicked in data_df
  
  ## reactive resetButton send analytics when reset ----
  observeEvent(input$resetButton, {
    
    ga_collect_event(event_category = "resetButton", 
                     event_label = "Reset", 
                     event_action = "Reset application")
    
    ## just reload the session
    session$reload()

  })

  ## reactive send analytics when download ----
  rv <- reactiveValues(download_flag = 0)
  
  observeEvent(rv$download_flag, {
    
    ga_collect_event(event_category = "downloadButton", 
                     event_label = paste0("Download: ", input$Region.Type), 
                     event_action = "Download data")
    
  }, ignoreInit = TRUE)
  
  ## reactive send analytics when query table ----
  observeEvent(input$goButton, {
    
    ga_collect_event(event_category = "goButton", 
                     event_label = paste0("Query: ", input$Region.Type), 
                     event_action = "Generate data")
    
  })
  
  
  ## reactive data table and download ----
  ## create reactive values for input data to create table and download data
  data_df <- eventReactive(input$goButton, {
    
    ## with input$goButton in eventReactive(), nothing will happen until button clicked
    
    showDefaultTable(FALSE)  ## now hide initial default table
    
    ## A. set df as appropriate dataset depending on age group type chosen
    df <- data1

    ## B. make selections
    Reg.Type <- c(input$Region.Type)  ## to be able to use as dynamic name in select
    df %>%
      filter(Region.Type == input$Region.Type) %>%
      filter(Region.Name %in% input$Region.Name) %>%
      filter(Year %in% input$Year) %>%
      select(`Region ID` = Region, !!Reg.Type := Region.Name, Year, Type, `Number of households`, `Persons per household`)  #everything(), -Region.Type)
    
    ## C. call data_df() in renderDataTable to create table in app
    ## D. call data_df() in downloadHandler to download data

  })

  output$table <- DT::renderDataTable(datatable({
    
      ## call function to create specified data table
      data_df()
      
    },
    filter = "none",
    ## table options: https://shiny.rstudio.com/articles/datatables.html
    options = list(
      columnDefs = list(list(className = 'dt-right', targets = c(5,6))),
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
      write.csv(data_df(), file, row.names = FALSE, na = "")
      rv$download_flag <- rv$download_flag + 1
    }
  )
  
  output$downloadTranslation <- downloadHandler(
    
    filename = function() {
      c("lha_translation.csv")
    },
    
    content = function(file) {
      file.copy("data/lha_translation.csv", file)
    }
  )

}

## Knit together ui and server ----
shinyApp(ui = ui, server = server)