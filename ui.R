library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)

dashboardPage(
  dashboardHeader(title = span(tagList(icon("check-square"), "SiMPL"))),
  
  
  dashboardSidebar(
    
    sidebarMenu( id="tabsmenu",
    
    menuItem("Home", icon=icon("home"),
             menuItem("Customer Map", tabName = "customermap", icon = icon("globe")),
             selectInput("colorBy","Color By",choices = c("Region","STATUS"), selected="Region")
       ),
    
    menuItem("Filters", icon=icon("filter"),
             
             
       lapply(FILTER_CATEGORY_COLS, function(col) {
         menuItem(paste0('Select ', col),
                  checkboxGroupInput(paste0(col,"Checkbox"), label = "", 
                                     choices = unique(customers[[col]]), selected = unique(customers[[col]]))
         )
       }),
           
       lapply(FILTER_FREETEXT_COLS, function(col) {
         menuItem(paste0('Select ', col), 
                  textAreaInput(paste0(col,"ToMatch"),label='Enter Text')
         )
       }),  
       
       #' menuItem("Product Filter", 
       #'          textAreaInput("productsToMatch",label="Product Codes")
       #' ),
       #' #' Contact Filters
       #' menuItem("Job Title filter", 
       #'          textAreaInput("jobsToMatch", label="Job Titles")
       #' ),
       actionButton("applyFilter", "Apply Filter")
    ),
    
    menuItem("Segment Analysis", icon=icon("cube"),
      #menuItem("View Pivot", icon=icon("cube"),  tabName = "custcontractView"),
      menuItem("Set Criteria", tabName = "setCriteria", icon = icon("cubes"),
               selectInput("xaxis","Summarise By",choices = colnames(customers) ,multiple = TRUE, selected=INITIAL_SEGMENT_COLS),
               selectInput("yaxis","Transpose Column",choices = SEGMENT_Y_AXIS_COLS,
                           multiple = FALSE, selected = "NONE"),
               
               conditionalPanel(condition = "input.yaxis != 'NONE'",
                                selectInput("valuevar","Value",choices = c("Customers","Products","Contracts", "TOV","APVC","AOV","MOV"), 
                                            multiple = FALSE, selected = "NONE"))
               
      ),
      actionButton("drawTable", "Draw Table"),
      tags$br(),
      menuItem("View Segments", tabName = "viewSegments", icon = icon("cubes")),
      tags$br(),
      downloadButton("exportPivot", "Export Table")
    ),
    
    
    #' data visualisation charts
    menuItem("Charts", icon=icon("pie-chart"),
      menuItem("Bar Chart", tabName = "barChart", icon=icon("bar-chart")),
      menuItem("Heat Map", tabName = "heatMap", icon=icon("th-large")),
      menuItem("Sankey", tabName = "SankeyChart", icon=icon("xing"))
    ),
    
    # menuItem("Venn", icon=icon("cc-mastercard"),  tabName = "venns"
    #          #menuSubItem("Create Venn", tabName = "venns")
    #          #downloadButton("exportDetails", "Export details")
    # ),
    
    menuItem("Intersections", icon=icon("cc-mastercard"),  tabName = "upsets"),
    
    menuItem("Selections", icon=icon("database"),
             menuItem("Load Selection", icon=icon("cube"), 
                      selectInput("selectionsToLoad","",choices = list.files("./selections", pattern = ".RDS") ,multiple = TRUE),
                      actionButton("loadSelection","load")),
             
             menuItem("Save Selection", icon=icon("save"),
                         textInput("selectionName", label = "Selection Name"),
                         actionButton("saveSelection","Save"))
    ),
    
    menuItem("Details", icon=icon("table"),
             menuSubItem("View details", icon=icon("cube"), tabName = "tableView"),
             downloadButton("exportDetails", "Export details")
    ),
    
    menuItem("Talk to data", icon=icon("android"), tabName = "talk_to_data")
    
    # tags$br(),
    # tags$br(),
    # 
    # textAreaInput("searchTerm", label = "Find by name"),
    # actionButton("filterByName", label = "Find")
    
    )
    # menuItem("Search account name",
    #          textInput("searchTerm", label = ""),
    #          actionButton("filterByName", label = "Find")
    # )
    
  ),
  dashboardBody(
    
    tags$head(tags$style("html, body, #map {
                         height:100%;
                         width:100%;
                         padding:0px;
                         margin:0px;
                         }
                         " )),
    
    tabItems(
      
      tabItem("customermap",
              
              fluidRow(
                
                column(width=5,
                       fluidRow(
                         valueBoxOutput("customersValueBox"),
                         valueBoxOutput("contractsValueBox"),
                         valueBoxOutput("totalValueBox"),
                         valueBoxOutput("APVCValueBox"),
                         valueBoxOutput("emailValueBox"),
                         valueBoxOutput("phoneValueBox")
                       ),
                       fluidRow(
                         column(width=6,
                            htmlOutput("donutChart1")
                         ),
                         column(width=6,
                            htmlOutput("donutChart2")
                         )
                       ),
                       fluidRow(
                         column(width=12,
                            #plotly::plotlyOutput("prodBar")
                            htmlOutput("barChart")
                         )
                         
                       )
                       
                ), # end summary column
                
                column(width = 7, 
                       box(width = NULL , solidHeader = TRUE, height = 675,
                           leafletOutput("map", height = "650px"),
                           
                           absolutePanel(id = "tagspanel", class = "panel panel-default", fixed = TRUE,
                                         draggable = TRUE, top = 150, right = 50, left = "auto", bottom = "auto", 
                                         width = "auto", height = "auto", style = "opacity: 0.90",
                                         
                                         actionButton("getAccountSummary", "Account Details")
                                         #uiOutput("countsSummary")
                           )
                       )
                       
                ) # end map column
                
              ), # end fluid row
              fluidRow(
                
                box(title = "Selected Accounts", width = NULL , solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    
                    fluidRow(DT::dataTableOutput("selectedAccountsTable")),
                    fluidRow(
                      box(title = "Update variable", width = NULL , solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                          fluidRow(
                            column(width=2,
                                   selectInput("columnToUpdate", label="", choices = "Region", selected = "Region")
                            ),
                            column(width=2,
                                   textInput("valueToUpdate", label="")
                            )
                          ),
                          # fluidRow(
                          #   column(width=2,
                          #          textInput("newColumnToCreate", label="")
                          #   ),
                          #   column(width=2,
                          #          textInput("newValueToUpdate", label="")
                          #   )
                          # ),
                          fluidRow(
                              actionButton("updateValueButton","Update")
                          )
                        )
                      ),
                      column(width=2),
                      column(width=2,
                        textInput("selectedAccountsSelectionName", label="")     
                      ),
                      column(width=2,
                        actionButton("createSelection","Save as selection")
                      )
                      
                    ) # end box1
                    
                ),# end fluid row2
              
                fluidRow(
                  box(title = "Account Summary", width = NULL , solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                      fluidRow(
                        column(width=3,
                               shiny::uiOutput("accountSummary")
                        ),
                        column(width=5,
                               #div(style = 'overflow-x: scroll', DT::dataTableOutput("contactsAtAccount"))
                               DT::dataTableOutput("contactsAtAccount")
                        ),
                        column(width=4,
                               #div(style = 'overflow-x: scroll', DT::dataTableOutput("productsAtAccount")) 
                               DT::dataTableOutput("productsAtAccount")
                        )
                      ),
                      fluidRow(
                        column(width=2),
                        column(width=10,
                               #div(style = 'overflow-x: scroll', DT::dataTableOutput("contractsAtAccount"))
                               DT::dataTableOutput("contractsAtAccount")
                        ) # end column
                        
                      ) # end fluid row
                      
                  )# end box2
                  
                ) # end fluid row2
              
          ), # end tabItem
      
      tabItem("tableView",
        div(style = 'overflow-x: scroll', DT::dataTableOutput("selectedDataSet"))
      ),
      
      tabItem("custcontractView",
              div(style = 'overflow-x: scroll', DT::dataTableOutput("custPivot"))
      ),
      tabItem("viewSegments",
              div(style = 'overflow-x: scroll', DT::dataTableOutput("custSegPivot")),
              actionButton('go_to_talk_to_data', "Talk to data", icon = icon("android"))
      ),
      tabItem("barChart",
              #plotOutput("barChartPlot"),
              plotly::plotlyOutput("barChartPlot")
      ),
      tabItem("heatMap",
              fluidRow(
                column(width=4, 
                  selectInput("heatmapXVar", label="X Axis", choices = colnames(customers), selected = "EMP_SIZE")
                ),
                column(width=4, 
                  selectInput("heatmapYVar", label="Y Axis", choices = colnames(customers), selected = "TURNOVER_SIZE")
                ),
                column(width=4, actionButton("drawHeatMap", label="Heat"))
              ),
              fluidRow(
                plotly::plotlyOutput("heatMapPlot")
              )
      ),
      tabItem("venns",
          fluidRow(
            column(width=4,
                   selectInput("selection1","",choices = list.files("./selections", pattern = ".RDS") ,multiple = TRUE)
            ),
            column(width=4,
                   selectInput("selection2","",choices = list.files("./selections", pattern = ".RDS") ,multiple = TRUE)
            ),
            column(width=4,
                   actionButton("drawVenn","Draw Venn")
            )
          ),
          fluidRow(
            column(width=8,
                   plotOutput("venn", width = "700px", height="700px")   
            ),
            column(width=4,
                   uiOutput("downloadSetsButton")
            )
            
          )
          # fluidRow(
          #   uiOutput("downloadSetsButton")
          # )
      ),
      
      tabItem("upsets",
              fluidRow(
                column(width=4,
                       selectInput("selected_sets","Select Datasets",choices = list.files("./selections", pattern = ".RDS") ,multiple = TRUE)
                ),
                column(width=4,
                       selectInput("selected_upset_chart_type","Select Chart Type",choices =c("Venn", "Upset")  ,multiple = FALSE, selected = "Venn")
                ),
                column(width=4,
                       actionButton("draw_upset","Draw Diagram")
                )
              ),
              fluidRow(
                column(width=8,
                       upsetjs::upsetjsOutput("upset_chart")   
                )
                
              )
              # fluidRow(
              #   uiOutput("downloadSetsButton")
              # )
      ),
      
      tabItem("talk_to_data",
              shinyalert::useShinyalert(),  # Set up shinyalert
              fluidRow(
                
                tabBox(
                  width = 12, 
                  
                  tabsetPanel(id="talk_to_data_tabs",
                  
                      tabPanel("Load data from CSV", 
                               fluidRow(
                                 column(4,
                                        fileInput('inputFile', 'Choose CSV File', 
                                                  accept=c('text/csv', 
                                                           'text/comma-separated-values,text/plain', 
                                                           '.csv'))
                                 ),
                                 column(1, actionButton("load_csv_data", "Load Data"))
                               ),
                               fluidRow(
                                 box(id = "data_table_box",
                                     DT::dataTableOutput("data_table"),
                                     collapsible = TRUE
                                 )
                               )
                               
                      ),
                      tabPanel("Load data from PDF",
                               
                               fluidRow(
                                 column(4,
                                        fileInput('inputPDF', 'Choose PDF File', multiple = TRUE)
                                 ),
                                 column(1, actionButton("load_pdf_data", "Load Data"))
                               ),
                               fluidRow(
                                 box(id = "text_table_box",
                                     DT::dataTableOutput("text_table"),
                                     collapsible = TRUE
                                 )
                                 
                               )
                               
                      )      
                                          
                  )
                  
                  
                  
                ),
                
                
                box(
                  
                  radioButtons(
                    inputId = "talk_to_data_about_radio_buttons",
                    label = "About: ",
                    choices = c("data table", "text file", "in general"), inline=TRUE
                  ),
                  
                  textAreaInput("question", "Ask me anything"),  
                  
                  actionButton("go", "Go"),
                  
                  DT::dataTableOutput("chat_table"),
                  downloadButton("download_as_ppt", "Download")
                  
                )
                
                
              )
      )
      
    ) # end tabItems
    
  ) # dashboardBody
)