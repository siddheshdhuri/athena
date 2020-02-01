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
       #' company filters
       menuItem("Select Emp Size", 
                checkboxGroupInput("empsizeCheckbox", label = "", 
                                   choices = levels(customers$EMP_SIZE), selected = levels(customers$EMP_SIZE))
       ),
       menuItem("Select Turnover Size", 
                checkboxGroupInput("turnoversizeCheckbox", label = "", 
                                   choices = levels(customers$TURNOVER_SIZE), selected = levels(customers$TURNOVER_SIZE))
       ),
       menuItem("Select Value Size",
                checkboxGroupInput("infovalsizeCheckbox", label = "", 
                                   choices = levels(customers$INFO_VALUE_SIZE), selected = levels(customers$INFO_VALUE_SIZE))
       ),
       menuItem("Select Region", 
                checkboxGroupInput("regionCheckbox", label = "", 
                                   choices = unique(customers$Region), selected = unique(customers$Region))
       ),
       menuItem("Select Type of Business",
                checkboxGroupInput("businessTypeCheckbox", label = "", 
                                   choices = unique(customers$CCH_BUSINESS_TYPE), selected = unique(customers$CCH_BUSINESS_TYPE))
       ),
       menuItem("Select Status of Customer",
                checkboxGroupInput("custStatusCheckbox", label = "", 
                                   choices = unique(customers$STATUS), selected = "LIVE")
       ),
       #' Contract Filters 
       menuItem("Select contract status", 
                checkboxGroupInput("contractStatusCheckbox", label=NULL,
                                   choices = unique(customers$CONTRACT_STATUS), selected = c("Active","Draft") )
       ),
       #' Product Filters
       menuItem("Product Brand", 
                checkboxGroupInput("productBrandCheckbox", label=NULL,
                                   choices = unique(customers$BRAND), selected = unique(customers$BRAND) )
       ),
       menuItem("Product Filter", 
                textAreaInput("productsToMatch",label="Product Codes")
       ),
       #' Contact Filters
       menuItem("Job Title filter", 
                textAreaInput("jobsToMatch", label="Job Titles")
       ),
       actionButton("applyFilter", "Apply Filter")
    ),
    
    menuItem("Segment Analysis", icon=icon("cube"),
      #menuItem("View Pivot", icon=icon("cube"),  tabName = "custcontractView"),
      menuItem("Set Criteria", tabName = "setCriteria", icon = icon("cubes"),
               selectInput("xaxis","Summarise By",choices = colnames(customers) ,multiple = TRUE, selected=initial_seg_cols),
               selectInput("yaxis","Transpose Column",choices = c("NONE","BUSINESS_UNIT", "BRAND","CONTRACT_STATUS", "PRODUCT_FAMILY", "PRODUCT_CODE",
                                                                  "EMP_SIZE", "INFO_VALUE_SIZE", "TURNOVER_SIZE")
                           ,multiple = FALSE, selected = "NONE"),
               
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
    
    menuItem("Venn", icon=icon("cc-mastercard"),  tabName = "venns"
             #menuSubItem("Create Venn", tabName = "venns")
             #downloadButton("exportDetails", "Export details")
    ),
    
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
    
    tags$br(),
    tags$br(),
    
    textAreaInput("searchTerm", label = "Find by name"),
    actionButton("filterByName", label = "Find")
    
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
                         }" )),
    
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
                            htmlOutput("empSizeDonut")
                         ),
                         column(width=6,
                            htmlOutput("statusDonut")
                         )
                       ),
                       fluidRow(
                         column(width=12,
                            #plotly::plotlyOutput("prodBar")
                            htmlOutput("prodBar")
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
              div(style = 'overflow-x: scroll', DT::dataTableOutput("custSegPivot"))
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
      )
      
    ) # end tabItems
    
  ) # dashboardBody
)