library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)

dashboardPage(
  dashboardHeader(title = "Customer view"),
  
  
  dashboardSidebar(
    
    sidebarMenu( id="tabsmenu",
    
    menuItem("Customer map", icon=icon("globe"),  tabName = "customermap"),
    
    menuItem("Filters", icon=icon("filter"),
       menuItem("Select Emp Size", 
                checkboxGroupInput("empsizeCheckbox", label = "", 
                                   choices = levels(customers$EMP_SIZE), selected = levels(customers$EMP_SIZE)[3:8])
       ),
       menuItem("Select Turnover Size", 
                checkboxGroupInput("turnoversizeCheckbox", label = "", 
                                   choices = levels(customers$TURNOVER_SIZE), selected = levels(customers$TURNOVER_SIZE))
       ),
       menuItem("Select Value Size",
                checkboxGroupInput("infovalsizeCheckbox", label = "", 
                                   choices = levels(customers$INFO_VALUE_SIZE), selected = levels(customers$INFO_VALUE_SIZE)[4:6])
       ),
       menuItem("Select Type of Business",
                checkboxGroupInput("businessTypeCheckbox", label = "", 
                                   choices = unique(customers$CCH_BUSINESS_TYPE), selected = unique(customers$CCH_BUSINESS_TYPE))
       ),
       menuItem("Select Status of Customer",
                checkboxGroupInput("custStatusCheckbox", label = "", 
                                   choices = unique(customers$STATUS), selected = "LIVE")
       ),
       menuItem("Select contract status", 
                checkboxGroupInput("contractStatusCheckbox", label=NULL,
                                   choices = unique(customers$CONTRACT_STATUS), selected = c("Active","Draft") )
       ),
       menuItem("Product Filter", 
                textAreaInput("productsToMatch",label="Product Codes")
       ),
       menuItem("Job Title filter", 
                textAreaInput("jobsToMatch", label="Job Titles")
       ),
       actionButton("applyFilter", "Apply Filter")
    ),
    
    menuItem("Customer Contract Pivot", icon=icon("cube"),
      
      #menuItem("View Pivot", icon=icon("cube"),  tabName = "custcontractView"),
      
      menuItem("View Segments", tabName = "viewSegments", icon = icon("cubes")),
      
      menuItem("Set Criteria", tabName = "setCriteria", icon = icon("cubes"),
               selectInput("xaxis","Summarise By",choices = colnames(customers) ,multiple = TRUE),
               selectInput("yaxis","Transpose Column",choices = c("NONE","BUSINESS_UNIT", "BRAND","PRODUCT_FAMILY",
                                                                  "EMP_SIZE", "INFO_VALUE_SIZE")
                           ,multiple = FALSE, selected = "NONE"),
               
               conditionalPanel(condition = "input.yaxis != 'NONE'",
                                selectInput("valuevar","Value",choices = c("Customers","Products","Contracts", "TOV","APVC","AOV","MOV"), 
                                            multiple = FALSE, selected = "NONE"))
               
      ),
      downloadButton("exportPivot", "Export Pivot")
    ),
    
    menuItem("Venn", icon=icon("cc-mastercard"),
             menuSubItem("Create Venn", tabName = "venns")
             #downloadButton("exportDetails", "Export details")
    ),
    
    menuItem("Details", icon=icon("table"),
             menuSubItem("View details", icon=icon("cube"), tabName = "tableView"),
             downloadButton("exportDetails", "Export details")
    ),
    
    menuItem("Selections", icon=icon("table"),
             menuItem("Load Selection", icon=icon("cube"), 
                      selectInput("selectionsToLoad","",choices = list.files("./selections", pattern = ".RDS") ,multiple = TRUE),
                      actionButton("loadSelection","load")),
             
             menuItem("Save Selection", icon=icon("save"),
                         textInput("selectionName", label = "Selection Name"),
                         actionButton("saveSelection","Save"))
    ),
    
    tags$br(),
    tags$br(),
    
    textInput("searchTerm", label = "Find by name"),
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
                       ),
                       box(width = NULL , solidHeader = TRUE,
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
                           
                       )# end box
                       
                ) # end map column
                
              ) # end fluid row
              
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
            plotOutput("venn", width = "700px", height="700px")
          )
      )
      
    ) # end tabItems
    
  ) # dashboardBody
)