library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Exploring the world population from Year 1995 to Year 2013",
                  titleWidth = 650
                  ),
  dashboardSidebar(
    # sidebarmenu
    sidebarMenu(
      id = "sidebar",
      
      #first menuitem
      menuItem("Dataset", tabName = "data", icon=icon("chart-line")),
      menuItem(text = "Visualization", tabName = "viz", icon=icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      #first tab item 
      tabItem(tabName = "data",
              #tab box
              tabBox(id="t1", width=12,
                     tabPanel("About", icon=icon("address-card"), h4("tabpanel 1 ")),
                     tabPanel(title = "Data", icon=icon("address-card"), h2("tabpanel 2 "))
                     )
              ),
      
      # second tab item or landing page
      tabItem(tabName = "viz",
              tabBox(id="t2", width = 12,
              tabPanel(title = "graph1", value = "trends", h4("tabPanel-1 placeholder UI")),
              tabPanel(title = "graph2", value = "trends", h4("tabPanel-2 placeholder UI")),
              tabPanel(title = "graph3", value = "trends", h4("tabPanel-3 placeholder UI"))
                     )
              )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
