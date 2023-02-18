library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(plotly)
library(ggplot2)

# load datasets
data_source <- USArrests

# assign row names
states <- rownames(data_source)

data_source <- data_source %>%
  mutate(State=states)
  

# choices for selectInput 
c1 <- data_source %>%
  select(-State) %>%
  names()



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
      menuItem(text = "Visualization", tabName = "viz", icon=icon("chart-line")),
      selectInput(inputId = "var1", label = "select the variable", choices = c1, selected="Rape")
    )
  ),
  dashboardBody(
    tabItems(
      #first tab item 
      tabItem(tabName = "data",
              #tab box
              tabBox(id="t1", width=12,
                     tabPanel("About", icon=icon("address-card"), fluidRow(
                       column(width = 4, tags$br(),
                              tags$p("Introduction of this dataset"))
                     )),
                     tabPanel(title = "Data", icon=icon("address-card"), dataTableOutput("dataT"))
                     )
              ),
      
      # second tab item or landing page
      tabItem(tabName = "viz",
              tabBox(id="t2", width = 12,
              tabPanel(title = "graph1", value = "trends", h4("tabPanel-1 placeholder UI")),
              tabPanel(title = "graph2", value = "distro", plotlyOutput("histplot")),
              tabPanel(title = "graph3", value = "trends", h4("tabPanel-3 placeholder UI"))
                     )
              )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$dataT <- renderDataTable(
    data_source
  )
  
  # stacked histogram and boxplot
  output$histplot <- renderPlotly({
    p1 = data_source %>%
      plot_ly() %>%
      add_histogram(~get(input$var1)) %>%
      layout(xaxis = list(title = input$var1))
    
    # box plot
    p2 = data_source %>%
      plot_ly() %>%
      add_boxplot(~get(input$var1)) %>%
      layout(yaxis = list(showticklables = F))
    
    # stacking plots
    subplot(p2, p1, nrows = 2) %>%
      hide_legend() %>%
      layout(title = "Distribution chart - Histogram and Boxplot",
             yaxis = list(title="Frequency"))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
