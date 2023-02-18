library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(plotly)
library(ggplot2)
library(ggtext)

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
      selectInput(inputId = "var1", label = "select the variable", choices = c1, selected="Rape"),
      selectInput(inputId = "var2", label = "select the x variable", choices = c1, selected="Rape"),
      selectInput(inputId = "var3", label = "select the y variable", choices = c1, selected="Assult")
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
              tabPanel("Relationship among Arrest types & Urban Population", 
                       radioButtons(inputId ="fit" , label = "Select smooth method", choices = c("loess", "lm"), selected = "lm" , inline = TRUE), 
                       plotlyOutput("scatter"), value="relation"),
              side = "left"
                    ),
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
  
  ### Scatter Charts 
  output$scatter <- renderPlotly({
    p = data_source %>% 
      ggplot(aes(x=get(input$var2), y=get(input$var3))) +
      geom_point() +
      geom_smooth(method=get(input$fit)) +
      labs(title = paste("Relation between", input$var2 , "and" , input$var3),
           x = input$var2,
           y = input$var3) +
      theme(plot.title = element_textbox_simple(size=10,
                                                  halign=0.5))
    
    
    # applied ggplot to make it interactive
    ggplotly(p)
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
