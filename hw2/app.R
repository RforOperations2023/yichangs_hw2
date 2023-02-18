library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(plotly)
library(ggplot2)
library(ggtext)
library(ggcorrplot)

# load datasets
data_source <- USArrests

states <- rownames(data_source)
data_source <- data_source %>%
  mutate(State=states)

# choices for selectInput 
c1 <- data_source %>%
  select(-"State") %>%
  names()

# Column names without state and UrbanPopulation. This will be used in the selectinput for choices in the shinydashboard
c2 = data_source %>% 
  select(-"State", -"UrbanPop") %>% 
  names()


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Exploring the US Arrests Data with Different Types of Crime",
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
                     tabPanel("About", icon=icon("address-card"), fluidRow(
                       column(width = 4, tags$br(),
                              tags$p("The statistics for arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973 are included in the base R data set. Additionally, information on the percentage of the population residing in urban areas is provided."))
                     )),
                     tabPanel(title = "Data", icon=icon("address-card"), dataTableOutput("dataT"))
              )
      ),
      
      # second tab item or landing page
      tabItem(tabName = "viz",
              tabBox(id="t2", width = 12,
                     tabPanel(title = "Distribution", value = "distro", 
                              fluidRow(tags$div(align="center", box(
                                title = "Crime summary", 
                                solidHeader = TRUE, 
                                status = "primary", 
                                width = 4, 
                                textOutput("crime_summary")))
                              ),
                              plotlyOutput("histplot"), selectInput(inputId = "var1", label = "select the state", choices = c1, selected="Murder")),
                     tabPanel("Relationship among Arrest types & Urban Population", 
                              radioButtons(inputId ="fit" , label = "Select smooth method", choices = c("loess", "lm"), selected = "lm" , inline = TRUE), 
                              plotlyOutput("scatter"), value="relation", selectInput(inputId = "var2", label = "select the x variable", choices = c1, selected="Rape"),
                              selectInput(inputId = "var3", label = "select the y variable", choices = c1, selected="Assult")),
                     tabPanel("Crime Trends by State", value="trends",
                              fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                                       tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                                       
                              ),
                              plotOutput(outputId = "bar"), selectInput(inputId = "var4" , label ="Select the Arrest type" , choices = c2)
                     ),
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
    # box plot
    p1 = data_source %>%
      plot_ly() %>%
      add_boxplot(~get(input$var1)) %>%
      layout(xaxis = list(title = paste(input$var1)))
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
  
  
  ### Bar Charts - State wise trend
  output$bar <- renderPlot({
    ggplot(data = data_source, aes_string(x = "State", y = input$var4)) + geom_bar(stat = "identity", fill="orange") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      labs(title=paste0(input$var4, "Arrests per 100,000 residents"))
  })
  
  
  # Render the box header
  output$head1 <- renderText(
    paste("5 states with high rate of", input$var4, "Arrests")
  )
  
  # Render the box header
  output$head2 <- renderText(
    paste("5 states with low rate of", input$var4, "Arrests")
  )
  
  # Render the box header
  output$head3 <- renderText(
    paste("Average arrests for different types of murder")
  )
  
  top5.table <- reactive({
    # Top 5 states with high rates
    data_source %>%
      select(State, input$var4) %>%
      arrange(desc(get(input$var4))) %>%
      head(5)
  })
  
  # Render table with 5 states with high arrests for specific crime type
  output$top5 <- renderTable({
    top5.table()
  })
  
  low5.table <- reactive({
    # Top 5 states with low rates
    data_source %>%
      select(State, input$var4) %>%
      arrange(get(input$var4)) %>%
      head(5)
  })
  
  # Render table with 5 states with high arrests for specific crime type
  output$low5 <- renderTable({
    low5.table()
  })
  
  output$crime_summary <- renderText(paste("The average arrests for Murder is ", mean(data_source$Murder),".",
                                           "The average arrests for Rape is ", mean(data_source$Rape),".",
                                           "The average arrests for Assault is ", mean(data_source$Assault)))
}



# Run the application 
shinyApp(ui = ui, server = server)