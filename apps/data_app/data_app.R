#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

df_compiled <- read_csv('../../data/annual_averages/annual_data_compiled.csv')
df_compiled_pivot <- pivot_longer(df_compiled, cols = -Year, names_to = 'variable', values_to = 'value')

column_names<-names(df_compiled)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Data Exploration"),

    sidebarLayout(
        sidebarPanel(
            selectInput("x_variable", "X Variable", column_names, selected = "Year"),
            selectInput("y_variable", "Y Variable", column_names, selected = "Mean_outflow_CY"),
            sliderInput("time_span",
                          "Time Period:",
                          min = min(df_compiled$Year),
                          max = max(df_compiled$Year),
                          value = c(min(df_compiled$Year),max(df_compiled$Year)))
        ),

    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("varPlot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    df_sub <- reactive({subset(df_compiled_pivot, variable %in% input$variable)})

    output$varPlot <- renderPlot({
        ggplot(df_compiled %>% filter(Year %in% c(input$time_span[1]:input$time_span[2])), aes(x = .data[[input$x_variable]],
                               y = .data[[input$y_variable]]
                               )) +
            geom_point(size = 4)+geom_smooth()+
            theme_light()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
