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
df_compiled <- pivot_longer(df_compiled, cols = -Year, names_to = 'variable', values_to = 'value')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),



    # Show a plot of the generated distribution
    fluidRow(
        column(
            width = 12, # width of entire row
            selectInput(inputId = 'variable',
                        multiple = TRUE,
                        choices = unique(df_compiled$variable),
                        label = 'Variable'
                        )
        )
    ),

    fluidRow(
        column(
            width = 12,
            plotOutput("distPlot", height = '700px')
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    df_sub <- reactive({subset(df_compiled, variable %in% input$variable)})

    output$distPlot <- renderPlot({
        # create histogram of all variables
        ggplot(df_sub(), aes(x = value)) +
            geom_histogram() +
            facet_wrap(~variable, scales = 'free')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
