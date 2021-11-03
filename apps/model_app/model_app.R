#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(lavaan)
library(semPlot)


combined=read.csv("../../data/annual_averages/annual_data_compiled.csv")
cnames=read.csv("../../analysis/column_names.csv", stringsAsFactors = F)
dsub=filter(combined, Year>=1975)
focaldata=dsub[,cnames$Datacolumn]
fvars=cnames$Shortname
colnames(focaldata)=fvars

#log transform
logvars=fvars[c(cnames$Log=="yes")]


logtrans=function(x) {
    x2=x[which(!is.na(x))]
    if(any(x2==0)) {log(x+min(x2[which(x2>0)],na.rm=T))}
    else {log(x)}
}
focaldatalog = focaldata %>%
    mutate_at(logvars,logtrans)

#scale data
fd=focaldatalog
fd=fd %>%
    mutate_at(2:length(fvars),list("1"=lag)) %>% #lag 1
    mutate_at(2:length(fvars),list("fd"=function(x) c(NA,diff(x)))) %>% #first difference
    mutate_at(2:length(fvars),list("dtr"=function(x) { #detrend
        x2=x
        x2[x2==0]=NA
        res=residuals(lm(x2~fd$year))
        out=x
        out[which(!is.na(x2))]=res
        return(out)
    })) %>%
    mutate_at(-1,scale)

column_names<-names(fd)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Structural Equation Model"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("response_variable_1",
                        "Response variable 1:",
                        choices=column_names,
                        selected="estfish"
                        ),
            selectInput("response_variable_2",
                        "Response variable 2:",
                        choices=column_names,
                        selected="year"),
            selectInput("predictor_variables_1",
                        "Predictor variables 1:",
                        choices=column_names,
                        selected=NULL,
                        multiple=TRUE),
            selectInput("predictor_variables_2",
                        "Predictor variables 2:",
                        choices=column_names,
                        selected=NULL,
                        multiple=TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({

        #generate lavaan formula
        formula1 <- paste0(c(input$response_variable_1, paste0(input$predictor_variables_1,collapse = "+")), collapse="~")
        formula2 <- paste0(c(input$response_variable_2, paste0(input$predictor_variables_2,collapse = "+")), collapse="~")
        formula <- paste0(c(formula1, formula2), collapse = "\n        ")

        #fit model
        modfit=sem(formula, data=fd)
        #summary(modfit1, standardized=T, rsq=T)

        semPaths(modfit, "std", edge.label.cex = 1, residuals = F)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
