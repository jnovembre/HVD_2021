require(colorRamps)
require(shiny)
source('additiveVarianceFuncs.R')

ui <- fluidPage(
    titlePanel("Dominance and Additivity"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "domdev",
                label = "Dominance deviation:",
                min = -1,
                max = 1,
                value = 0.5,
                step=0.01
            )
        ),
        mainPanel(
            plotOutput("propAddPlot"),
        )
    )
)
server <- function(input,output) {

    xlims <- reactive({
        return(c(0,1))
    })
    ylims <- reactive({
        return(c(0,1))
    })

    xlabel <- reactive('Total Variance')
    ylabel <- reactive('Proportion of Variance that is Additive')
    
    
    output$propAddPlot <- renderPlot(
        propAddPlot(
            input$domdev,
            xlims(),
            ylims(),
            xlabel=xlabel(),
            ylabel=ylabel()
        )
    )
}

