source('wfSimFuncs.R')
ui <- fluidPage(
    titlePanel("Wright-Fisher Sims"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "N",
                label = "Population Size:",
                min = 100,
                max = 10000,
                value = 100,
                step=10
            ),
            sliderInput(
                inputId = "ngens",
                label = "Number of Generations:",
                min = 10,
                max = 2000,
                value = 100,
                step=10
            ),
            sliderInput(
                inputId = "reps",
                label = "Number of Replicates:",
                min = 10,
                max = 100,
                value = 100,
                step=10
            ),
            sliderInput(
                inputId = "p0",
                label = "Starting Frequency:",
                min = 1/100,
                max = 99/100,
                value = 0.3,
                step=0.01
            )
                                        #actionButton("onOff", "Condition")
        ),
        mainPanel(
            plotOutput("freqPlot",height='600px'),
            plotOutput("hetPlot",height='600px'),
        )
    )
)
server <- function(input,output) {

    sims <- reactive({
        wfBinom(N=input$N, ngens=input$ngens, reps=input$reps, p0=input$p0)
    })

    output$freqPlot <- renderPlot(
        freqPlot(
            sims(),
            input$ngens,
            input$p0
        )
    )

    output$hetPlot <- renderPlot(
        hetPlot(
            sims(),
            input$ngens,
            input$p0,
            input$N
        )
    )

}
