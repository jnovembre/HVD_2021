source('wfSimsFuncs.R')
ui <- fluidPage(
    titlePanel("Wright-Fisher Simulations"),
    actionButton("go", "Run simulations"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "N",
                label = "Diploid Population Size:",
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
                max = 1000,
                value = 100,
                step=10
            ),
            sliderInput(
                inputId = "p0",
                label = "Starting Frequency:",
                min = 1/200,
                max = 199/200,
                value = 0.3,
                step=0.005
            )
        ),
        mainPanel(
            plotOutput("freqPlot",height='600px'),
            plotOutput("hetPlot",height='600px'),
        )
    )
)
server <- function(input,output) {

    sims <- eventReactive(input$go,{
        wfBinom(N=input$N, ngens=input$ngens, reps=input$reps, p0=input$p0)
    })

    ngens <- eventReactive(input$go,{
        input$ngens
    })
    p0 <- eventReactive(input$go,{
        input$p0
    })
    Ne <- eventReactive(input$go,{
        input$N
    })

    output$freqPlot <- renderPlot(
        freqPlot(
            sims(),
            ngens(),
            p0()
        )
    )

    output$hetPlot <- renderPlot(
        hetPlot(
            sims(),
            ngens(),
            p0(),
            Ne()
        )
    )

}
