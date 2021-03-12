source('wfCondSimsFuncs.R')
ui <- fluidPage(
    titlePanel("Conditional Wright-Fisher Sims"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "N",
                label = "Population Size:",
                min = 100,
                max = 10000,
                value = 1000,
                step=10
            ),
            sliderInput(
                inputId = "p0",
                label = "Starting Frequency:",
                min = 1/100,
                max = 99/100,
                value = 0.6,
                step=0.01
            )
                                        #actionButton("onOff", "Condition")
        ),
        mainPanel(
            plotOutput("freqPlot",height='600px'),
            plotOutput("ageHistPlot",height='600px'),
        )
    )
)
server <- function(input,output) {

    sims <- reactive({
        wfCondBinom(N=input$N, reps=1000, p0=input$p0)
    })
    ages <- reactive({
        getAges(sims())
    })

    output$freqPlot <- renderPlot(
        freqPlot(
            sims(),
            input$N,
            input$p0
        )
    )

    output$ageHistPlot <- renderPlot(
        ageHistPlot(
            ages(),
            input$N
        )
    )

}
