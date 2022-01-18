source('wfCondSimsFuncs.R')
ui <- fluidPage(
    titlePanel("Conditional Wright-Fisher Sims"),
    actionButton("go", "Run simulations"),
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
                label = "Present Frequency:",
                min = 1/100,
                max = 99/100,
                value = 0.6,
                step=0.01
            )
        ),
        mainPanel(
            plotOutput("freqPlot",height='600px'),
            plotOutput("ageHistPlot",height='600px'),
        )
    )
)
server <- function(input,output) {

    sims <- eventReactive(input$go,{
        wfCondBinom(N=input$N, reps=1000, p0=input$p0)
    })
    ages <- eventReactive(input$go,{
        getAges(sims())
    })
    Ne <- eventReactive(input$go,{
        input$N
    })
    p0 <- eventReactive(input$go,{
        input$p0
    })

    output$freqPlot <- renderPlot(
        freqPlot(
            sims(),
            Ne(),
            p0()
        )
    )

    output$ageHistPlot <- renderPlot(
        ageHistPlot(
            ages(),
            Ne()
        )
    )

}
