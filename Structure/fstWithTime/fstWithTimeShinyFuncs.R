source('fstWithTimeFuncs.R')
ui <- fluidPage(
    titlePanel("Wright-Fisher Simulations"),
    actionButton("go", "Run simulations"), ## a button for rerunning the simulations
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
                max = 100000,
                value = 10000,
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
            plotOutput("fstPlot",height='600px'),
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
    my.fsts <- eventReactive(input$go,{
        mean.fsts <- numeric()
        for(i in 1:length(sims())){
            mean.fsts[i] <- fst(sims()[[i]])
        }
        mean.fsts
    })
    output$freqPlot <- renderPlot(
        freqPlot(
            do.call(cbind,sims()),
            ngens(),
            p0()
        )
    )
    output$fstPlot <- renderPlot(
        fstPlot(
            my.fsts(),
            ngens(),
            Ne()
        )
    )
}
