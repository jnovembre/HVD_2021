source('structureFuncs.R')
ui <- fluidPage(
    titlePanel("Wright-Fisher Simulations"),
    actionButton("go", "Run simulations"), ## a button for rerunning the simulations
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "N",
                label = "Diploid Population Size:",
                min = 1000,
                max = 10000,
                value = 1000,
                step=10
            ),
            sliderInput(
                inputId = "ngens",
                label = "Number of Generations:",
                min = 10,
                max = 10000,
                value = 4000,
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
                inputId = "p1",
                label = "Starting Frequency Population 1:",
                min = 1/200,
                max = 199/200,
                value = 0.3,
                step=0.005
            ),
            sliderInput(
                inputId = "p2",
                label = "Starting Frequency Population 2:",
                min = 1/200,
                max = 199/200,
                value = 0.7,
                step=0.005
            ),
            sliderInput(
                inputId = "mig",
                label = "Log_10 Migration Rate:",
                min = -7,
                max = -1,
                value = -3 ,
                step=1/4
            ),
            sliderInput(
                inputId = "mut",
                label = "Log_10 Mutation Rate:",
                min = -7,
                max = -1,
                value = -7 ,
                step=1/4
            )
        ),
        mainPanel(
            plotOutput("freqPlot",height='600px'),
            plotOutput("fstPlot",height='600px'),
        )
    )
)
server <- function(input,output) {

    mig.rate <- eventReactive(input$go,{
        10^input$mig
    })
    mut.rate <- eventReactive(input$go,{
        10^input$mig
    })
    sims <- eventReactive(input$go,{
        wfBinomWMig(N=input$N, ngens=input$ngens, reps=input$reps, p0=c(input$p1,input$p2),m=mig.rate(),mu=mut.rate())
    })
    my.fsts <- eventReactive(input$go,{
        fst.list <- list()
        mean.fsts <- numeric()
        for(i in 1:length(sims())){
            tmp <- fst(sims()[[i]])
            fst.list[[i]] <- tmp[[1]]
            mean.fsts[i] <- tmp[[2]]
        }
        list(
            do.call(rbind,fst.list),
            mean.fsts
        )
    })


    ngens <- eventReactive(input$go,{
        input$ngens
    })
    Ne <- eventReactive(input$go,{
        input$N
    })

    output$freqPlot <- renderPlot(
        freqPlot(
            sims(),
            ngens()
        )
    )

    output$fstPlot <- renderPlot(
        fstPlot(
            my.fsts()[[1]],
            my.fsts()[[2]],
            ngens(),
            4*Ne()*mig.rate()
        )
    )


    ## output$hetPlot <- renderPlot(
    ##     hetPlot(
    ##         sims(),
    ##         ngens(),
    ##         p0(),
    ##         Ne()
    ##     )
    ## )

}
