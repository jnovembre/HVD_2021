source('continentIslandFuncs.R')
ui <- fluidPage(
    titlePanel("Wright-Fisher Simulations"),
    actionButton("go", "Run simulations"), ## a button for rerunning the simulations
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "logN",
                label = "Log_10 Diploid Population Size:",
                min = 2,
                max = 7,
                value = 3,
                step=1/10
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
                inputId = "pCont",
                label = "Continental Allele Frequency:",
                min = 1/200,
                max = 199/200,
                value = 0.7,
                step=0.005
            ),
            sliderInput(
                inputId = "pIsle",
                label = "Initial Island Allele Frequency:",
                min = 1/200,
                max = 199/200,
                value = 0.7,
                step=0.005
            ),
            sliderInput(
                inputId = "logMig",
                label = "Log_10 Migration Rate:",
                min = -7,
                max = -2,
                value = -5 ,
                step=1/10
            )
            ## sliderInput(
            ##     inputId = "mut",
            ##     label = "Log_10 Mutation Rate:",
            ##     min = -7,
            ##     max = -1,
            ##     value = -7 ,
            ##     step=1/4
            ## )
        ),
        mainPanel(
            plotOutput("freqPlot",height='600px'),
            plotOutput("fstPlot",height='600px'),
        )
    )
)
server <- function(input,output) {
    mig.rate <- eventReactive(input$go,{
        10^input$logMig
    })
    pop.size <- eventReactive(input$go,{
        10^input$logN
    })
    sims <- eventReactive(input$go,{
        wfBinomWMig(N=pop.size(), ngens=input$ngens, reps=input$reps, pCont=input$pCont,pIsle=input$pIsle,m=mig.rate())
    })
    ngens <- eventReactive(input$go,{
        input$ngens
    })
    pCont <- eventReactive(input$go,{
        input$pCont
    })
    my.fsts <- eventReactive(input$go,{
        fst.list <- list()
        mean.fsts <- numeric()
        for(i in 1:length(sims())){
            mean.fsts[i] <- fst(sims()[[i]])
        }
        mean.fsts
    })
    output$freqPlot <- renderPlot(
        freqPlot(
            sims(),
            pCont(),
            ngens()
        )
    )
    output$fstPlot <- renderPlot(
        fstPlot(
            my.fsts(),
            ngens(),
            4*pop.size()*mig.rate(),
            pop.size()
        )
    )
}
