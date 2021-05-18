source('wfSelFuncs.R')
ui <- fluidPage(
    titlePanel("Positive Selection"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "N",
                label = "Population Size:",
                min = 100,
                max = 20000,
                value = 5000,
                step=10
            ),
            sliderInput(
                inputId = "pstart",
                label = "Starting Allele Frequency:",
                min = 0.01,
                max = 0.99,
                value = 0.02,
                step=0.01
            ),
            sliderInput(
                inputId = "wAA",
                label = "Fitness of AA genotype:",
                min = 0.98,
                max = 1,
                value = 1,
                step=0.0001
            ),
            sliderInput(
                inputId = "wAa",
                label = "Fitness of Aa genotype:",
                min = 0.98,
                max = 1,
                value = 0.995,
                step=0.0001
            ),
            sliderInput(
                inputId = "waa",
                label = "Fitness of aa genotype:",
                min = 0.98,
                max = 1,
                value = 0.99,
                step=0.0001
            ),
            sliderInput(
                inputId = "reps",
                label = "Number of replicate populations:",
                min = 10,
                max = 1000,
                value = 100,
                step = 10
            )
            ## actionButton("onOff", "Condition")
        ),
        mainPanel(
            plotOutput("trajPlot",height='600px'),
        )
    )
)
server <- function(input,output) {



    w <- reactive(
        c(input$wAA,input$wAa,input$waa)
    )
    p.start <- reactive(
        input$pstart
    )



    traj <- reactive({
        getDetTrajectory(w(),p.start())
    })

    ngens <- reactive({
        length(traj())

    })



    stoch.trajs <- reactive({
        wfBinomPosSel(input$N,w(),ngens(),input$reps,input$pstart,mu=0)
    })
    mean.traj <- reactive({
        rowMeans(stoch.trajs())
    })



    output$trajPlot <- renderPlot({
        trajPlot(
            traj(),
            w()
        )
        matplot(
            stoch.trajs(),
            type='l',
            lty=1,
            lwd=0.6,
            add=TRUE,
            col=adjustcolor("grey",0.3)
        )
        trajPlotLines(
            traj(),
            w()
        )
        lines(
            mean.traj(),
            col='darkgreen',
            lwd=2
        )
    })

    ##
}
