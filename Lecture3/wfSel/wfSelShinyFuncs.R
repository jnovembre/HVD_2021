source('wfSelFuncs.R')
ui <- fluidPage(
    titlePanel("Positive Selection"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "N",
                label = "Population Size:",
                min = 100,
                max = 1000,
                value = 500,
                step=10
            ),
            sliderInput(
                inputId = "pstart",
                label = "Starting Allele Frequency:",
                min = 0.01,
                max = 0.99,
                value = 0.01,
                step=0.01
            ),
            sliderInput(
                inputId = "wAA",
                label = "Fitness of AA genotype:",
                min = 0.9,
                max = 1,
                value = 1,
                step=0.001
            ),
            sliderInput(
                inputId = "wAa",
                label = "Fitness of Aa genotype:",
                min = 0.9,
                max = 1,
                value = 1,
                step=0.001
            ),
            sliderInput(
                inputId = "waa",
                label = "Fitness of aa genotype:",
                min = 0.9,
                max = 1,
                value = 0.99,
                step=0.001
            )
            #actionButton("onOff", "Condition")
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

    output$trajPlot <- renderPlot(
        trajPlot(
            traj(),
            w()
        )
    )

    ##
}
