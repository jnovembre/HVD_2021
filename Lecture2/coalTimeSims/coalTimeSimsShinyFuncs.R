source('coalTimeSimsFuncs.R')
ui <- fluidPage(
    titlePanel("Wright-Fisher Sims"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "N1",
                label = "Population size in recent epoch:",
                min = 100,
                max = 10000,
                value = 1000,
                step=100
            ),
            sliderInput(
                inputId = "N2",
                label = "Population size in middle epoch:",
                min = 100,
                max = 10000,
                value = 1000,
                step=100
            ),
            sliderInput(
                inputId = "Nanc",
                label = "Population size in oldest epoch:",
                min = 100,
                max = 10000,
                value = 1000,
                step=100
            ),
            sliderInput(
                inputId = "t1",
                label = "Date of transition from middle to recent epoch:",
                min = 100,
                max = 2000,
                value = 100,
                step=10
            ),
            sliderInput(
                inputId = "t2",
                label = "Date of transition from middle to recent epoch:",
                min = 2000,
                max = 20000,
                value = 2000,
                step=10
            ),
            sliderInput(
                inputId = "reps",
                label = "Number of Replicates:",
                min = 100,
                max = 10000,
                value = 1000,
                step=100
            )
            #actionButton("onOff", "Condition")
        ),
        mainPanel(
            plotOutput("sizeHistoryPlot",height='600px'),
            plotOutput("coalTimePlot",height='600px')
        )
    )
)
server <- function(input,output) {

    ## gen <- reactive({
    ##     rnorm(n=10000,0,input$h2)
    ## })
    size.history <- reactive({
        constructPopHistory(N=c(input$N1,input$N2),t=c(input$t1,input$t2))
        })

    coals <- reactive({
        recover()
        coalTimeSims(N=size.history(),Nanc=input$Nanc,reps=input$reps)
    })

    my.xlim <- reactive({
        max.coal <- max(coals())
        om <- round(log(max.coal,10),0)
        c(0,10^om)
        ## c(0,10^round(log(max(coals()),10),0))
    })

    #print(my.xlim())

    output$sizeHistoryPlot <- renderPlot(
        sizeHistoryPlot(
            size.history(),
            my.xlim=my.xlim()
        )
    )

    output$coalTimePlot <- renderPlot(
        coalTimePlot(
            coals(),
            my.xlim=my.xlim()
        )
    )

}
