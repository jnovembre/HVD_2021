source('coalTimeSimsFuncs.R')

ui <- fluidPage(
    titlePanel("Coalescent Time Distributions"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "N1",
                label = "Population size at beginning of recent epoch:",
                min = 100,
                max = 10000,
                value = 10000,
                step=100
            ),
            sliderInput(
                inputId = "r",
                label = "Growth rate in recent epoch:",
                min = -0.005,
                max = 0.02,
                value = 0,
                step=0.0001
            ),
            sliderInput(
                inputId = "t1",
                label = "Length of recent epoch:",
                min = 100,
                max = 2000,
                value = 1000,
                step=10
            ),
            sliderInput(
                inputId = "N2",
                label = "Population size in middle epoch:",
                min = 100,
                max = 10000,
                value = 10000,
                step=100
            ),
            sliderInput(
                inputId = "t2",
                label = "Length of middle epoch:",
                min = 0,
                max = 20000,
                value = 2000,
                step=10
            ),
            sliderInput(
                inputId = "Nanc",
                label = "Population size in ancient epoch:",
                min = 100,
                max = 10000,
                value = 10000,
                step=100
            ),
            sliderInput(
                inputId = "nSamp",
                label = "Sample Size",
                min = 2,
                max = 100,
                value = 2,
                step = 1
            )
        ),
        mainPanel(
            plotOutput("sizeHistoryPlot",height='600px'),
            plotOutput("coalPDFPlot",height='600px'),
            plotOutput("coalCDFPlot",height='600px')
        )
    )
)
server <- function(input,output) {

    boundaries <- reactive(
        cumsum(c(input$t1,input$t2))
    )


    size.history <- reactive({
        constructPopHistory(N=c(input$N1,input$N2,input$Nanc),boundaries=boundaries(),gr=c(input$r,0))
    })

    coal.dist <- reactive({
        coalTimeDist(
            size.history(),
            nSamp=input$nSamp
        )
    })

    ## temp hack til I can get a button working or something
    my.xmax <- 40000
    my.xlim <- reactive({
        max.coal <- which.max(coal.dist()$cdf>0.99)
        om <- round(log(max.coal,10),0)
        ## c(0,10^om)
        c(0,my.xmax)
        ## c(0,10^round(log(max(coals()),10),0))
    })

    mean.time <- reactive({
        ## deep.coal.mean.time <- 2*input$Nanc+length(coal.dist()$cdf)
        ## p.not.deep <- tail(coal.dist()$cdf,1)
        ## p.deep <- 1-p.not.deep
        ## sum(1-coal.dist()$cdf)*p.not.deep + p.deep*deep.coal.mean.time
        sum(1-coal.dist()$cdf)
    })

    mean.rate <- reactive({
        1/mean.time()
    })

    output$sizeHistoryPlot <- renderPlot(
        sizeHistoryPlot(
            size.history(),
            my.xlim=my.xlim()
        )
    )

    output$coalPDFPlot <- renderPlot({
        coalPDFPlot(
            coal.dist()$pdf,
            my.xlim=my.xlim(),
            mean.coal.time=mean.time(),
            nSamp=input$nSamp
        )

    })

    output$coalCDFPlot <- renderPlot(
        coalCDFPlot(
            coal.dist()$cdf,
            my.xlim=my.xlim(),
            eb=boundaries()
        )
    )

}
