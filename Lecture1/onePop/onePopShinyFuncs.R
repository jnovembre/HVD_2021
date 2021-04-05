source('onePopFuncs.R')
ui <- fluidPage(
    titlePanel("The Frequency Spectrum for a Single Population"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "Nanc",
                label = "Ancestral Population Size:",
                min = 100,
                max = 100000,
                value = 10000,
                step=100
            ),
            sliderInput(
                inputId = "tgrowth",
                label = "How long ago did the instantaneous change in population size occur?:",
                min = 100,
                max = 1000,
                value = 100,
                step=10
            ),
            sliderInput(
                inputId = "Nbottle",
                label = "Population Size after instantaneous change:",
                min = 100,
                max = 100000,
                value = 10000,
                step=100
            ),
            sliderInput(
                inputId = "Npres",
                label = "Present day population size after exponential growth/decline:",
                min = 100,
                max = 100000,
                value = 10000,
                step=100
            ),
            sliderInput(
                inputId = "sampSize",
                label = "Sample Size:",
                min = 2,
                max = 1000,
                value = 10,
                step=10
            ),
            actionButton("logAxes", "Log Axes")
        ),
        mainPanel(
            plotOutput("sizeHistoryPlot",height='600px'),
            plotOutput("fspecPlot",height='600px'),
        )
    )
)
server <- function(input,output) {



    gr <- reactive({
        log(input$Npres/input$Nbottle)/input$tgrowth
    })

    size.history <- reactive({
        constructPopHistory(c(input$Nbottle,input$Nanc),input$tgrowth,gr())
    })


    ## temp hack til I can get a button working or something
    my.xmax <- 5000
    my.xlim <- reactive({
        ## c(0,10^om)
        c(0,my.xmax)
        ## c(0,10^round(log(max(coals()),10),0))
    })

    output$sizeHistoryPlot <- renderPlot(
        sizeHistoryPlot(
            size.history(),
            my.xlim=my.xlim()
        )
    )




    params <- reactive(
        c(input$Nbottle/input$Nanc,
          input$Npres/input$Nanc,
          input$tgrowth/(2*input$Nanc)
          )
    )


    fspec <- reactive(
        dadi$Demographics1D$bottlegrowth(
                                params(),
                                list(as.integer(input$sampSize)),
                                as.integer(1000)
                            )[c(-1,-(input$sampSize+1))]
    )

    diversity <- reactive(
        dadi$Demographics1D$bottlegrowth(
                                params(),
                                list(as.integer(2)),
                                as.integer(1000)
                            )
    )






    output$fspecPlot <- renderPlot({
        freqs <- 1:(input$sampSize-1)
        my.x <- freqs/(input$sampSize)


        ## if(input$sampSize>100){
        ##     x.temp <- 10^c(ceiling(log(1/input$sampSize,10)):-1)
        ##     my.xaxis <- c(x.temp,0.9)
        ## }
        if(input$logAxes%%2==0){
            log.axes <- ''
            my.xlim <- c(0,input$sampSize)/input$sampSize
            my.ylim <- c(0,max(fspec())*1.05)
        } else {
            log.axes <- 'xy'
            my.xlim <- c(1,input$sampSize)/input$sampSize
            my.ylim <- c(min(fspec())*0.96,10^1.5)
        }
        par(mar=c(5.1,5.1,4.1,2.1))
        plot(
            my.x,
            fspec(),
            type='p',
            bty='n',
            pch=20,
            cex=2,
            xlab='',
            ylab='',
            log=log.axes,
            xlim=my.xlim,
            ylim=my.ylim

        )
        lines(
            my.x,
            1/freqs,
            lty=2,
            lwd=2
        )


        mtext(
            "Density",
            side=2,
            line=3,
            cex=2
        )
        mtext(
            "Sample Allele Frequency",
            side=1,
            line=3,
            cex=2
        )
        legend(
            'topright',
            legend=c("Constant Size Expectation"),
            lty=2,
            cex=1.6,
            bty='n'
        )
    }
    )





}
