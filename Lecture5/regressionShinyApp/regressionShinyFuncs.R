source('regressionFuncs.R')
ui <- fluidPage(
    titlePanel("Regression"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "h2",
                label = "Heritability:",
                min = 0.01,
                max = 0.99,
                value = 0.7,
                step=0.01
            ),
            sliderInput(
                inputId = "mp_phen",
                label = "Midparent Phenotype:",
                min = -3,
                max = 3,
                value = 0,
                step=0.01
            ),
            sliderInput(
                inputId = "n.points",
                label = "Sample Size:",
                min = 1000,
                max = 100000,
                value = 1000,
                step=1000
            ),
            actionButton("onOff", "Condition on mid-Parent phenotype")
        ),
        mainPanel(
            plotOutput("regressionMp_Phen",height='600px'),
        )
    )
)
server <- function(input,output) {



    gen <- reactive({
        rnorm(n=input$n.points,0,input$h2)
    })

    env <- reactive({
        rnorm(n=input$n.points,0,1-input$h2)
    })

    hist.gens <- reactive({
        rnorm(n=10000,input$h2*input$mp_phen,input$h2*(1-input$h2))
    })

    hist.envs <- reactive({
        rnorm(n=10000,(1-input$h2)*input$mp_phen,input$h2*(1-input$h2))
    })

    xlims.scat <- reactive({
        return(c(-2.5,2.5))
    })
    ylims.scat <- reactive({
        return(c(-2,2))
    })

    xlims.gen <- reactive({
        return(c(-2,2))
    })

    xlims.env <- reactive({
        return(c(-2,2))
    })

    cex.axes <- reactive({
        return(1.8)
    })

    x.label <- reactive({
        return('Genetic Component')
    })

    y.label <- reactive({
        return('Environmental Component')
    })

    output$regressionMp_Phen <- renderPlot(
        moPlot(
            gen=gen(),
            env=env(),
            hist.gens=hist.gens(),
            hist.envs=hist.envs(),
            h2=input$h2,
            input$mp_phen,
            xlims.scat=xlims.scat(),
            ylims.scat=ylims.scat(),
            xlims.gen=xlims.gen(),
            xlims.env=xlims.env(),
            cex.axis=cex.axes(),
            xlab.scat=x.label(),
            ylab.scat=y.label(),
            conditionCounter=input$onOff
        )
    )
}
