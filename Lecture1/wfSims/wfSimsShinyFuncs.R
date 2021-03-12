source('wfSimsFuncs.R')
ui <- fluidPage(
    titlePanel("Wright-Fisher Sims"),
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
                inputId = "ngens",
                label = "Number of Generations:",
                min = 10,
                max = 2000,
                value = 100,
                step=10
            ),
            sliderInput(
                inputId = "reps",
                label = "Number of Replicates:",
                min = 10,
                max = 100,
                value = 100,
                step=10
            )
            #actionButton("onOff", "Condition")
        ),
        mainPanel(
            plotOutput("simPlot",height='600px'),
        )
    )
)
server <- function(input,output) {

    ## gen <- reactive({
    ##     rnorm(n=10000,0,input$h2)
    ## })
    sims <- reactive({
        wfBinom(N=input$N, ngens=input$ngens=150, reps=input$reps)
    })

    ## env <- reactive({
    ##     rnorm(n=10000,0,1-input$h2)
    ## })

    ## hist.gens <- reactive({
    ##     rnorm(n=10000,input$h2*input$mp_phen,input$h2*(1-input$h2))
    ## })

    ## hist.envs <- reactive({
    ##     rnorm(n=10000,(1-input$h2)*input$mp_phen,input$h2*(1-input$h2))
    ## })

    ## xlims.scat <- reactive({
    ##     return(c(-2.5,2.5))
    ## })
    ## ylims.scat <- reactive({
    ##     return(c(-2,2))
    ## })

    ## xlims.gen <- reactive({
    ##     return(c(-2,2))
    ## })

    ## xlims.env <- reactive({
    ##     return(c(-2,2))
    ## })

    ## cex.axes <- reactive({
    ##     return(1.8)
    ## })

    ## x.label <- reactive({
    ##     return('Genetic Component')
    ## })

    ## y.label <- reactive({
    ##     return('Environmental Component')
    ## })

    output$simPlot <- renderPlot(
        simPlot(
            sims(),
            input$ngens
        )
    )

    ##
}
