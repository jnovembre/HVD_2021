source('additiveVarianceFuncs.R')
ui <- fluidPage(
    titlePanel("Dominance and Additivity"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "frequency",
                label = "Allele Frequency:",
                min = 0.01,
                max = 0.99,
                value = 0.2
            ),
            sliderInput(
                inputId = "homdiff",
                label = "Additive effect:",
                min = -1,
                max = 1,
                value = 1,
                step=0.01
            ),
            sliderInput(
                inputId = "domdev",
                label = "Dominance deviation:",
                min = -1,
                max = 1,
                value = 0.5,
                step=0.01
            )
        ),
        mainPanel(
            plotOutput("additivePlot"),
            textOutput("add_var"),
            textOutput("dom_var"),
            textOutput("narrow_sense_frac")
        )
    )
)

server <- function(input,output) {

    addVar <- reactive({
        add.var <- getAddVar(input$frequency,input$homdiff,input$domdev)
        return(add.var)
    })
    domVar <- reactive({
        dom.var <- getDomVar(input$frequency,input$homdiff,input$domdev)
        return(dom.var)
    })

    xlims <- reactive(c(0,2))
    ylims <- reactive(c(-1,1)*2)
    
    xlabel <- reactive('Genotype')
    ylabel <- reactive('Phenotype')

   
    output$additivePlot <- renderPlot(
        plotAddAndDom(
            input,
            xlim=xlims(),
            ylim=ylims(),
            xlabel=xlabel(),
            ylabel=ylabel()
        )
        
    )
        
    output$add_var <- renderPrint({
        paste(
            "The additive genetic variance is: ",
            addVar()
        )
    })
    output$dom_var <- renderPrint({
        paste(
            "The dominance genetic variance is: ",
            domVar()
        )
    })
    output$narrow_sense_frac <- renderPrint({
        paste(
            "The narrow sense heritability explains:",
            round(addVar()/(addVar()+domVar()),2),
            " of the total genetic variance."
        )
    })
}






