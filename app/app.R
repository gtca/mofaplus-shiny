#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

library(devtools)
load_all('../../BioFAM/BioFAMtools/')

options(shiny.maxRequestSize = 300*1024^2)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = "styles.css",

    # Application title
    titlePanel("MOFA+ model exploration"),
    

    # Sidebar with main model parameters choices
    sidebarLayout(
        sidebarPanel(
            # HDF5 model file picker
            fileInput(inputId = "model_hdf_file",
                      label = "Model HDF5 file:",
                      buttonLabel = "Model file",
                      accept = "hdf5"),
            uiOutput("factorsChoice"),
            uiOutput("colourChoice"),
            width = 3
        ),
    

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Model overview", 
                    plotOutput("modelOverviewPlot")
                ),
                tabPanel("Variance", 
                         # TODO: selector x, y
                         # TODO: selector group_by
                         # TODO: embed multiple plots on one page
                         plotOutput("varianceExplainedPlot")
                ),
                tabPanel("Loadings", 
                         # TODO: show/hide labels of N genes
                         # TODO: N ^
                         # TODO: Sort values by factor ...
                         plotOutput("weightsPlot")
                ),
                tabPanel("Factors", 
                         # TODO: group_by
                         plotOutput("factorsPlot")
                ),
                tabPanel("Embeddings", 
                         fluidRow(
                            column(3, uiOutput("factorChoice_x")),
                            column(3, uiOutput("factorChoice_y"))
                         ),
                         hr(),
                         plotOutput("embeddingsPlot")
                )
            ),
            width = 9
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    model <- reactive({
        input_model <- input$model_hdf_file
        if (is.null(input_model)) return(NULL)
        load_model(input_model$datapath)
    })
    
    factorsChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        factors_names(m)
    })
    
    coloursChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        metadata_names <- colnames(samples_metadata(m))
        metadata_names[metadata_names != "sample_name"]
    })
    
    factorsSelection <- reactive({
        if (is.null(input$factorsChoice))
            return("all")
        input$factorsChoice
    })
    
    colourSelection <- reactive({
        if (is.null(input$colourChoice))
            return("group_name")
        input$colourChoice
    })
    
    
    
    factorSelection_x <- reactive({
        if (is.null(input$factorChoice_x)) {
            selected_global <- input$factorsChoice
            if (is.null(selected_global)) {
                return(factorsChoice()[1])
            } else if (length(selected_global) >= 1) {
                return(selected_global[1])
            } else {
                return(factorsChoice()[1])
            }
        }
        input$factorChoice_x
    })
    
    factorSelection_y <- reactive({
        if (is.null(input$factorChoice_x)) {
            selected_global <- input$factorsChoice
            if (is.null(selected_global)) {
                return(factorsChoice()[2])
            } else if (length(selected_global) > 1) {
                return(selected_global[2])
            } else {
                return(factorsChoice()[2])
            }
        }
        input$factorChoice_y
    })
    
    
    output$factorsChoice <- renderUI({
        selectInput('factorsChoice', 'Factors:', choices = factorsChoice(), multiple = TRUE, selectize = TRUE)
    })
    
    output$colourChoice <- renderUI({
        selectInput('colourChoice', 'Colour cells:', choices = coloursChoice(), multiple = FALSE, selectize = TRUE)
    })
    
    output$modelOverviewPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_data_overview(m)
    })
    
    output$varianceExplainedPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_variance_explained(m, factors = factorsSelection(), 
                                x = 'group', y = 'factor',
                                plot_total = FALSE)
    })

    output$weightsPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_weights(m, factors = factorsSelection()) 
    })
    
    output$factorsPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_factors(m, factors = factorsSelection(), color_by = colourSelection()) 
    })
    
    output$embeddingsPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_embeddings(m, factors = c(factorSelection_x(), factorSelection_y()), color_by = colourSelection()) 
    })
    
    output$factorChoice_x <- renderUI({
        selectInput('factorChoice_x', 'Factor on X axis:', 
                    choices = factorsChoice(), multiple = FALSE, selectize = TRUE,
                    selected = factorSelection_x())
    })
    
    output$factorChoice_y <- renderUI({
        selectInput('factorChoice_y', 'Factor on Y axis:', 
                    choices = factorsChoice(), multiple = FALSE, selectize = TRUE,
                    selected = factorSelection_y())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
