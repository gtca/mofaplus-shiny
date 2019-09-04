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
#library(MOFA2)
load_all('../../MOFA2/MOFA2')

options(shiny.maxRequestSize = 1000*1024^2)

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
                tabPanel("Data overview", 
                    plotOutput("dataOverviewPlot")
                ),
                tabPanel("Variance", 
                         # TODO: selector x, y
                         # TODO: selector group_by
                         # TODO: embed multiple plots on one page
                         plotOutput("varianceExplainedPlot")
                ),
                tabPanel("Loadings", 
                         fluidRow(
                            column(5, sliderInput(inputId = "nfeatures_to_label",
                                                  label = "Number of features to label",
                                                  min = 0,
                                                  max = 100,
                                                  value = 10,
                                                  step = 1))
                         ),
                         plotOutput("weightsPlot")
                         # plotOutput("weightsPlot", hover = "weightsHover")
                         # verbatimTextOutput("weightsInfo")
                ),
                tabPanel("Factors", 
                         fluidRow(
                            column(2, uiOutput("factorsAxisChoice_x")),
                            column(3, uiOutput("factorsGroupsChoice")),
                            column(2, materialSwitch(inputId = "factorsAddDots", label = "Add dots", status = "default", value = TRUE)),
                            column(2, materialSwitch(inputId = "factorsAddViolins", label = "Add violins", status = "default", value = FALSE))
                         ),
                         hr(),
                         plotOutput("factorsPlot")
                ),
                tabPanel("Embeddings", 
                         fluidRow(
                            column(3, uiOutput("factorChoice_x")),
                            column(1, actionButton("swapEmbeddings", "", 
                                                   icon("exchange-alt"),
                                                   style = "margin: 25px 0;")),
                            column(3, uiOutput("factorChoice_y"))
                         ),
                         hr(),
                         plotOutput("embeddingsPlot", 
                                    brush = brushOpts(id = "plot_brush", fill = "#aaa")),
                         verbatimTextOutput("embeddingsInfo")
                )
            ),
            width = 9
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ###########################
    ### REACTIVE COMPONENTS ###

    ### GLOBAL VARIABLES ###
    
    model <- reactive({
        input_model <- input$model_hdf_file
        if (is.null(input_model)) return(NULL)
        load_model(input_model$datapath)
    })
    
    factorsChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        factors(m)
    })
    
    metaChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        metadata_names <- colnames(samples_metadata(m))
        metadata_names[metadata_names != "sample"]
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
    
    ### EMBERDDINGS ###
    
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

    ### FACTOR VALUES ###

    factorsAxisSelection_x <- reactive({
        if (is.null(input$colourChoice)) {
            selected_global <- input$colourChoice
            if (is.null(selected_global)) {
                return(metaChoice()[1])
            } else if (length(selected_global) >= 1) {
                return(selected_global[1])
            } else {
                return(metaChoice()[1])
            }
        }
        input$colourChoice
    })


    factorsGroupsChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        as.character(unique(samples_metadata(m)[,factorsAxisSelection_x()]))
    })
    


    #################
    ### RENDERING ###
    #################
    
    
    output$factorsChoice <- renderUI({
        selectInput('factorsChoice', 'Factors:', choices = factorsChoice(), multiple = TRUE, selectize = TRUE)
    })
    
    output$colourChoice <- renderUI({
        selectInput('colourChoice', 'Colour cells:', choices = metaChoice(), multiple = FALSE, selectize = TRUE)
    })

    ### MODEL OVERVIEW ###
    
    output$dataOverviewPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_data_overview(m)
    })

    ### VARIANCE EXPLAINED ###
    
    output$varianceExplainedPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_variance_explained(m, factors = factorsSelection(), 
                                x = 'group', y = 'factor',
                                plot_total = FALSE)
    })

    ### WEIGHTS (LOADINGS) ###

    output$weightsPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_weights(m, factors = factorsSelection(), nfeatures = input$nfeatures_to_label)
    })

    # output$weightsInfo <- renderPrint({
    #     m <- model()
    #     if (is.null(m)) return(NULL)
    #     nearPoints(plot_weights(m, factors = factorsSelection(), nfeatures = input$nfeatures_to_label, return_data = TRUE),
    #                input$weightsHover,
    #                threshold = 10, maxpoints = 3, addDist = TRUE)
    # })

    ### FACTOR VALUES ###
    
    output$factorsPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_factor(m, factors = factorsSelection(), color_by = colourSelection(), group_by = factorsAxisSelection_x(), 
                    groups = input$factorsGroupsChoice, add_dots = input$factorsAddDots, add_violin = input$factorsAddViolins)
    })

    output$factorsAxisChoice_x <- renderUI({
        selectInput('factorsAxisChoice_x', 'X axis:',
                    choices = metaChoice(), multiple = FALSE, selectize = TRUE,
                    selected = factorsAxisSelection_x())
    })

    output$factorsGroupsChoice <- renderUI({
        selectInput('factorsGroupsChoice', 'Display groups:',
                    choices = factorsGroupsChoice(), multiple = TRUE, selectize = TRUE,
                    selected = factorsGroupsChoice())
    })

    ### FACTORS SCATTERPLOT (EMBEDDINGS) ###
    
    output$embeddingsPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_factors(m, factors = c(factorSelection_x(), factorSelection_y()), color_by = colourSelection()) 
    })
    
    output$embeddingsInfo <- renderPrint({
        m <- model()
        if (is.null(m)) return(NULL)
        df <- plot_factors(m, factors = c(factorSelection_x(), factorSelection_y()), color_by = colourSelection(), return_data = TRUE) 
        brushedPoints(df, input$plot_brush)
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
    
    
    observeEvent(input$swapEmbeddings, {
        x_sel <- input$factorChoice_x
        y_sel <- input$factorChoice_y
        if (!is.null(x_sel) && !is.null(y_sel)) {
            output$factorChoice_y <- renderUI({
                selectInput('factorChoice_y', 'Factor on Y axis:', 
                            choices = factorsChoice(), multiple = FALSE, selectize = TRUE,
                            selected = x_sel)
            })
            
            output$factorChoice_x <- renderUI({
                selectInput('factorChoice_x', 'Factor on X axis:', 
                            choices = factorsChoice(), multiple = FALSE, selectize = TRUE,
                            selected = y_sel)
            })
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
