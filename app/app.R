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
ui <- fluidPage(

    # Application title
    titlePanel("MOFA+ model exploration"),
    

    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
        # sidebarPanel(
        #     # sliderInput("bins",
        #     #             "Number of bins:",
        #     #             min = 1,
        #     #             max = 50,
        #     #             value = 30),
        #     # HDF5 model file picker
        #     fileInput(inputId = "model_hdf_file",
        #               label = "Model HDF5 file:",
        #               buttonLabel = "Model file",
        #               accept = "hdf5")
        #                 
        # ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Model overview", 
                    # HDF5 model file picker
                    fileInput(inputId = "model_hdf_file",
                              label = "Model HDF5 file:",
                              buttonLabel = "Model file",
                              accept = "hdf5"),
                    plotOutput("modelOverviewPlot")
                ),
                tabPanel("Loadings", 
                         uiOutput("factorsChoice_loadings"),
                         # sidebarPanel(selectInput("factorsChoice", "Choose one or more factors:", choices = 1:2, multiple = TRUE)),
                         plotOutput("weightsPlot")
                ),
                tabPanel("Factors", 
                         uiOutput("factorsChoice_factors"),
                         plotOutput("factorsPlot")
                )
            )
        )
    # )
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
    
    factorsSelection_loadings <- reactive({
        if (is.null(input$factorsChoice_loadings))
            return("all")
        input$factorsChoice_loadings
    })
    
    factorsSelection_factors <- reactive({
        if (is.null(input$factorsChoice_factors))
            return("all")
        input$factorsChoice_factors
    })
    
    # userSelection <- reactiveValues(factorsSelection = reactive({
    #     if (is.null(input$factorsChoice))
    #         return("all")
    #     input$factorsChoice
    # }))
    
    output$factorsChoice_loadings <- renderUI({
        selectInput('factorsChoice_loadings', 'Factors:', choices = factorsChoice(), multiple = TRUE, selected = factorsSelection_factors())
    })
    output$factorsChoice_factors <- renderUI({
        selectInput('factorsChoice_factors', 'Factors:', choices = factorsChoice(), multiple = TRUE, selected = factorsSelection_loadings())
    })
    
    output$modelOverviewPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_data_overview(m)
    })

    output$weightsPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_weights(m, factors = factorsSelection_loadings()) 
    })
    
    output$factorsPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_factors_jitter(m, factors = factorsSelection_factors()) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
