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

library(tools)
library(ggplot2)
library(MOFA2)

options(shiny.maxRequestSize = 1500*1024^2)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = "styles.css",

    # Application title
    titlePanel("MOFA+ model exploration"),
    

    # Sidebar with main model parameters choices
    sidebarLayout(
        sidebarPanel(
            # Model file picker (HDF5 or RDS file)
            fileInput(inputId = "model_file",
                      label = "Model file:",
                      buttonLabel = "Model file",
                      accept = c("hdf5", "rds")),
            uiOutput("viewsChoice"),
            uiOutput("groupsChoice"),
            uiOutput("factorsChoice"),
            uiOutput("colourChoice"),
            div(id="descriptionMofa", checked=NA,
                span("More information on MOFA+ is "),
                a(href="https://github.com/bioFAM/MOFA2", " on GitHub", target="_blank"),
                # TODO: add link to the paper when available
                br(),
                span("Source code of this app is available "),
                a(href="https://github.com/gtca/mofaplus-shiny", "here", target="_blank"),
            ),
            width = 3
        ),
    

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Data overview", 
                    p("", class="description"),
                    plotOutput("dataOverviewPlot")
                ),
                tabPanel("Variance", 
                    p("", class="description"),
                    # TODO: selector x, y
                    # TODO: selector group_by
                    # TODO: embed multiple plots on one page
                    plotOutput("varianceExplainedPlot")
                ),
                tabPanel("Weights", 
                    p("Visualize factor weights as a first step to interpret factors.", class="description"),
                    fluidRow(
                       column(2, uiOutput("weightsViewSelection")),
                       column(5, sliderInput(inputId = "nfeatures_to_label",
                                             label = "Number of features to label:",
                                             min = 0,
                                             max = 100,
                                             value = 10,
                                             step = 1))
                    ),
                    plotOutput("weightsPlot")
                ),
                tabPanel("Factor exploration",
                    p("Explore factors one by one by plotting original data values for their top weights.", class="description"),
                    fluidRow(
                       column(2, uiOutput("dataFactorSelection")),
                       column(5, sliderInput(inputId = "nfeatures_to_plot",
                                             label = "Number of top features to plot:",
                                             min = 0,
                                             max = 100,
                                             value = 10,
                                             step = 1))
                    ),
                    plotOutput("dataHeatmapPlot"),
                    plotOutput("dataScatterPlot")
                ),
                tabPanel("Factors beeswarm", 
                    p("Visualize factor values and explore their distribution in different sets of samples", class="description"),
                    fluidRow(
                       column(2, uiOutput("factorsAxisChoice_x")),
                       # column(3, uiOutput("factorsGroupsChoice")),
                       column(1, switchInput(inputId = "factorsAddDots", label = "Points", value = TRUE),
                                 style = "margin-top: 25px; margin-right: 25px;"),
                       column(1, switchInput(inputId = "factorsAddViolins", label = "Violins", value = FALSE),
                                 style = "margin-top: 25px; margin-right: 25px;")
                    ),
                    hr(),
                    plotOutput("factorsPlot")
                ),
                tabPanel("Factors scatter", 
                    p("Visualize pairs of factors to study how they separate different sets of samples.", class="description"),
                    fluidRow(
                       column(3, uiOutput("factorChoice_x")),
                       column(1, actionButton("swapEmbeddings", "", 
                                              icon("exchange-alt"),
                                              style = "margin: 25px 0;")),
                       column(3, uiOutput("factorChoice_y")),
                       column(3, uiOutput("factorsGroupsChoice_xy"))
                    ),
                    hr(),
                    plotOutput("embeddingsPlot", 
                               brush = brushOpts(id = "plot_factors", fill = "#aaa")),
                    verbatimTextOutput("embeddingsInfo")
                ),
                tabPanel("Embeddings", 
                    p("Run non-linear dimensionality reduction method on factors.", class="description"),
                    fluidRow(
                       column(2, uiOutput("manifoldChoice"))
                    ),
                    hr(),
                    plotOutput("dimredPlot")
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
        input_model <- input$model_file
        if (is.null(input_model)) return(NULL)
        print(file_ext(input_model$name))
        if (file_ext(input_model$name) %in% c("hdf5", "h5")) {
          load_model(input_model$datapath)
        } else if (file_ext(input_model$name) %in% c("rds", "RDS")) {
          readRDS(input_model$datapath)
        } else {
          return(NULL)
        }
    })
    
    factorsChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        factors_names(m)
    })
    
    viewsChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        views_names(m)
    })
    
    groupsChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        groups_names(m)
    })
    
    metaChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        metadata_names <- colnames(samples_metadata(m))
        metadata_names[metadata_names != "sample"]
    })

    metaAndFeatureChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        choices_names <- colnames(samples_metadata(m))
        choices_names <- choices_names[choices_names != "sample"]
        c(choices_names, features_names(m))
    })

    metaFeatureFactorChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        choices_names <- colnames(samples_metadata(m))
        choices_names <- choices_names[choices_names != "sample"]
        c(list("metadata" = choices_names), 
          features_names(m),
          list("factors" = factors_names(m)))
    })
    
    dimredChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        if (!.hasSlot(m, "dim_red") || (length(m@dim_red) == 0)) 
            return(c("UMAP", "TSNE"))  # to be computed
            # no t-SNE due to its frequent problems with 
            # perplexity too high for small datasets
        names(m@dim_red)
    })

    ### Remembering selected subset of views, groups, factors, etc.

    viewsSelection <- reactive({
        if (is.null(input$viewsChoice))
            return("all")
        input$viewsChoice
    })


    groupsSelection <- reactive({
        if (is.null(input$groupsChoice))
            return("all")
        input$groupsChoice
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

    ### LOADINGS ###

    weightsViewSelection <- reactive({
        if (is.null(input$weightsViewSelection))
            return(1)
        input$weightsViewSelection
    })

    ### DATA ###

    dataFactorSelection <- reactive({
        if (is.null(input$dataFactorSelection))
            return(1)
        input$dataFactorSelection
    })
    
    ### EMBERDDINGS ###
    
    factorSelection_x <- reactive({
        selected_global <- input$factorsChoice
        if (is.null(selected_global)) {
            return(factorsChoice()[1])
        } else if (length(selected_global) >= 1) {
            return(selected_global[1])
        } else {
            return(factorsChoice()[1])
        }
        input$factorChoice_x
    })
    
    factorSelection_y <- reactive({
        selected_global <- input$factorsChoice
        if (is.null(selected_global)) {
            return(factorsChoice()[2])
        } else if (length(selected_global) > 1) {
            return(selected_global[2])
        } else {
            return(factorsChoice()[2])
        }
        input$factorChoice_y
    })

    ### FACTOR VALUES ###

    factorsAxisSelection_x <- reactive({
        selected_global <- input$factorsAxisChoice_x
        if (is.null(selected_global)) {
            return(metaChoice()[1])
        } else if (length(selected_global) >= 1) {
            return(selected_global[1])
        } else {
            return(metaChoice()[1])
        }
    })


    factorsGroupsChoice <- reactive({
        m <- model()
        if (is.null(m)) return(NULL)
        as.character(unique(samples_metadata(m)[,factorsAxisSelection_x()]))
    })


    ### MANIFOLD VALUES ###
    manifoldSelection <- reactive({
        selected_global <- input$manifoldChoice    
        if (is.null(selected_global)) {
            return(dimredChoice()[1])
        } else if (length(selected_global) >= 1) {
            return(selected_global[1])
        } else {
            return(dimredChoice()[1])
        }
    })


    #################
    ### RENDERING ###
    #################
    
    
    output$viewsChoice <- renderUI({
        selectInput('viewsChoice', 'Views:', choices = viewsChoice(), multiple = TRUE, selectize = TRUE)
    })

    output$groupsChoice <- renderUI({
        selectInput('groupsChoice', 'Groups:', choices = groupsChoice(), multiple = TRUE, selectize = TRUE)
    })

    output$factorsChoice <- renderUI({
        selectInput('factorsChoice', 'Factors:', choices = factorsChoice(), multiple = TRUE, selectize = TRUE)
    })
    
    output$colourChoice <- renderUI({
        selectInput('colourChoice', 'Colour cells:', choices = metaFeatureFactorChoice(), multiple = FALSE, selectize = TRUE)
    })

    ### MODEL OVERVIEW ###
    
    output$dataOverviewPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        # Use custom colour palette
        shiny_palette <- c("#B1BED5", "#BFD8D5", "#DFDFDF", "#6392A3", "f4f3f3")
        n_views <- get_dimensions(m)$M
        if (n_views <= length(shiny_palette)) {
            shiny_colours <- shiny_palette[seq_len(n_views)] 
        } else {
            shiny_colours <- rainbow(n_views)
        }
        names(shiny_colours) <- views_names(m)
        plot_data_overview(m, colors = shiny_colours) +
            theme(strip.text.x = element_text(size = 16, colour = "#333333"), 
                  axis.text.y = element_text(size = 16, colour = "#333333"))
    })

    ### VARIANCE EXPLAINED ###
    
    output$varianceExplainedPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_variance_explained(m, 
                                views = viewsSelection(), groups = groupsSelection(), factors = factorsSelection(), 
                                plot_total = FALSE, use_cache = FALSE) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                  strip.text.x = element_text(size = 14, colour = "#333333"))
    })

    ### WEIGHTS (LOADINGS) ###

    output$weightsViewSelection <- renderUI({
        selectInput('weightsViewSelection', 'View:', choices = viewsChoice(), multiple = FALSE, selectize = TRUE)
    })

    output$weightsPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_weights(m, view = weightsViewSelection(), factors = factorsSelection(), nfeatures = input$nfeatures_to_label)
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
                    groups = groupsSelection(), add_dots = input$factorsAddDots, add_violin = input$factorsAddViolins)
    })

    output$factorsAxisChoice_x <- renderUI({
        selectInput('factorsAxisChoice_x', 'X axis:',
                    choices = metaChoice(), multiple = FALSE, selectize = TRUE,
                    selected = factorsAxisSelection_x())
    })

    ### DATA (SINGLE FACTOR EXPLORATION) ###

    output$dataFactorSelection <- renderUI({
        selectInput('dataFactorSelection', 'Factor:', choices = factorsChoice(), multiple = FALSE, selectize = TRUE)
    })

    output$dataHeatmapPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        annotation_samples <- NULL
        if (colourSelection() %in% colnames(samples_metadata(m))) annotation_samples <- colourSelection()
        plot_data_heatmap(m, view = viewsSelection(), groups = groupsSelection(), 
                          factor = dataFactorSelection(), features = input$nfeatures_to_plot,
                          annotation_samples = annotation_samples)
    })

    output$dataScatterPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_data_scatter(m, view = viewsSelection(), groups = groupsSelection(), 
                          factor = dataFactorSelection(), features = input$nfeatures_to_plot)
    })


    ### FACTORS SCATTERPLOT (EMBEDDINGS) ###
    
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
    
    output$embeddingsPlot <- renderPlot({
        m <- model()
        if (is.null(m)) return(NULL)
        plot_factors(m, groups = groupsSelection(), factors = c(input$factorChoice_x, input$factorChoice_y), color_by = colourSelection()) 
    })
    
    output$embeddingsInfo <- renderPrint({
        m <- model()
        if (is.null(m)) return(NULL)
        df <- plot_factors(m, groups = groupsSelection(), factors = c(input$factorChoice_x, input$factorChoice_y), color_by = colourSelection(), return_data = TRUE) 
        brushedPoints(df, input$plot_factors)
    })

    # output$factorsGroupsChoice_xy <- renderUI({
    #     selectInput('factorsGroupsChoice_xy', 'Display groups:',
    #                 choices = factorsGroupsChoice(), multiple = TRUE, selectize = TRUE,
    #                 selected = factorsGroupsChoice())
    # })
    
    
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


    ### DIMENSIONALITY REDUCTION PLOT ###

    output$manifoldChoice <- renderUI({
        selectInput('manifoldChoice', 'Manifold:',
                    choices = dimredChoice(), multiple = FALSE, selectize = TRUE,
                    selected = manifoldSelection())
    })
    
    output$dimredPlot <- renderPlot({
        m <- model()
        method <- manifoldSelection()
        if (is.null(m) || is.null(method) || (method == "")) {
            return(NULL)
        } else {
            if (method == "TSNE") {
                plot_dimred(m, method, groups = groupsSelection(), factors = factorsSelection(), color_by = colourSelection(),
                        # Provide perplexity for t-SNE since the default one is typically too high for small datasets
                        perplexity = 10)     
            } else {
                plot_dimred(m, method, groups = groupsSelection(), factors = factorsSelection(), color_by = colourSelection())
            }
        }
    })


    # # Saving the plots
    # output$saveButtonLoadings <- downloadHandler(
    #     filename = "mofa2_plot.pdf",
    #     content = function(file) {
    #         ggsave(file, device = "pdf",
    #                plot = plot_weights(model(), view = loadingsViewSelection(), factors = factorsSelection(), nfeatures = input$nfeatures_to_label))
    #     }
    # )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
