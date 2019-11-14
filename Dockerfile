FROM gtca/mofa2:latest

WORKDIR /mofa2
ADD app /mofa2/shiny

# Install shiny dependencies
RUN R --vanilla -e "sapply(c('shiny', 'shinyWidgets'), BiocManager::install)"

EXPOSE 3838

CMD R -e "shiny::runApp('/mofa2/shiny/', port = 3838, launch.browser = FALSE, host = '0.0.0.0')"
