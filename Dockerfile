FROM gtca/r3.5.2_python3:latest

WORKDIR /mofaplus
ADD app /mofaplus/shiny
ADD MOFA2 /mofaplus/mofa2

RUN python3 /mofaplus/mofa2/setup.py install

# Install shiny dependencies
RUN R --vanilla -e "sapply(c('shiny', 'shinyWidgets'), BiocManager::install)"

RUN R CMD INSTALL --build /mofaplus/mofa2/MOFA2

EXPOSE 3838

CMD R -e "shiny::runApp('/mofaplus/shiny/', port = 3838, launch.browser = FALSE, host = '0.0.0.0')"
