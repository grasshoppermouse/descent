library(shiny)
library(DT)
library(GENLIB)

source('descent.R')

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9 * 1024 ^ 2)

function(input, output, session) {

  genealogyInput <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    library(readr)
    read_delim(inFile$datapath,
               delim = input$sep,
               quote = input$quote)

  })

  output$contents <- DT::renderDataTable(genealogyInput(),
   options = list(
    scrollY = '75vh',
    scrollX = TRUE,
    paging = FALSE
  ) )

  output$egoSelectUI <- renderUI({
    selectInput("egoVar", "Ego variable", names(genealogyInput()) )
  })

  output$motherSelectUI <- renderUI({
    selectInput("motherVar", "Mother variable", names(genealogyInput()) )
  })

  output$fatherSelectUI <- renderUI({
    selectInput("fatherVar", "Father variable", names(genealogyInput()) )
  })

  output$sexSelectUI <- renderUI({
    selectInput("sexVar", "Sex variable", names(genealogyInput()) )
  })

  output$livingdeadSelectUI <- renderUI({
    selectInput("livingdeadVar", "Living/dead variable", c('All living', names(genealogyInput())) )
  })

  errors <- eventReactive(input$checkErrors, {
    error_df(genealogyInput(),
             input$egoVar,
             input$motherVar,
             input$fatherVar,
             input$sexVar,
             input$femalevalue,
             input$malevalue,
             input$missingvalue)
  })

  output$errors <- DT::renderDataTable(errors(),
                                       options = list(
                                         scrollY = '75vh',
                                         scrollX = TRUE,
                                         paging = FALSE
                                       ) )


  phi <- eventReactive(input$computePhi, {
    gen.phi(gen.genealogy(as.pedigree(genealogyInput(),
                                              input$egoVar,
                                              input$motherVar,
                                              input$fatherVar,
                                              input$sexVar,
                                              input$femalevalue,
                                              input$malevalue,
                                              input$missingvalue),
                                  autoComplete = T))
  })

  output$phi <- DT::renderDataTable(phi(),
                                    options = list(
                                      scrollY = '75vh',
                                      scrollX = TRUE,
                                      paging = FALSE
                                    ) )

  output$downloadData <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      'relatedness.csv' # paste(input$dataset, input$filetype, sep = ".")
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- ',' # switch(input$filetype, "csv" = ",", "tsv" = "\t")

      # Write to a file specified by the 'file' argument
      write.table(phi(), file, sep = sep,
                  row.names = FALSE)
    }
  )
}
