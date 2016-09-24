library(shiny)
library(DT)
library(GENLIB)

source('descent.R')

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9 * 1024 ^ 2)

function(input, output, session) {

  observe({
    if (input$quit > 0) stopApp()  # stop shiny
  })

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
    read_delim(
      inFile$datapath,
      delim = input$sep,
      quote = input$quote,
      col_names = input$header
    )

  })

  output$contents <- DT::renderDataTable(
    genealogyInput(),
    options = list(
      scrollY = '75vh',
      scrollX = TRUE,
      paging = FALSE
    ),
    rownames = FALSE
  )

  select_var <- function(nms, vars) {
    for (nm in nms) {
      if (tolower(nm) %in% vars)
        return(nm)
    }
    return(nms[1])
  }

  output$egoSelectUI <- renderUI({
    nms <- names(genealogyInput())
    s <- select_var(nms, c('ego', 'id', 'ind', 'name'))
    selectInput("egoVar", "Ego variable", nms, selected = s)
  })

  output$motherSelectUI <- renderUI({
    nms <- names(genealogyInput())
    s <- select_var(nms, c('mother', 'mid', 'mat', 'mom'))
    selectInput("motherVar", "Mother variable", nms, selected = s)
  })

  output$fatherSelectUI <- renderUI({
    nms <- names(genealogyInput())
    s <- select_var(nms, c('father', 'fid', 'fat', 'dad'))
    selectInput("fatherVar", "Father variable", nms, selected = s)
  })

  output$sexSelectUI <- renderUI({
    nms <- names(genealogyInput())
    s <- select_var(nms, c('sex', 'gender', 'm/f'))
    selectInput("sexVar", "Sex variable", nms, selected = s)
  })

  output$livingdeadSelectUI <- renderUI({
    selectInput("livingdeadVar",
                "Living/dead variable",
                c('All living', names(genealogyInput())))
  })

  output$groupSelectUI <- renderUI({
    selectInput("groupVar", "Group variable", names(genealogyInput()))
  })

  guess_value <- function(clmn, options) {
    df <- genealogyInput()
    values <- unique(df[[clmn]])
    for (v in values) {
      if (tolower(v) %in% options)
        return(v)
    }
    return('')
  }

  output$femaleInputUI <- renderUI({
    v <- guess_value(input$sexVar, c('f', 'female', 'woman', 'fem'))
    textInput("femalevalue", "Female value", v)
  })

  output$maleInputUI <- renderUI({
    v <- guess_value(input$sexVar, c('m', 'male', 'man'))
    textInput("malevalue", "Male value", v)
  })

  output$missingInputUI <- renderUI({
    df <- genealogyInput()
    egos <- df[[input$egoVar]]
    fathers <- df[[input$fatherVar]]
    if (is.null(fathers)) v <- ''
    else if (0 %in% fathers & !(0 %in% egos)) v = 0
    else if (!max(fathers) %in% egos) v = max(fathers)
    else v <- ''
    textInput("missingvalue", "Missing value", v)
  })

  errors <- eventReactive(input$checkErrors, {
    error_df(
      genealogyInput(),
      input$egoVar,
      input$motherVar,
      input$fatherVar,
      input$sexVar,
      input$femalevalue,
      input$malevalue,
      input$missingvalue
    )
  })

  output$errors <- DT::renderDataTable(errors(),
                                       options = list(
                                         scrollY = '75vh',
                                         scrollX = TRUE,
                                         paging = FALSE
                                       ))


  # phi <- eventReactive(input$computePhi, {
  #   gen.phi(
  #     as.pedigree.GL(
  #       genealogyInput(),
  #       input$egoVar,
  #       input$motherVar,
  #       input$fatherVar,
  #       input$sexVar,
  #       input$femalevalue,
  #       input$malevalue,
  #       input$missingvalue
  #     )
  #   ) * 2
  # })

  phi <- eventReactive(input$computePhi, {
    kinship2::kinship(
      as.pedigree.K2(
        genealogyInput(),
        input$egoVar,
        input$fatherVar,
        input$motherVar,
        input$sexVar,
        input$malevalue,
        input$femalevalue,
        input$missingvalue
      )
    ) * 2
  })

  output$phi <- DT::renderDataTable(phi(),
                                    options = list(
                                      scrollY = '75vh',
                                      scrollX = TRUE,
                                      paging = FALSE
                                    ))

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

  grouprelatedness <- eventReactive(input$groupStats, {
    mean_group_relatedness(
      genealogyInput(),
      input$egoVar,
      input$fatherVar,
      input$motherVar,
      input$sexVar,
      input$malevalue,
      input$femalevalue,
      input$missingvalue,
      input$groupVar
    )
  })

  output$groupStats <- DT::renderDataTable(grouprelatedness(),
                                           options = list(
                                             scrollY = '75vh',
                                             scrollX = TRUE,
                                             paging = FALSE
                                           ))
}
