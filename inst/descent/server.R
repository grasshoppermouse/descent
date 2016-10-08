library(shiny)
library(ggplot2)
library(ggrepel)
library(DT)
library(GENLIB)

source('descent.R')

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9 * 1024 ^ 2)

function(input, output, session) {
  observe({
    if (input$quit > 0)
      stopApp()  # stop shiny
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
      quote = '',
      #input$quote,
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

  output$livingdeadSelectUI <- renderUI({
    selectInput("livingdeadVar",
                "Living/dead variable",
                c('All living', names(genealogyInput())))
  })

  output$livingInputUI <- renderUI({
    v <-
      guess_value(input$livingdeadVar, c('alive', 'living', 'yes', '1'))
    textInput("livingvalue", "Living value", v)
  })

  output$deadInputUI <- renderUI({
    v <-
      guess_value(input$livingdeadVar, c('dead', 'deceased', 'no', '0'))
    textInput("deadvalue", "Dead value", v)
  })

  output$missingInputUI <- renderUI({
    df <- genealogyInput()
    egos <- df[[input$egoVar]]
    fathers <- df[[input$fatherVar]]
    if (is.null(fathers))
      v <- ''
    else if (0 %in% fathers & !(0 %in% egos))
      v = 0
    else if (!max(fathers) %in% egos)
      v = max(fathers)
    else
      v <- ''
    textInput("missingvalue", "Missing value", v)
  })

  output$groupSelectUI <- renderUI({
    selectInput("groupVar", "Group variable", names(genealogyInput()))
  })

  gen_params <- reactive({
    list(
      ego = input$egoVar,
      father = input$fatherVar,
      mother = input$motherVar,
      sex = input$sexVar,
      male = input$malevalue,
      female = input$femalevalue,
      livingdead = input$livingdeadVar,
      living = input$livingvalue,
      dead = input$deadvalue,
      missing = input$missingvalue
    )
  })

  errors <- eventReactive(input$checkErrors, {
    error_df(
      genealogyInput(),
      gen_params()
    )
  })

  warnings <- eventReactive(input$checkErrors, {
    warning_df(
      genealogyInput(),
      gen_params()
    )
  })

  output$error_msg <-
    renderText(if (is.null(errors()))
      'No errors'
      else
        'Rows with errors')

  output$errors <- DT::renderDataTable(errors(),
                                       options = list(# scrollY = '75vh',
                                         scrollX = TRUE,
                                         paging = FALSE))

  output$warning_msg <-
    renderText(if (is.null(warnings()))
      'No warnings'
      else
        'Rows with warnings')

  output$warnings <- DT::renderDataTable(warnings(),
                                         options = list(# scrollY = '75vh',
                                           scrollX = TRUE,
                                           paging = FALSE))

  summary <- eventReactive(input$computeSummary, {
    summary_stats(
      genealogyInput(),
      gen_params()
    )
  })

  output$summaryStats <- renderText({
    stats <- summary()$stats
    txt <-
      mapply(function(x, i)
        paste(i, x, sep = ': '), stats, names(stats))
    paste0('\n', txt)
  })

  output$kindepth <- renderPlot({
    kd <- summary()$kd
    x <- table(kd$depth)
    dotchart(c(rev(x)),
             main = 'Number of individuals in each generation (0 = founders)',
             xlab = 'Number of individuals')
  })


  # GENLIB phi doesn't return complete matrix for some reason
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
    ped <- as.pedigree.K2(
      genealogyInput(),
      gen_params()
    )
    if (is.null(ped))
      return(NULL)
    kinship2::kinship(ped) * 2
  })

  output$phi <- DT::renderDataTable(phi(),
                                    options = list(
                                      scrollY = '75vh',
                                      scrollX = TRUE,
                                      paging = FALSE
                                    ))

  output$phiHist <- renderPlot({
    mtrx <- phi()
    mtrx <- mtrx[upper.tri(mtrx)]
    # hist(mtrx, main = 'Histogram of relatedness values', xlab = 'Relatedness')
    ggplot(data.frame(r = mtrx), aes(r)) +
      geom_histogram() +
      scale_x_sqrt() +
      scale_y_sqrt() +
      labs(title = 'Histogram of relatedness values (axes on root-root scale)', x = 'Relatedness')
  })

  output$downloadRelatednessMatrix <- downloadHandler(
    filename = function() {
      'relatedness.csv'
    },
    content = function(file) {
      write.table(phi(), file, sep = ',',
                  row.names = FALSE)
    }
  )

  grouprelatedness <- eventReactive(input$groupStats, {
    mean_group_relatedness(
      genealogyInput(),
      gen_params(),
      input$groupVar
    )
  })

  output$groupStatsTable <- DT::renderDataTable(
    grouprelatedness(),
    options = list(
      scrollY = '75vh',
      scrollX = TRUE,
      paging = FALSE
    ),
    rownames = FALSE
  )

  output$groupStatsPlot <- renderPlot({
    df <- grouprelatedness()
    if (is.null(df))
      return(NULL)
    ggplot(df, aes(`Group size`, `Mean relatedness`)) +
      geom_point() +
      geom_text_repel(aes(label = `Group id`))
  })

  output$downloadGroupData <- downloadHandler(
    filename = function() {
      'group_relatedness.csv'
    },
    content = function(file) {
      write.table(grouprelatedness(),
                  file,
                  sep = ',',
                  row.names = FALSE)
    }
  )
}
