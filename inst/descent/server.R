library(shiny)
library(ggrepel)
library(DT)
library(GENLIB)

source('descent.R')

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9 * 1024 ^ 2)

function(input, output) {

  # Not sure best place to define these functions
  showmenus <- function(){
    showTab(inputId = "tabs", target = "Summary")
    showTab(inputId = "tabs", target = "Kin")
    # showTab(inputId = "tabs", target = "Inbreeding")
    # showTab(inputId = "tabs", target = "Groups")
    showTab(inputId = "tabs", target = "Pedigree")
  }

  hidemenus <- function(){
    hideTab(inputId = "tabs", target = "Summary")
    hideTab(inputId = "tabs", target = "Kin")
    # hideTab(inputId = "tabs", target = "Inbreeding")
    # hideTab(inputId = "tabs", target = "Groups")
    hideTab(inputId = "tabs", target = "Pedigree")
  }

  hidemenus()

  observe({
    if (input$quit > 0)
      stopApp()  # stop shiny
  })


# Open data file and set params ----------------------------------------------------------

  fileInput <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$file1

    if (is.null(inFile)) return(NULL)

    hidemenus()

    readr::read_delim(
      inFile$datapath,
      delim = input$sep,
      quote = '',
      #input$quote,
      col_names = input$header
    )

  })

  genealogyInput <- reactive({

    withProgress(message = 'Loading file', value = 0, {

      incProgress(amount = 0.2, detail = 'Including all parents as egos')
      df <- complete_pedigree(fileInput(), gen_params())

      if (! validParams(gen_params(), df)) {
        return(NULL)
      }

      incProgress(amount = 0.2, detail = 'Adding matrilineages')
      if(! 'matrilineage' %in% names(df)){
        df$matrilineage <- lineage(df, gen_params(), type = 'matrilineage')
      }

      incProgress(amount = 0.2, detail = 'Adding patrilineages')
      if(! 'patrilineage' %in% names(df)){
        df$patrilineage <- lineage(df, gen_params(), type = 'patrilineage')
      }
    })

    return(df)
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
    nms <- names(fileInput())
    s <- select_var(nms, c('ego', 'id', 'ind', 'name'))
    selectInput("egoVar", "Ego variable", nms, selected = s)
  })

  output$motherSelectUI <- renderUI({
    nms <- names(fileInput())
    s <- select_var(nms, c('mother', 'mid', 'mat', 'mom'))
    selectInput("motherVar", "Mother variable", nms, selected = s)
  })

  output$fatherSelectUI <- renderUI({
    nms <- names(fileInput())
    s <- select_var(nms, c('father', 'fid', 'fat', 'dad'))
    selectInput("fatherVar", "Father variable", nms, selected = s)
  })

  output$sexSelectUI <- renderUI({
    nms <- names(fileInput())
    s <- select_var(nms, c('sex', 'gender', 'm/f'))
    selectInput("sexVar", "Sex variable", nms, selected = s)
  })

  guess_value <- function(clmn, options) {
    df <- fileInput()
    values <- unique(df[[clmn]])
    for (v in values) {
      if (tolower(v) %in% options)
        return(v)
    }
    return('')
  }

  guess_sex_value <- function(ego, parent, sex){
    df <- fileInput()
    ids <- intersect(df[[parent]], df[[ego]]) # Parents that are also egos
    if (length(ids) > 0){
      return(names(sort(table(df[[sex]][df[[ego]] %in% ids]),decreasing=T)[1])) # Most common value
    } else {
      return('')
    }
  }

  output$femaleInputUI <- renderUI({
    v <- guess_sex_value(input$egoVar, input$motherVar, input$sexVar)
    textInput("femalevalue", "Female value", v)
  })

  output$maleInputUI <- renderUI({
    v <- guess_sex_value(input$egoVar, input$fatherVar, input$sexVar)
    textInput("malevalue", "Male value", v)
  })

  output$livingdeadSelectUI <- renderUI({
    selectInput("livingdeadVar",
                "Living/dead variable",
                c('All living', names(fileInput())))
  })

  output$livingInputUI <- renderUI({
    v <- guess_value(input$livingdeadVar, c('alive', 'living', 'yes', '1'))
    textInput("livingvalue", "Living value", v)
  })

  output$deadInputUI <- renderUI({
    v <- guess_value(input$livingdeadVar, c('dead', 'deceased', 'no', '0'))
    textInput("deadvalue", "Dead value", v)
  })

  output$missingInputUI <- renderUI({
    # Guess missing value
    df <- fileInput()
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

  # validParams <- reactive({
  #   print('validParams')
  #   params <- gen_params()
  #   req_params <- c('ego', 'father', 'mother', 'sex', 'male', 'female', 'missing')
  #   if(sum(map_lgl(params[req_params], ~ !isTruthy(.)) > 0)) {
  #     return(F)
  #   }
  #   if(length(unique(params[req_params])) != length(req_params)) {
  #     return(F)
  #   }
  #   return(T)
  # })

  output$downloadGENLIB <- downloadHandler(
    filename = function() {
      'GENLIB.csv'
    },
    content = function(file) {
      write_ped(genealogyInput(), gen_params(), path=file)
    }
  )

# Errors ------------------------------------------------------------------

  errors <- reactive({
    df <- error_df(
      genealogyInput(),
      gen_params()
    )

    if (is.null(df)){
      showmenus()
    } else {
      hidemenus()
    }
    return(df)
  })


  warnings <- reactive({
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

  output$errors <- DT::renderDataTable(
    errors(),
    options = list(# scrollY = '75vh',
      scrollX = TRUE,
      paging = FALSE)
  )

  output$warning_msg <-
    renderText(if (is.null(warnings()))
      'No warnings'
      else
        'Rows with warnings')

  output$warnings <- DT::renderDataTable(
    warnings(),
    options = list(# scrollY = '75vh',
      scrollX = TRUE,
      paging = FALSE)
  )


# Summary -----------------------------------------------------------------

  summary <- reactive({
    summary_stats(
      genealogyInput(),
      gen_params()
    )
  })

  output$summaryStats <- renderText({
    stats <- summary()$stats
    txt <- mapply(function(x, i) paste(i, x, sep = ': '), stats, names(stats))
    paste0('\n', txt)
  })

  output$kindepth <- renderPlot({
    kd <- summary()$kd
    hist(kd$depth, xlab = 'Mean genealogical depth', main = '')
    # ggplot(kd, aes(depth)) + geom_histogram() + facet_wrap(~sex, ncol=1) + theme_bw()
  })

# Kinship coefficients ----------------------------------------------------

  # phi <- eventReactive(input$computePhi, {
  #   genphi2(
  #     as.genealogy.GL(
  #       genealogyInput(),
  #       gen_params = gen_params()
  #     ),
  #     genealogyInput(),
  #     gen_params()
  #   )
  # })

  phi <- reactive({
    genphi2(
      as.genealogy.GL(
        genealogyInput(),
        gen_params = gen_params()
      ),
      genealogyInput(),
      gen_params()
    )
  })

  # output$phi <- DT::renderDataTable(
  #   phi(),
  #   options = list(
  #     scrollY = '75vh',
  #     scrollX = TRUE,
  #     paging = FALSE
  #   )
  # )

  output$phiHist <- renderPlot({
    mtrx <- phi()
    mtrx <- mtrx[upper.tri(mtrx)]
    # hist(mtrx, main = 'Histogram of Kinship coefficients', xlab = 'Kinship coefficient')
    ggplot(data.frame(r = mtrx), aes(r)) +
      geom_histogram() +
      scale_x_sqrt() +
      scale_y_sqrt() +
      labs(title = 'Histogram of Kinship coefficients', x = '\nKinship coefficient (root scale)', y = 'Count (root scale)\n') +
      theme_bw()
  })

  output$downloadKinshipMatrix <- downloadHandler(
    filename = function() {
      'coefficients_of_kinship.csv'
    },
    content = function(file) {
      write.table(phi(), file, sep = ',', row.names = T)
    }
  )

  output$downloadRelatednessMatrix <- downloadHandler(
    filename = function() {
      'coefficients_of_relationship.csv'
    },
    content = function(file) {
      write.table(relatedness(phi()), file, sep = ',', row.names = T)
    }
  )


# Inbreeding coefficients -------------------------------------------------

  inbreeding <- reactive({
    genf2(
      as.genealogy.GL(
        genealogyInput(),
        gen_params = gen_params()
      ),
      genealogyInput(),
      gen_params()
    )
  })

  output$inbreeding <-
    DT::renderDataTable(
      tibble(
        Ego = names(inbreeding()),
        `Inbreeding coefficient` = inbreeding()
        ) %>%
        dplyr::filter(`Inbreeding coefficient` > 0),
      options = list(
        # scrollY = '75vh',
        scrollX = TRUE,
        paging = FALSE
      ),
      caption = 'Egos with positive inbreeding coefficients'
    )

  output$inbreedingHist <- renderPlot({
    inbreeding_coefs <- inbreeding()
    ggplot(data.frame(inbreeding = inbreeding_coefs), aes(inbreeding_coefs)) +
      geom_histogram() +
      # scale_x_sqrt() +
      scale_y_sqrt() +
      labs(title = 'Histogram of inbreeding coefficients', x = '\nInbreeding coefficient', y = 'Count (root scale)\n') +
      theme_bw()
  })

  output$downloadInbreeding <- downloadHandler(
    filename = function() {
      'inbreeding_coefficients.csv'
    },
    content = function(file) {
      df_inbreeding <- data.frame(ego = names(inbreeding()), inbreeding_coefficient = inbreeding())
      write.table(df_inbreeding, file, sep = ',', row.names = FALSE)
    }
  )


# Lineage size distributions ----------------------------------------------

  output$lineageDistribution <- renderPlot({
    ml <- table(genealogyInput()$matrilineage)
    pl <- table(genealogyInput()$patrilineage)
    df <- data.frame(
      size = c(ml, pl),
      type = c(rep('Matrilinage', length(ml)), rep('Patrilineage', length(pl)))
      )
    ggplot(df, aes(size)) +
      geom_histogram() +
      facet_wrap(~type, ncol = 1) +
      labs(title = 'Histogram of lineage sizes', x = '\nNumber of members', y = 'Count\n') +
      theme_bw()
  })

  # output$patrilineageDistribution <- renderPlot({
  #   df <- data.frame(patrilineage = c(table(genealogyInput()$patrilineage)))
  #   ggplot(df, aes(patrilineage)) +
  #     geom_histogram() +
  #     labs(title = 'Histogram of patrilineage sizes', x = '\nNumber of members', y = 'Count\n') +
  #     theme_bw()
  # })

# Group stats -------------------------------------------------------------

  grouprelatedness <- eventReactive(input$groupStats, {
    mean_group_kinship(
      phi(),
      genealogyInput(),
      gen_params(),
      input$groupVar,
      input$nosingles
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
    ggplot(df, aes(`Group size`, `Mean kinship`)) +
      geom_point() +
      scale_y_continuous(limits=c(0,NA)) +
      geom_text_repel(aes(label = `Group id`)) +
      labs(x = "\nGroup size", y = "Mean kinship coefficient\n") +
      theme_bw()
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

  # Pedigree plots -----------------------------------------------------------

    # Egos
  output$egoIdSelectUI <- renderUI({
    df <- genealogyInput()
    gp <- gen_params()

    if (is.null(gp$ego)) egos <- ''
    else egos <- sort(exclude_founders(df, gp))

    selectInput("selectedEgoVar", "Select ego (founders excluded)", egos)
  })

  ego_ped <- reactive({
    as.genealogy.GL(genealogyInput(), gen_params())
  })

  output$egoPedigreePlot <- renderPlot({
    gengraph2(ego_ped(), genealogyInput(), gen_params(), ego = as.numeric(input$selectedEgoVar))
  })

  # Matrilineages
  output$matrilineageIdSelectUI <- renderUI({
    df <- genealogyInput()
    selectInput("selectedMatVar", "Select matrilineage", lineage_ids(df$matrilineage))
  })

  output$matrilineagePedigreePlot <- renderPlot({
    df <- genealogyInput()
    gp <- gen_params()
    matrilineage <- as.numeric(input$selectedMatVar)
    pros <- lineage_members(df, gp, matrilineage, 'matrilineage', add_other_parent=T)
    gengraph2(ego_ped(), df, gp, ego = pros, ancestor = matrilineage)
  })

  # Patrilineages
  output$patrilineageIdSelectUI <- renderUI({
    df <- genealogyInput()
    selectInput("selectedPatVar", "Select patrilineage", lineage_ids(df$patrilineage))
  })

  output$patrilineagePedigreePlot <- renderPlot({
    df <- genealogyInput()
    gp <- gen_params()
    patrilineage <- as.numeric(input$selectedPatVar)
    pros <- lineage_members(df, gp, patrilineage, 'patrilineage', add_other_parent=T)
    gengraph2(ego_ped(), df, gp, ego = pros, ancestor = patrilineage)
  })
}


