


library(shiny)

shinyUI(navbarPage(
  title = div(img(src = "descentlogo.gif")),
  tabPanel("File",
           sidebarLayout(
             sidebarPanel(
               fileInput(
                 'file1',
                 'Choose text file',
                 accept = c(
                   'text/csv',
                   'text/comma-separated-values',
                   'text/tab-separated-values',
                   'text/plain',
                   '.csv',
                   '.tsv'
                 )
               ),
               tags$hr(),
               # checkboxInput('header', 'Header', TRUE),
               radioButtons('sep', 'Separator',
                            c(
                              Comma = ',',
                              Semicolon = ';',
                              Tab = '\t'
                            ),
                            ','),
               radioButtons(
                 'quote',
                 'Quote',
                 c(
                   None = '',
                   'Double Quote' = '"',
                   'Single Quote' = "'"
                 ),
                 '"'
               ),
               htmlOutput("egoSelectUI"),
               htmlOutput("motherSelectUI"),
               htmlOutput("fatherSelectUI"),
               htmlOutput("sexSelectUI"),
               htmlOutput("livingdeadSelectUI"),

               textInput("femalevalue", "Female value", "F"),
               textInput("malevalue", "Male value", "M"),
               textInput("missingvalue", "Missing value", "999")

             ),
             mainPanel(DT::dataTableOutput('contents'))
           )),
  tabPanel(
    "Errors",
    sidebarPanel(
      checkboxGroupInput(
        "errorCheckGroup",
        label = "Check for errors",
        choices = list(
          "Incest" = 1,
          "Choice 2" = 2,
          "Choice 3" = 3
        ),
        selected = NULL
      ),

      actionButton('checkErrors', 'Check errors')
    ),
    mainPanel(DT::dataTableOutput('errors'))
  ),
  tabPanel(
    "Relatedness",
    verticalLayout(
      DT::dataTableOutput('phi'),
      wellPanel(actionButton('computePhi', 'Compute relatedness'),
                downloadButton('downloadData', 'Download'))
    )
  ),

  # sidebarPanel(
  #
  #   actionButton('computePhi', 'Compute relatedness')
  # ),
  # mainPanel(
  #   DT::dataTableOutput('phi')
  # )
  # ),
  tabPanel("Lineages")
))
