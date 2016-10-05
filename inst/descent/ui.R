
library(shiny)

navbarPage(
  title = div(img(src = "descentlogo.gif")),
  windowTitle = 'Descent',
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

               checkboxInput('header', 'Header', TRUE),

               radioButtons('sep', 'Separator',
                            c(
                              Comma = ',',
                              Semicolon = ';',
                              Tab = '\t'
                            ),
                            ','),

               # radioButtons(
               #   'quote',
               #   'Quote',
               #   c(
               #     None = '',
               #     'Double Quote' = '"',
               #     'Single Quote' = "'"
               #   ),
               #   '"'
               # ),

               htmlOutput("egoSelectUI"),
               htmlOutput("motherSelectUI"),
               htmlOutput("fatherSelectUI"),
               htmlOutput("sexSelectUI"),
               htmlOutput("livingdeadSelectUI"),

               htmlOutput("femaleInputUI"),
               htmlOutput("maleInputUI"),
               htmlOutput("missingInputUI")
               # textInput("missingvalue", "Missing value", "999")

             ),
             mainPanel(DT::dataTableOutput('contents'))
           )),
  tabPanel(
    "Errors",
    sidebarPanel(
      # checkboxGroupInput(
      #   "errorCheckGroup",
      #   label = "Check for errors",
      #   choices = list(
      #     "Incest" = 1,
      #     "Choice 2" = 2,
      #     "Choice 3" = 3
      #   ),
      #   selected = NULL
      # ),

      actionButton('checkErrors', 'Check errors')

    ),
    mainPanel(h3(textOutput('error_msg')),
              DT::dataTableOutput('errors'),
              h3(textOutput('warning_msg')),
              DT::dataTableOutput('warnings'))
  ),
  tabPanel(
    "Relatedness",
    verticalLayout(
      # DT::dataTableOutput('phi'),
      wellPanel(actionButton('computePhi', 'Compute relatedness'),
                downloadButton('downloadRelatednessMatrix', 'Download matrix')),
      plotOutput('phiHist')
    )
  ),
  tabPanel(
    "Groups",
    sidebarPanel(
      htmlOutput("groupSelectUI"),
      actionButton('groupStats', 'Group relatedness'),
      downloadButton('downloadGroupData', 'Download csv')),
    mainPanel(
      # DT::dataTableOutput('groupStatsTable')
      plotOutput('groupStatsPlot')
      )
  ),
  tabPanel(
    tags$button(
      id = 'quit',
      type = "button",
      class = "btn action-button",
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; padding:2px",
      onclick = "setTimeout(function(){window.close();},500);",  # close browser
      "Quit"
    )
  )
)
