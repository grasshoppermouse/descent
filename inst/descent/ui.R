
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
               htmlOutput("femaleInputUI"),
               htmlOutput("maleInputUI"),
               htmlOutput("livingdeadSelectUI"),
               htmlOutput("livingInputUI"),
               htmlOutput("deadInputUI"),
               htmlOutput("missingInputUI")

             ),
             mainPanel(DT::dataTableOutput('contents'))
           )),
  tabPanel(
    "Errors",
    verticalLayout(
      actionButton('checkErrors', 'Check errors'),
      h3(textOutput('error_msg')),
      DT::dataTableOutput('errors'),
      h3(textOutput('warning_msg')),
      DT::dataTableOutput('warnings')
    )
  ),
  tabPanel(
    "Summary",
    verticalLayout(
      actionButton('computeSummary', 'Summary stats'),
      tags$br(),
      verbatimTextOutput("summaryStats"),
      plotOutput("kindepth")
    )
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
    "Help",
    # includeHTML("help/index.html")
    includeMarkdown("www/help.md")
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
