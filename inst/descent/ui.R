


library(shiny)
require(rmarkdown)

navbarPage(
  title = div(img(src = "descentlogo.gif")),
  id = 'tabs',
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

               htmlOutput("egoSelectUI"),
               htmlOutput("motherSelectUI"),
               htmlOutput("fatherSelectUI"),
               htmlOutput("sexSelectUI"),
               htmlOutput("femaleInputUI"),
               htmlOutput("maleInputUI"),
               htmlOutput("missingInputUI"),
               htmlOutput("livingdeadSelectUI"),
               htmlOutput("livingInputUI"),
               htmlOutput("deadInputUI")

             ),
             mainPanel(
               conditionalPanel(
                 condition = 'input.egoVar',
                 downloadButton('downloadGENLIB', 'Download GENLIB format file')
               ),
               DT::dataTableOutput('contents')
             )
           )),
  tabPanel(
    "Errors",
    verticalLayout(
      h3(textOutput('error_msg')),
      DT::dataTableOutput('errors'),
      h3(textOutput('warning_msg')),
      DT::dataTableOutput('warnings')
    )
  ),
  tabPanel(
    "Summary",
    verticalLayout(
      # actionButton('computeSummary', 'Summary stats'),
      tags$br(),
      verbatimTextOutput("summaryStats"),
      plotOutput("kindepth")
    )
  ),
  navbarMenu(
    'Kin',
    tabPanel(
      "Kinship coefficients",
      verticalLayout(
        # actionButton('computePhi', 'Compute kinship'),
        conditionalPanel(
          condition = 'output.phiHist',
          br(),
          downloadButton('downloadKinshipMatrix', 'Download kinship matrix'),
          downloadButton('downloadRelatednessMatrix', 'Download relatedness matrix'),
          br(),
          hr()
        ),
        plotOutput('phiHist')
      )
    ),
    tabPanel(
      "Inbreeding coefficients",
      verticalLayout(
        wellPanel(
          # actionButton('computeInbreeding', 'Compute inbreeding'),
          downloadButton('downloadInbreeding', 'Download inbreeding coefficients')
        ),
        plotOutput('inbreedingHist'),
        # h3('Egos with positive coefficients'),
        DT::dataTableOutput('inbreeding')
      )
    ),
    tabPanel("Mean group kinship",
             verticalLayout(
               wellPanel(
                 fluidRow(
                   column(6, htmlOutput("groupSelectUI")),
                   column(3, actionButton('groupStats', 'Group kinship')),
                   column(3, downloadButton('downloadGroupData', 'Download csv')),
                   tags$style(type = 'text/css', "#groupStats { width:100%; margin-top: 25px;}"),
                   tags$style(type = 'text/css', "#downloadGroupData { width:100%; margin-top: 25px;}")
                 ),
                 checkboxInput('nosingles', 'Omit groups with one person', T)
               ),
               # DT::dataTableOutput('groupStatsTable')
               plotOutput('groupStatsPlot')
             )),
    tabPanel("Lineages",
             plotOutput('lineageDistribution'))
  ),
  navbarMenu('Pedigree',
             tabPanel("Egos",
                      verticalLayout(
                        wellPanel(fluidRow(
                          column(4, htmlOutput("egoIdSelectUI")),
                          # column(6, actionButton('plotPedigree', 'Plot ego pedigree')),
                          tags$style(type = 'text/css', "#plotEgoPedigree { width:100%; margin-top: 25px;}")
                        )),
                        plotOutput('egoPedigreePlot')
                      )),
             tabPanel("Matrilineages",
                      verticalLayout(
                        wellPanel(fluidRow(
                          column(4, htmlOutput("matrilineageIdSelectUI")),
                          # column(6, actionButton('plotMatPedigree', 'Plot matrilineage pedigree')),
                          tags$style(type = 'text/css', "#plotMatPedigree { width:100%; margin-top: 25px;}")
                        )),
                        plotOutput('matrilineagePedigreePlot')
                      )),
             tabPanel("Patrilineages",
                      verticalLayout(
                        wellPanel(fluidRow(
                          column(4, htmlOutput("patrilineageIdSelectUI")),
                          # column(6, actionButton('plotPatPedigree', 'Plot patrilineage pedigree')),
                          tags$style(type = 'text/css', "#plotPatPedigree { width:100%; margin-top: 25px;}")
                        )),
                        plotOutput('patrilineagePedigreePlot')
                      ))
             ),
  tabPanel("Help",
           # includeHTML("help/index.html")
           includeMarkdown("www/help.md")),
  tabPanel(
    tags$button(
      id = 'quit',
      type = "button",
      class = "btn action-button",
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; padding:2px",
      onclick = "setTimeout(function(){window.close();},500);",
      # close browser
      "Quit"
    )
  )
)
