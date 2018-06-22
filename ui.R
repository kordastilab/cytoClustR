
source("dependencies.R")


shinyUI(

  dashboardPage(

    ## Header
    skin="blue",
    dashboardHeader(
      title = "cytoClustR",
      titleWidth = 200
    ),## End header

    ## Sidebar
    dashboardSidebar(
      width = 200,
      sidebarMenu(
        id = "tabs",
        menuItem("Cytobank log in", tabName = "cytologin", icon = icon("sign-in")),
        menuItem("Sample tags", tabName = "sampleTags", icon = icon("server")),
        menuItem("Manual input mode", tabName = "mimode", icon=icon("file")),
        menuItem("Clustering", tabName = "hierclus", icon=icon("area-chart")),
        menuItem("Node identification", tabName = "postproc", icon=icon("eye")),
        menuItem("Node push", tabName = "nodeid", icon=icon("cloud-upload"))
        #menuItem("Contact us", tabName = "contact", icon=icon("envelope-o"))
      )
    ), ## End sidebar

    ## Main body
    dashboardBody(
      tabItems(

        ## Cytologin
        tabItem(tabName = "cytologin",
                div(class = "login",
                    uiOutput("uiLogin"),
                    br(),
                    textOutput("pass"),
                    tags$head(tags$style("#pass{color: red;
                                         font-size: 20px;
                                         font-style: italic;
                                         }"
                         ))
                    ),
                fluidPage(
                  fluidRow(
                    ## Include the line below so you can send messages
                    tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'))),
                    column(width=4,
                           uiOutput("cytoLogged")
                    )),


                  br(),
                  uiOutput("submitSpade_header"),
                  rHandsontableOutput("all_spades", width = "100%", height = "100%"),
                  br(),
                  uiOutput("submitSpade_btn"),
                  br(),
                  uiOutput("submitCytoGroups_header"),
                  rHandsontableOutput("cytoGroups", width = "100%", height = "100%"),
                  br(),
                  uiOutput("sampleTagsbtn"),
                  br(),
                  uiOutput("cytoOptions"),
                  uiOutput("submitCytoGroups_btn"),
                  br(),
                  uiOutput("markersToClusterCyto")
                )
        ), ## End Cytologin

        ## Sample tags
        tabItem("sampleTags",
                actionButton("submitSampleTags", "Push sample tags",
                             icon("cloud-upload"),
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                br(),
                br(),
                rHandsontableOutput("sampleTagstb", width = "100%", height = "100%")

        ), ## End Sample tags

        tabItem(tabName = "mimode",
                tabsetPanel(
                  tabPanel("Single-sample mode",
                           fluidRow(
                             column(3,
                                    h4(""),
                                    fileInput('FileInputSS', h4('Choose input file:'),
                                              accept=NULL),
                                    fileInput('columnCorrectionInputSS', h4('Marker cleaning file:'),
                                              accept=NULL)
                             ),
                             column(4, offset = 1,
                                    numericInput('cellCountSS', h4('Filter by cell count'), value = -1),
                                    numericInput('percentTotalSS', h4('Filter by percenttotal'), value = -1)
                             ),
                             column(4,
                                    selectInput('scalingMethodSS', h4('Data transformation'), c('None','arcsinh')),
                                    numericInput('coFactorSS', h4('co-factor'), value = 5)
                             )
                           ),
                           bootstrapPage(
                             div(style="display:inline-block",selectInput('columnTypeSS', h4('Select column type'), c('Raw medians','Medians', 'All'), width="200px")),
                             div(style="display:inline-block",selectInput('tSNEcolsSS', h4('Include tSNE'), c('No', 'Yes'), width="200px"))
                           ),
                           br(),
                           actionButton("submitSSparams", "Select markers",
                                        icon("columns"),
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           hr(),
                           uiOutput("markersToClusterSS")
                  ),
                  tabPanel("Multiple-sample mode",
                           fluidRow(
                             column(3,
                                    h4(""),
                                    fileInput('columnCorrectionInputMS', h4('Marker cleaning file:'),
                                              accept=NULL)
                             ),
                             column(4, offset = 1,
                                    numericInput('cellCountMS', h4('Filter by cell count'), value = -1),
                                    numericInput('percentTotalMS', h4('Filter by percenttotal'), value = -1)
                             ),
                             column(4,
                                    selectInput('scalingMethodMS', h4('Data transformation'), c('None','arcsinh')),
                                    numericInput('coFactorMS', h4('co-factor'), value = 5)
                             )
                           ),
                           bootstrapPage(
                             div(style="display:inline-block",selectInput('columnTypeMS', h4('Select column type'), c('Raw medians','Medians','All'), width="200px")),
                             div(style="display:inline-block",selectInput('tSNEcolsMS', h4('Include tSNE'), c('No', 'Yes'), width="200px"))
                           ),
                           br(),
                           numericInput("num_groups", "Change number of groups (resets the table)", value = 2, min=1, max = 10, step = 1),
                           h4("Please define your groups"),
                           h6("*Supply Output.Directory and click \"Clean and Save\" to export cleaned individual sample files"),
                           rHandsontableOutput("dataGroups_tbs", width = "100%", height = "100%"),
                           hr(),
                           actionButton("submitMSparams", "Select markers",
                                        icon("columns"),
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           actionButton("saveCleanMSGroups", "Clean and Save",
                                        icon("save"),
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           hr(),
                           uiOutput("markersToClusterMS")
                  )
                )
        ),


        ## Hierarchical clustering
        tabItem(tabName = "hierclus",
                tabsetPanel(
                  tabPanel("Main group",
                           br(),
                           fluidPage(
                             fluidRow(
                               column(width=6,
                                 box(title = "Heatmap configuration", status = "primary", solidHeader = TRUE,
                                     width = NULL,
                                     collapsible = TRUE,
                                     collapsed = TRUE,
                                     br(),
                                     textInput('mainHeatmapTitle', h4('Plot title:'), value = NULL),
                                     tags$div(align = 'left',
                                              class = 'multicol',
                                              uiOutput("markersToShow")),
                                     numericInput('mainHeatmapTitlefont', h4('Change title font size'), value = 16, step = 1),
                                     bootstrapPage(
                                       div(style="display:inline-block",numericInput('mainHeatmapYfont', h4('Y axis font size'), value = 8, step = 1)),
                                       div(style="display:inline-block",numericInput('mainHeatmapXfont', h4('X axis font size'), value = 12, step = 1))
                                     ),
                                     uiOutput("columnToSort"),
                                     uiOutput("columnToAnnotate"),
                                     actionButton("mainHeatmapParams", "Update plot",
                                                  icon("refresh"),
                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                            )))),
                            uiOutput("groupPlotSelect"),
                            bootstrapPage(
                              div(style="display:inline-block",downloadButton('downloadMainHeatmap', 'Download Plot')),
                              div(style="display:inline-block",downloadButton('downloadDataMainHeatmap', 'Download Data'))
                            ),
                            br(),
                            br(),
                            sliderInput("colorBreaks", labe="Number of color breaks", min = 10, max = 50, step = 5, value = 10),
                            plotOutput("mainHeatmap",height = "100%")
                  ),

                  tabPanel("Forced group",
                           #h4("** To use this function please make sure you have created the plot of the reference group in the Plot tab"),
                           br(),
                           fluidPage(
                             fluidRow(
                               column(width=6,
                                 box(title = "Heatmap configuration", status = "primary", solidHeader = TRUE,
                                     width = NULL,
                                     collapsible = TRUE,
                                     collapsed = TRUE,
                                     br(),
                                     textInput('forcedHeatmapTitle', h4('Plot title:'), value = NULL),
                                     numericInput('forcedHeatmapTitlefont', h4('Change title font size'), value = 16, step = 1),
                                     bootstrapPage(
                                       div(style="display:inline-block",numericInput('forcedHeatmapYfont', h4('Y axis font size'), value = 9, step = 1)),
                                       div(style="display:inline-block",numericInput('forcedHeatmapXfont', h4('X axis font size'), value = 12, step = 1))
                                    ),
                                    actionButton("forcedHeatmapParams", "Update plot",
                                                 icon("refresh"),
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                  )
                           ))),

                           h5("Select group to force heatmap"),
                           bootstrapPage(
                             div(style="display:inline-block",uiOutput("selectForcedGroup")),
                             div(style="display:inline-block",actionButton("forceHeatmap", "Force heatmap",icon("gg"),
                                                                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                           ),
                           br(),
                           br(),
                           bootstrapPage(
                             div(style="display:inline-block",downloadButton('downloadForcedHeatmap', 'Download Plot')),
                             div(style="display:inline-block",downloadButton('downloadDataForced', 'Download Data'))
                           ),
                           br(),
                           br(),
                           plotOutput("forcedHeatmap",height = "100%")

                  ),

                  tabPanel("Overlay groups",
                           br(),
                           fluidPage(
                             fluidRow(
                               column(width=6,
                                      box(title = "Heatmap configuration", status = "primary", solidHeader = TRUE,
                                          width = NULL,
                                          collapsible = TRUE,
                                          collapsed = TRUE,
                                          br(),
                                          textInput('overlayHeatmapTitle', h4('Plot title:'), value = NULL),
                                          numericInput('overlayHeatmapTitlefont', h4('Change title font size'), value = 16, step = 1),
                                          bootstrapPage(
                                            div(style="display:inline-block",numericInput('overlayHeatmapYfont', h4('Y axis font size'), value = 9, step = 1)),
                                            div(style="display:inline-block",numericInput('overlayHeatmapXfont', h4('X axis font size'), value = 12, step = 1))
                                          ),
                                          actionButton("overlayHeatmapParams", "Update plot",
                                                       icon("refresh"),
                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                      )))),
                           bootstrapPage(
                             div(style="display:inline-block",downloadButton('downloadOverlayHeatmap', 'Download Plot')),
                             div(style="display:inline-block",downloadButton('downloadDataOverlay', 'Download Data'))
                           ),
                           br(),
                           br(),
                           plotOutput("overlayHeatmap",height = "100%")
                  )
              )
          ), ## End of hierarchical clustering

        tabItem(tabName = "postproc",
                tabsetPanel(
                  tabPanel("Select Markers",
                           br(),
                           fluidPage(
                             fluidRow(
                               column(width=6,
                                      box(title = "Options", solidHeader = TRUE,
                                        width = NULL,
                                        status = "primary",
                                        collapsible = TRUE,
                                        collapsed = TRUE,
                                        br(),
                                        textInput("marker1",  h4("Marker 1")),
                                        textInput("marker2",  h4("Marker 2")),
                                        textInput("marker3",  h4("Marker 3")),
                                        br(),
                                        h4("Heatmap:"),
                                        textInput('postHeatmapTitle', h4('Plot title:'), value = NULL),
                                        numericInput('postHeatmapTitlefont', h4('Change title font size'), value = 16, step = 1),
                                        bootstrapPage(
                                          div(style="display:inline-block",numericInput('postHeatmapYfont', h4('Change Y axis font size'), value = 9, step = 1)),
                                          div(style="display:inline-block",numericInput('postHeatmapXfont', h4('Change X axis font size'), value = 12, step = 1)),
                                          div(style="display:inline-block",numericInput('postHeatmapRowTitlefont', h4('Change row title font size'), value = 8, step = 1)),
                                          div(style="display:inline-block",numericInput('postHeatmapSplitGap', h4('Change row gap (mm)'), value = 5, step = 1))
                                        ),
                                        br(),
                                        actionButton("postProc", "Go!",
                                                    icon("line-chart"),
                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                    )))),

                              downloadButton('downloadPostHeatmap', 'Download Plot'),
                              br(),
                              br(),
                              plotOutput("postHeatmap",height = "100%")
                )
              )
            ),

        tabItem("nodeid",
                actionButton("submitNodeGroups", "Push node groups",
                             icon("cloud-upload"),
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                br(),
                br(),
                rHandsontableOutput("nodeGroupstb", width = "100%", height = "100%")

        )



      )
    )## End main body

  )## End dashboard page


)
