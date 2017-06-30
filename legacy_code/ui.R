source("dependencies.R")

dashboardPage(
  skin="blue",
  dashboardHeader(
    title = "cytoClusteR",
    titleWidth = 200
    ## I will do this content dynamic
    # dropdownMenu(type = "messages",
    #              messageItem(
    #                from = "Sales Dept",
    #                message = "Sales are steady this month."
    #              )
    # ),
    # dropdownMenu(type = "notifications",
    #              notificationItem(
    #                text = "5 new users today",
    #                icon("users"),
    #                status="warning"
    #              )
    # )
  ),
  
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = "tabs",
      menuItem("Cytobank log in", tabName = "cytologin", icon = icon("sign-in")),
      menuItem("Sample tags", tabName = "sampleTags", icon = icon("database")),
      menuItem("Manual input mode", tabName = "mimode", icon=icon("file")),
      menuItem("Clustering", tabName = "hierclus", icon=icon("area-chart")),
      #menuItem("Data", tabName = "data"),
      #menuItem("PCA", tabName = "pca"),
      menuItem("Post-processing", tabName = "postproc", icon=icon("fast-forward")),
      menuItem("Contact us", tabName = "contact")
    )
  ),
  
  dashboardBody(
    tabItems(
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
      ),
      tabItem("sampleTags",
              actionButton("submitSampleTags", "Push tags",
                           icon("save"), 
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              actionButton("resetSampleGroups", "Reset tags",
                           icon("refresh"), 
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              br(),
              rHandsontableOutput("sampleTagstb", width = "100%", height = "100%")
              
      ),
      tabItem(tabName = "mimode",
        tabsetPanel(
          tabPanel("Single-sample mode",
                   h2("Options"),
                   fluidRow(
                     column(3,
                            h4(""),
                            fileInput('FileInputSS', h4('Choose input file:'),
                                      accept=NULL),
                            fileInput('columnCorrectionInputSS', h4('Marker cleaning file:'),
                                      accept=NULL)
                     ),
                     column(4, offset = 1,
                            numericInput('cellCountSS', h4('Filter by cell count'), value = 0),
                            numericInput('percentTotalSS', h4('Filter by percenttotal'), value = 0)
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
                   uiOutput("markersToClusterSS"),
                   hr(),
                   actionButton("submitSSmode", "Go!",
                                icon("line-chart"), 
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          tabPanel("Multiple-sample mode",
                   h2("Options"),
                   fluidRow(
                     column(3,
                            h4(""),
                            fileInput('columnCorrectionInputMS', h4('Marker cleaning file:'),
                                      accept=NULL)
                     ),
                     column(4, offset = 1,
                            numericInput('cellCountMS', h4('Filter by cell count'), value = 0),
                            numericInput('percentTotalMS', h4('Filter by percenttotal'), value = 0)
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
                   numericInput("num_groups", "Number of groups", value = 2, min=1),
                   actionButton("updateMStable", "Reset Group Table",
                                icon("refresh"), 
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                   
                   hr(),
                   h2("Please define your groups"),
                   h6("*Supply Output.Directory and click \"Clean and Save\" to export cleaned individual sample files"),
                   rHandsontableOutput("dataGroups_tbs", width = "100%", height = "100%"),
                   hr(),
                   actionButton("submitMSparams", "Select markers",
                                icon("columns"), 
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                   actionButton("saveCleanGroups", "Clean and Save",
                                icon("save"), 
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                   hr(),
                   uiOutput("markersToClusterMS"),
                   hr(),
                   actionButton("submitMSmode", "Go!",
                                icon("line-chart"), 
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        )
      ),
      
      tabItem(tabName = "hierclus",
              tabsetPanel(
                tabPanel("Plot",
                         
                         sidebarPanel(
                           sliderInput("obs",
                                       "Number of observations:",
                                       min = 0,
                                       max = 1000,
                                       value = 500)
                         ),
                         mainPanel(
                           br(),
                           uiOutput("groupPlotSelect"),
                           downloadButton('downloadHeatmap1', 'Download Plot'),
                           br(),
                           br(),
                           plotOutput("heatmapHC",height = "100%")
                         )
 
                ),
                tabPanel("Data",
                         br(),
                         downloadButton('downloadData', 'Download Data'),
                         hr(),
                         DT::dataTableOutput("dataToShow")
                ),
                tabPanel("Define Clusters",
                         hr(),
                         numericInput('hc_clust', "Select number of clusters", value = 2, step = 1, min=1),
                         actionButton("hc_clust_button", "Update clusters",
                                      icon("refresh"), 
                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                         hr(),
                         ## For automatic cluster detection uncomment the following line
                         #rHandsontableOutput("hcclusters", width = "100%", height = "100%")
                         rHandsontableOutput("hcclusters_tbs", width = "100%", height = "100%"),
                         hr(),
                         actionButton("submitClusters", "Analyze Clusters",
                                      icon("line-chart"), 
                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                         hr(),
                         hr(),
                         DT::dataTableOutput("cluster_zscores")
                ),
                tabPanel("Forced heatmap",
                         h4("** To use this function please make sure you have created the plot of the reference group in the first tab"),
                         h5("Select group to force heatmap"),
                         uiOutput("selectForcedGroup"),
                         actionButton("forceHeatmap", "Force heatmap",icon("gg"), 
                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                         downloadButton('downloadForcedHeatmap', 'Download Plot'),
                         br(),
                         br(),
                         plotOutput("forcedHeatmap",height = "100%")
                         
                ),
                tabPanel("Forced Heatmap Data",
                         br(),
                         downloadButton('downloadDataForced', 'Download Data'),
                         hr(),
                         DT::dataTableOutput("dataToShowForced")
                ),
                tabPanel("Superimpose heatmaps",
                         h4("** Calculate the fold change between the reference heatmap and the forced heatmap")
                ),
                tabPanel("MEM"
                  
                ),
                tabPanel("Plot configuration",
                         br(),
                         fluidRow(
                           box(title = "Heatmap:", status = "primary", solidHeader = TRUE,
                               collapsible = TRUE,
                               textInput('heatmap1Title', h4('Plot title:'), value = NULL),
                               # bootstrapPage(
                               #   div(style="display:inline-block",numericInput(inputId="heatmap1Width", label="Heatmap width", value = 1024)),
                               #   div(style="display:inline-block",numericInput(inputId="heatmap1Height", label="Heatmap height", value = 700))
                               # ),
                               tags$div(align = 'left', 
                                        class = 'multicol',
                                        uiOutput("markersToShow")),
                               numericInput('heatmap1Titlefont', h4('Change title font size'), value = 16, step = 1),
                               bootstrapPage(
                                 div(style="display:inline-block",numericInput('heatmap1Yfont', h4('Change Y axis font size'), value = 9, step = 1)),
                                 div(style="display:inline-block",numericInput('heatmap1Xfont', h4('Change X axis font size'), value = 12, step = 1))
                               ),
                               numericInput('minimumIntensity', 'Minimum intensity', value = -10)
                           ),
                           box(title = "Dendrogram:", status = "primary", solidHeader = TRUE,
                               collapsible = TRUE,
                               selectInput('dendroBoolean', h4('Show dendrogram'), choices = c("Yes", "No"), selected = "Yes"),
                               selectInput('boxBoolean', h4('Show boxplots'), choices = c("Yes", "No"), selected = "No")
                           ),
                           br(),
                           box(title = "Annotation:", status = "primary", solidHeader = TRUE,
                               collapsible = TRUE,
                               uiOutput("columnToAnnotate")
                           ),
                           br(),
                           actionButton("HCPlotparams", "Update plot",
                                        icon("refresh"), 
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                           
                         )
                )
              )
      ),
      tabItem(tabName = "postproc",
              tabsetPanel(
                tabPanel("Select Markers",
                         fluidRow(
                           column(width=9,
                                  box(width = NULL, height = 1300, solidHeader = TRUE,
                                      title = "Plot",
                                      br(),
                                      downloadButton('downloadHeatmap2', 'Download Plot'),
                                      br(),
                                      br(),
                                      plotOutput("heatmap2",height = "100%")
                                    )
                           ),
                           column(width = 3,
                                  box(width = NULL, status = "warning",
                                      title="Options",
                                      textInput("marker1",  h4("Marker 1")),
                                      textInput("marker2",  h4("Marker 2")),
                                      textInput("marker3",  h4("Marker 3")),
                                      br(),
                                      h4("Heatmap:"),
                                      h5("Note: When z-score >= 1 then level is set to High(H),
                                         when z-score > -1 and < 1 then level is around mean (M) and 
                                         when z-score <= -1 then level is set to Low(L)"),
                                      textInput('heatmap2Title', h4('Plot title:'), value = NULL),
                                      numericInput('heatmap2Titlefont', h4('Change title font size'), value = 16, step = 1),
                                      bootstrapPage(
                                        div(style="display:inline-block",numericInput('heatmap2Yfont', h4('Change Y axis font size'), value = 9, step = 1)),
                                        div(style="display:inline-block",numericInput('heatmap2Xfont', h4('Change X axis font size'), value = 12, step = 1))
                                      ),
                                      br(),
                                      actionButton("postProc", "Go!",
                                                   icon("line-chart"), 
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                  )
                           )
                         )
                ),
                tabPanel("Data",
                         br(),
                         downloadButton('downloadDataPostProcessing', 'Download Data'),
                         hr(),
                         DT::dataTableOutput("dataToShowPostProc")
                )
              )
          )
              
      
    )
  )
)


