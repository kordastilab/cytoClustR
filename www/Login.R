#### Log in module ###
USER <- reactiveValues(Logged = Logged)#, pass=NULL)

shinyjs::useShinyjs()

fieldsMandatory <- c("userName", "passwd", "userSite")
fieldsMandatoryToken <- c("cytoToken", "userSite")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"

passwdInput <- function(inputId, label) {
  tagList(
    tags$label(label),
    br(),
    tags$input(id = inputId, type="password", value="", width="250px")
  )
}

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    fluidRow(
      shinyjs::useShinyjs(),
      shinyjs::inlineCSS(appCSS),
      column(width = 6,
        #wellPanel(
          h4(strong("Please enter your Cytobank credential")),
          textInput("userName", labelMandatory("Username:"), width = "250px", value=""),
          br(),
          passwordInput("passwd", labelMandatory("Password:"), width="250px", value=""),
          hr(),
          h5("or alternatively paste an authentication token below"),
          textAreaInput("cytoToken", "Authentication token", "", width = "350px", height = "100px"),
          br(),
          textInput("userSite", labelMandatory("Cytobank site:"), width = "250px", value=""),
          hr(),
          actionButton("Login", "Log in", class = "btn-primary", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          br(),
          br(),
          h5("1) Log in to your cytobank account using username, password and site"),
          h5("2) Alternatively you can provide only authentication token and site")
        #)
      )
    )
  }
})

## In here I have to check if authentication in cytobank is successful
output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if (input$Login) {
        ## Insert here Cytobank's authentication
        ## and check the cytobank session return
        if(input$userName!="" & input$passwd!=""){
          t = try(authenticate(site=input$userSite, username = input$userName, password = input$passwd))
        }else if(input$userName=="" & input$passwd=="" & input$cytoToken!=""){
          t = try(authenticate(site=input$userSite, auth_token=input$cytoToken))
        }
        
        if (is(t,"try-error")) {
          "Username or password failed!"
        } else  {
          USER$Logged <- TRUE
          #USER$cyto_session <- t
          USER$userName = input$userName
          USER$passwd = input$passwd
          USER$cytoToken = input$cytoToken
          USER$userSite = input$userSite
        }
      } 
    }
  }
})

