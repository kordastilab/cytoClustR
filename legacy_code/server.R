 source("dependencies.R")

Logged = FALSE

shinyServer(function(input, output, session) {
  source("www/Login.R",  local = TRUE)
  v <- reactiveValues(fileInput=NULL,
                      fileInputName=NULL,
                      columnCorrectionInput=NULL,
                      initialData=NULL,
                      columnType=NULL,
                      cellCount=NULL,
                      percentTotal=NULL,
                      scalingMethod=NULL,
                      coFactor=NULL,
                      tSNECols=NULL,
                      annotationColumn=NULL,
                      heatmap1Title=NULL,
                      heatmap1Titlefont=16,
                      heatmap1Yfont=9,
                      heatmap1Xfont=12,
                      minimumIntensity = -10,
                      dendroBoolean="Yes",
                      boxBoolean="No",
                      hc_clust=2,
                      silc=NULL,
                      num_groups=2,
                      dataGroups_tbs = NULL,
                      dataGroups_tb = NULL,
                      hcclusters_tbs = NULL,
                      hcclusters_tb = NULL,
                      clusterColumns=NULL,
                      msData = NULL,
                      msgroups = NULL,
                      msGroupSelected = NULL,
                      msDataCombined = NULL,
                      dataToPlot = NULL,
                      cluster_zscores=NULL,
                      all_zscores=NULL,
                      marker1 = NULL,
                      marker2 = NULL,
                      marker3 = NULL,
                      heatmap2Title = NULL,
                      heatmap2Titlefont = 16,
                      heatmap2Yfont = 9,
                      heatmap2Xfont = 12,
                      dataPostProc=NULL,
                      mainHeatRowOrder=NULL,
                      mainHeatColumnOrder=NULL,
                      forcedGroup=NULL,
                      dataToPlot_forced=NULL,
                      exp_id=NULL,
                      all_spades=NULL,
                      sampleTags=NULL,
                      cytoGroups=NULL,
                      columnTypeCyto=NULL,
                      cellCountCyto=NULL,
                      percentTotalCyto=NULL,
                      scalingMethodCyto=NULL,
                      coFactorCyto=NULL,
                      tSNEColsCyto=NULL,
                      cyto_session = NULL,
                      experiments=NULL)

    ## Activate log in button given a condition
    observe({
      # check if all mandatory fields have a value

      ## First for username and password
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)

      ## And then check also the token
      if(mandatoryFilled==FALSE){
        mandatoryFilled <-
          vapply(fieldsMandatoryToken,
                 function(x) {
                   !is.null(input[[x]]) && input[[x]] != ""
                 },
                 logical(1))
        mandatoryFilled <- all(mandatoryFilled)
      }

      # enable/disable the submit button
      shinyjs::toggleState(id = "Login", condition = mandatoryFilled)
  })

  # Cytobank login
  output$cytoLogged <- renderUI({
    if (USER$Logged == TRUE) {
      ## Here I should put the UI of the application if the login to cytobank is successful
      if(input$userName!="" & input$passwd!=""){
        v$cyto_session = authenticate(site=input$userSite, username = input$userName, password = input$passwd)
      }else if(input$userName=="" & input$passwd=="" & input$cytoToken!=""){
        v$cyto_session = authenticate(site=input$userSite, auth_token=input$cytoToken)
      }
      v$experiments = experiments.list(v$cyto_session)
      list(
          selectInput('experiment', strong('Select an experiment'), width="250px", choices=c("None",unique(v$experiments$experimentName)), selected = "None"),
          actionButton("submitExperiment", "Get analyses",
                        icon("cloud-download"),
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        )
    }else{
      NULL
    }
  })


  ## When you get the experiment
  ## Present to the user a table with the analyses of the experiment
  ## Alert below will trigger if the experiment has no SPADE analyses
  observeEvent(input$submitExperiment,{
    if(USER$Logged == TRUE)
    {
      if(!is.null(v$experiments))
      {
        if(input$experiment !="None")
        {
          v$exp_id = unlist(v$experiments$id[v$experiments$experimentName==input$experiment])
          ## Get sample tags and store the tags in a variable
          ## Here create a temporary directory to save the sample_tags
          temp_dir = "~/.cytocluster_temp"
          dir.create(temp_dir)
          t = try(sample_tags.download(v$cyto_session, experiment_id = v$exp_id, directory = temp_dir))
          if (is(t,"try-error")) {
            my_spade_check_test <- "No sample tags found for this experiment!"
            js_string <- 'alert("SOMETHING");'
            js_string <- sub("SOMETHING",my_spade_check_test,js_string)
            session$sendCustomMessage(type='jsCode', list(value = js_string))
          }else{
            ## Read in sample tags
            v$sampleTags = read.table(paste0(temp_dir,"/experiment_",v$exp_id,"_annotations.tsv"), header = T, quote = "", sep="\t")
          }

          ## Clean the directory
          unlink(temp_dir, recursive = T)

          # ## Get the list of spade analyses
          t = try(spade.list(v$cyto_session, experiment_id = v$exp_id))
          if (is(t,"try-error")) {
            v$all_spades = NULL
            my_spade_check_test <- "No SPADE analysis found for this experiment!"
            js_string <- 'alert("SOMETHING");'
            js_string <- sub("SOMETHING",my_spade_check_test,js_string)
            session$sendCustomMessage(type='jsCode', list(value = js_string))
          }else{
            v$all_spades = t
            v$all_spades$Pick = FALSE
          }
        }
      }
    }
  })

  output$all_spades = renderRHandsontable({
    if(is.null(v$all_spades)){
      return()
    }else{
      rhandsontable(v$all_spades) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }
  })

  output$submitSpade_header <- renderUI({
    if(is.null(v$all_spades)){
      return()
    }else{
      h4(strong("Select SPADE analysis"))
    }
  })

  output$submitSpade_btn <- renderUI({
    if(is.null(v$all_spades)){
      return()
    }else{
      actionButton("submitSpade", "Get statistics tables",
                   icon("cloud-download"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })

  ## After selecting a spade analysis get the files and their tag
  observeEvent(input$submitSpade, {

    ## Print a message to the user
    my_spade_check_test <- "This may take a couple of minutes. Please wait..."
    js_string <- 'alert("SOMETHING");'
    js_string <- sub("SOMETHING",my_spade_check_test,js_string)
    session$sendCustomMessage(type='jsCode', list(value = js_string))

    ## Get the spade object
    spade_selected = hot_to_r(input$all_spades) %>% subset(Pick==TRUE)
    spade_idx = spade_selected$id[1]
    spade_name = spade_selected$name[1]
    cyto_spade = spade.show(v$cyto_session, experiment_id = v$exp_id, spade_id = spade_idx)
    temp_dir = "~/.cytocluster_temp"
    dir.create(temp_dir)
    ## Download the statistics tables
    spade.download_statistics_tables(v$cyto_session, spade=cyto_spade, directory = temp_dir)
    old_path = getwd()
    setwd(temp_dir)
    system(paste0("unzip ", temp_dir, "/", spade_name, ".zip_statistics.zip"))
    setwd(old_path)

    fns = list.files(paste0(temp_dir, "/bySample"))
    groups = data.frame(filename=fns)
    groups$Individuals = apply(groups, 1, function(x) str_extract(x[1], paste(unique(v$sampleTags$Individuals), collapse="|")))
    groups = groups %>% left_join(v$sampleTags%>%select(Individuals, Conditions)%>%unique)

    ## For each group make a directory with the corresponding files
    ## Put a with progress here possibly
    for(g in unique(groups$Conditions)){
      g_dir = paste0(temp_dir, "/", g)
      dir.create(g_dir)
      g_fns = groups %>% subset(Conditions==g) %>% .$filename
      file.copy(paste0(temp_dir,"/bySample/",g_fns), g_dir)
      g_add = data.frame(Pick=TRUE,
                         Input.Directory=g_dir,
                         Name=g,
                         Files=length(g_fns),
                         Output.Directory="",
                         stringsAsFactors = FALSE)
      v$cytoGroups = rbind(v$cytoGroups, g_add)
    }
  })

  ## Render UI here to show the table with the groups and the files
  output$cytoGroups = renderRHandsontable({
    if(is.null(v$cytoGroups)){
      return()
    }else{
      rhandsontable(v$cytoGroups) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }
  })

  output$submitCytoGroups_header <- renderUI({
    if(is.null(v$cytoGroups)){
      return()
    }else{
      h4(strong("Select groups to analyse"))
    }
  })

  output$sampleTagstb = renderRHandsontable({
    if(is.null(v$cytoGroups)){
      return()
    }else{
      rhandsontable(data.frame(file=c("abdc", "ahfgt"), group=c("None", "None"))) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }
  })

  output$sampleTagsbtn <- renderUI({
    if(is.null(v$cytoGroups)){
      return()
    }else{
      actionButton("sampleTagsbtn", "Sample tags",
                   icon("database"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })

  output$submitCytoGroups_btn <- renderUI({
    if(is.null(v$cytoGroups)){
      return()
    }else{
      list(
        actionButton("submitCytoGroups", "Select markers",
                     icon("columns"),
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        actionButton("saveCleanCytoGroups", "Clean and Save",
                     icon("save"),
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      )
    }
  })

  observeEvent(input$sampleTagsbtn,{
    ## Change tab to hier clustering
    updateTabItems(session, "tabs", selected = "sampleTags")
  })

  ## Options
  output$cytoOptions <- renderUI({
    if(is.null(v$cytoGroups)){
      return()
    }else{
      list(
        h4(strong("Options")),
        fileInput('columnCorrectionInputCyto', h4('Marker cleaning file:'),
                  accept=NULL),
        fluidRow(
          column(4,
                 numericInput('cellCountCyto', h4('Filter by cell count'), value = 0),
                 numericInput('percentTotalCyto', h4('Filter by percenttotal'), value = 0)
          ),
          column(4,
                 selectInput('scalingMethodCyto', h4('Data transformation'), c('None','arcsinh')),
                 numericInput('coFactorCyto', h4('co-factor'), value = 5)
          ),
          column(4,
                 selectInput('columnTypeCyto', h4('Select column type'), c('Raw medians','Medians','All')),
                 selectInput('tSNEcolsCyto', h4('Include tSNE'), c('No', 'Yes'))
          )
        )
      )
    }
  })

  observeEvent(input$submitCytoGroups, {
    v$dataGroups_tb <- hot_to_r(input$cytoGroups)
    v$columnCorrectionInput = input$columnCorrectionInputCyto
    v$columnType = input$columnTypeCyto
    v$cellCount = input$cellCountCyto
    v$percentTotal = input$percentTotalCyto
    v$scalingMethod = input$scalingMethodCyto
    v$coFactor = input$coFactorCyto
    v$tSNECols = input$tSNEcolsCyto

    group_tb = NULL
    if(is.null(v$dataGroups_tb)){
      v$initialData = NULL
      v$msgroups = NULL
    }else{
      dtb = v$dataGroups_tb
      dtb = dtb %>% subset(Pick==TRUE)
      if(sum(dtb$Input.Directory=="None")==nrow(dtb)){
        v$initialData = NULL
        v$msgroups = NULL
      }else{
        dtb = dtb %>% subset(Input.Directory!="None")
        for(i in 1:nrow(dtb)){
          pattern="csv|xlsx|xls"
          p = dtb$Input.Directory[i]
          g = dtb$Name[i]
          g = dtb$Name[i]
          if(g=="None"){
            g=paste0(dtb$Group[i], "_cleaned")
          }
          s = dtb$Output.Directory[i]

          fns = list.files(path = p, pattern = pattern)
          fns = fns[grepl(pattern = pattern, x = fns)]

          withProgress(message = 'Reading data ', value = 0, {

            y = length(fns)

            for (fn in fns){
              splits = unlist(strsplit(fn, "\\."))
              ext = splits[length(splits)]
              if (ext=="xlsx" | ext=="xls"){
                d = read.xlsx(paste0(p, "/", fn), 1)
              }else if (ext=="csv"){
                d = read.table(paste0(p, "/", fn), sep=",", header = T)
              }

              ## exclude raw_medians if you select the medians column
              if(v$columnTypeCyto=="Medians"){
                kept_cols = c("ID", "count", colnames(d)[grep("medians", colnames(d))], "percenttotal")
                kept_cols = kept_cols[!grepl("raw_median", kept_cols)]
                kept_cols = data.frame(col=kept_cols) %>%
                  mutate(num=extract_numeric(col)) %>%
                  mutate(final_col=ifelse(is.na(num), col,
                                          ifelse(grepl("tSNE1", col), "tSNE1_clust",
                                                 ifelse(grepl("tSNE2", col), "tSNE2_clust", num))))
              }else if (v$columnTypeCyto=="Raw medians"){
                kept_cols = c("ID", "count", colnames(d)[grep("raw_median", colnames(d))], "percenttotal")
                kept_cols = data.frame(col=kept_cols) %>%
                  mutate(num=extract_numeric(col)) %>%
                  mutate(final_col=ifelse(is.na(num), col,
                                          ifelse(grepl("tSNE1", col), "tSNE1_clust",
                                                 ifelse(grepl("tSNE2", col), "tSNE2_clust", num))))
              }


              ## Check nameconversion file
              if (is.null(v$columnCorrectionInputG)){
                ## Sometimes the user may supply a file with names already fixed
                ## Check that and return the file as it is if it is the case
                if (length(colnames(d)[grep("medians", colnames(d))])==0 & length(colnames(d)[grep("raw_median", colnames(d))])==0){
                  d = d
                }else{
                  d = d %>% select(one_of(kept_cols$col))
                }
              }else if (!is.null(v$columnCorrectionInputG)){
                n = read.xlsx(v$columnCorrectionInputG$datapath, 1)[,1:2]
                colnames(n) = c("row", "marker")
                n = n %>% mutate(row=as.character(row))
                kept_cols = kept_cols %>%
                  left_join(n, by=c("final_col"="row"))
                ids = c("ID", "count", "percenttotal")
                kept_cols = kept_cols %>%
                  mutate(marker=ifelse(col%in%ids, col,
                                       ifelse(grepl("tSNE1", col), "tSNE1_clust",
                                              ifelse(grepl("tSNE2", col), "tSNE2_clust", marker))))
                kept_cols = kept_cols %>% subset(!is.na(marker))
                d = d %>% select(one_of(kept_cols$col))
                colnames(d) = kept_cols$marker
              }

              ## Take in other parameters as well
              #log2(d/co-factor + sqrt(1 + (d/co-factor)^2) )
              if (v$scalingMethodCyto=="arcsinh"){
                ds = d %>% dplyr::select(-ID, -count, -percenttotal)
                dns = d %>% dplyr::select(ID, count, percenttotal)
                ds = as.matrix(ds)
                ds = apply(ds, 2,function(x) log(x/as.numeric(input$coFactorCyto) + sqrt(1 + (x/as.numeric(input$coFactorCyto))^2) ))
                d = cbind(dns%>%dplyr::select(ID,count),ds, dns%>%dplyr::select(percenttotal))
              }else{
                d=d
              }

              ## Filters on cellCount and percentTotal
              if("count"%in%colnames(d)){
                d = d %>% subset(count > v$cellCountCyto)
              }
              if("percenttotal"%in%colnames(d)){
                d = d %>% subset(percenttotal > v$percentTotalCyto)
              }

              d = d %>% mutate(fn=fn, group=g)

              if(v$tSNEColsCyto=="No"){
                if(length(grep("tSNE|tsne", colnames(d)))==0){
                  d = d
                }else{
                  d = d[, -grep("tSNE|tsne", colnames(d))]
                }
              }else{
                d = d %>% data.frame()
              }

              group_tb = rbind(group_tb, d)

              # Increment the progress bar, and update the detail text.
              incProgress(1/y, detail = paste(g,": ", y, " files"))

              # Pause for 0.1 seconds to simulate a long computation.
              Sys.sleep(0.1)
            }

          })
        }
      }

      v$msData = group_tb %>% data.frame()
      v$msgroups = unique(group_tb$group)
    }
  })

  ## Get the data for each group
  observeEvent(input$saveCleanGroups, {
    v$dataGroups_tb <- hot_to_r(input$dataGroups_tbs)
    v$columnCorrectionInputG = input$columnCorrectionInputG
    v$columnTypeG = input$columnTypeG
    v$cellCountG = input$cellCountG
    v$percentTotalG = input$percentTotalG
    v$scalingMethodG = input$scalingMethodG
    v$coFactorG = input$coFactorG
    v$tSNEColsG = input$tSNEcolsG

    if(!is.null(v$dataGroups_tb)){
      ## Work on the input table to define the number of groups
      dtb = v$dataGroups_tb
      dtb = dtb %>% subset(Pick==TRUE)

      if(sum(dtb$Input.Directory=="None")<nrow(dtb)){
        dtb = dtb %>% subset(Input.Directory!="None")
        for(i in 1:nrow(dtb)){
          pattern="csv|xlsx|xls"
          p = dtb$Input.Directory[i]
          g = dtb$Name[i]
          s = dtb$Output.Directory[i]
          if(s!="None"){
            fns = list.files(path = p, pattern = pattern)
            fns = fns[grepl(pattern = pattern, x = fns)]

            withProgress(message = 'Cleaning and saving files ', value = 0, {

              y = length(fns)
              for (fn in fns){
                splits = unlist(strsplit(fn, "\\."))
                ext = splits[length(splits)]
                if (ext=="xlsx" | ext=="xls"){
                  d = read.xlsx(paste0(p, "/", fn), 1)
                }else if (ext=="csv"){
                  d = read.table(paste0(p, "/", fn), sep=",", header = T)
                }

                ## exclude raw_medians if you select the medians column
                if(v$columnTypeG=="Medians"){
                  kept_cols = c("ID", "count", colnames(d)[grep("medians", colnames(d))], "percenttotal")
                  kept_cols = kept_cols[!grepl("raw_median", kept_cols)]
                  kept_cols = data.frame(col=kept_cols) %>%
                    mutate(num=extract_numeric(col)) %>%
                    mutate(final_col=ifelse(is.na(num), col,
                                            ifelse(grepl("tSNE1", col), "tSNE1_clust",
                                                   ifelse(grepl("tSNE2", col), "tSNE2_clust", num))))
                }else if (v$columnTypeG=="Raw medians"){
                  kept_cols = c("ID", "count", colnames(d)[grep("raw_median", colnames(d))], "percenttotal")
                  kept_cols = data.frame(col=kept_cols) %>%
                    mutate(num=extract_numeric(col)) %>%
                    mutate(final_col=ifelse(is.na(num), col,
                                            ifelse(grepl("tSNE1", col), "tSNE1_clust",
                                                   ifelse(grepl("tSNE2", col), "tSNE2_clust", num))))
                }


                ## Check nameconversion file
                if (is.null(v$columnCorrectionInputG)){
                  ## Sometimes the user may supply a file with names already fixed
                  ## Check that and return the file as it is if it is the case
                  if (length(colnames(d)[grep("medians", colnames(d))])==0 & length(colnames(d)[grep("raw_median", colnames(d))])==0){
                    d = d
                  }else{
                    d = d %>% select(one_of(kept_cols$col))
                  }
                }else if (!is.null(v$columnCorrectionInputG)){
                  n = read.xlsx(v$columnCorrectionInputG$datapath, 1)[,1:2]
                  colnames(n) = c("row", "marker")
                  n = n %>% mutate(row=as.character(row))
                  kept_cols = kept_cols %>%
                    left_join(n, by=c("final_col"="row"))
                  ids = c("ID", "count", "percenttotal")
                  kept_cols = kept_cols %>%
                    mutate(marker=ifelse(col%in%ids, col,
                                         ifelse(grepl("tSNE1", col), "tSNE1_clust",
                                                ifelse(grepl("tSNE2", col), "tSNE2_clust", marker))))
                  kept_cols = kept_cols %>% subset(!is.na(marker))
                  d = d %>% select(one_of(kept_cols$col))
                  colnames(d) = kept_cols$marker
                }

                if(v$tSNEColsG=="No"){
                  if(length(grep("tSNE|tsne", colnames(d)))==0){
                    d = d
                  }else{
                    d = d[, -grep("tSNE|tsne", colnames(d))]
                  }
                }else{
                  d = d %>% data.frame()
                }

                d = d %>% data.frame()

                ## Take in other parameters as well
                #log2(d/co-factor + sqrt(1 + (d/co-factor)^2) )
                if (v$scalingMethodG=="arcsinh"){
                  ds = d %>% dplyr::select(-ID, -count, -percenttotal)
                  dns = d %>% dplyr::select(ID, count, percenttotal)
                  ds = as.matrix(ds)
                  ds = apply(ds, 2,function(x) log(x/as.numeric(input$coFactor) + sqrt(1 + (x/as.numeric(input$coFactor))^2) ))
                  d = cbind(dns%>%dplyr::select(ID,count),ds, dns%>%dplyr::select(percenttotal))
                }else{
                  d=d
                }

                ## Filters on cellCount and percentTotal
                if("count"%in%colnames(d)){
                  d = d %>% subset(count > v$cellCountG)
                }
                if("percenttotal"%in%colnames(d)){
                  d = d %>% subset(percenttotal > v$percentTotalG)
                }

                #save_dir=paste0(s, "/", g)
                save_dir=s
                if (!is.null(s)){
                  if (!file.exists(save_dir)){
                    dir.create(file.path(save_dir))
                  }
                  write.table(d, file=paste0(save_dir, "/", fn, "_cleaned.csv"), quote = F, row.names = F, sep = ",")
                }

                # Increment the progress bar, and update the detail text.
                incProgress(1/y, detail = paste("Saving ", g,": ", y, " files"))

                # Pause for 0.1 seconds to simulate a long computation.
                Sys.sleep(0.1)

              }
            })
          }else{
            next
          }
        }
      }
    }
  })

  observe({
    if(is.null(v$ssfileInput)){
      updateTextInput(session, "heatmap1Title", value = NULL)
    }
    else{
      updateTextInput(session, "heatmap1Title", value = v$ssfileInputName)
    }
  })

  # Markers to cluster
  output$markersToCluster <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(v$ssData)){
      return()
    }else{
      # Get the data set with the appropriate name
      cnames <- colnames(v$ssData)
      cnames <- cnames[cnames!="ID"]
      cnames <- cnames[cnames!="count"]
      cnames <- cnames[cnames!="percenttotal"]
      # Create the checkboxes and select them all by default
      wellPanel(
        h2("Select Markers to cluster"),
        checkboxGroupInput("markersTC",h4(""),
                           choices  = cnames,
                           selected = cnames,
                           inline = TRUE),
        bootstrapPage(
          div(style="display:inline-block",actionButton("UncheckAll", "Uncheck all",
                                                        icon("square-o"),
                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
          div(style="display:inline-block",actionButton("CheckAll", "Check All",
                                                        icon("check-square-o"),
                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        )
      )
    }
  })

  ## buttons for selecting columns to cluster
  observeEvent(input$UncheckAll,{
    cnames <- colnames(v$ssData)
    cnames <- cnames[cnames!="ID"]
    cnames <- cnames[cnames!="count"]
    cnames <- cnames[cnames!="percenttotal"]
    updateCheckboxGroupInput(session=session, inputId="markersTC",
                             choices=cnames,
                             selected=NULL,
                             inline = TRUE)
  })

  observeEvent(input$CheckAll,{
    cnames <- colnames(v$ssData)
    cnames <- cnames[cnames!="ID"]
    cnames <- cnames[cnames!="count"]
    cnames <- cnames[cnames!="percenttotal"]
    updateCheckboxGroupInput(session=session, inputId="markersTC",
                             choices=cnames,
                             selected=cnames,
                             inline = TRUE)
  })

  output$markersToClusterCyto <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(v$msData))
      return()

    # Get the data set with the appropriate name
    cnames <- colnames(v$msData)
    cnames <- cnames[cnames!="ID"]
    cnames <- cnames[cnames!="count"]
    cnames <- cnames[cnames!="percenttotal"]
    cnames <- cnames[cnames!="fn"]
    cnames <- cnames[cnames!="group"]
    # Create the checkboxes and select them all by default
    list(
      wellPanel(
        h2("Select Markers to cluster"),
        checkboxGroupInput("markersTCCyto",h4(""),
                           choices  = cnames,
                           selected = cnames,
                           inline = TRUE),
        bootstrapPage(
          div(style="display:inline-block",actionButton("UncheckAllCyto", "Uncheck all",
                                                        icon("square-o"),
                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
          div(style="display:inline-block",actionButton("CheckAllCyto", "Check All",
                                                        icon("check-square-o"),
                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        )
      ),
      br(),
      actionButton("submitCytoMode", "Go!",
                   icon("line-chart"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  })

  observeEvent(input$UncheckAllCyto,{
    cnames <- colnames(v$msData)
    cnames <- cnames[cnames!="ID"]
    cnames <- cnames[cnames!="count"]
    cnames <- cnames[cnames!="percenttotal"]
    cnames <- cnames[cnames!="fn"]
    cnames <- cnames[cnames!="group"]
    updateCheckboxGroupInput(session=session, inputId="markersTCG",
                             choices=cnames,
                             selected=NULL,
                             inline = TRUE)
  })

  observeEvent(input$CheckAllCyto,{
    cnames <- colnames(v$msData)
    cnames <- cnames[cnames!="ID"]
    cnames <- cnames[cnames!="count"]
    cnames <- cnames[cnames!="percenttotal"]
    cnames <- cnames[cnames!="fn"]
    cnames <- cnames[cnames!="group"]
    updateCheckboxGroupInput(session=session, inputId="markersTCG",
                             choices=cnames,
                             selected=cnames,
                             inline = TRUE)
  })

  observeEvent(input$submitCytoMode, {
    if(is.null(v$msData)){
      v$clusterColumnsG = NULL
    }else{
      cc <- as.vector(input$markersTCCyto)
      v$clusterColumns = cc
      ## Change tab to hier clustering
      updateTabItems(session, "tabs", selected = "hierclus")
    }
  })

  output$groupPlotSelect <- renderUI({
    if(!is.null(v$msData)){
      bootstrapPage(
        div(style="display:inline-block",selectInput('msPlot', '', width="250px", choices=c("None",unique(v$msData$group)), selected = "None")),
        div(style="display:inline-block",      actionButton("msGroupSelect", "Generate plot",
                                                            icon("line-chart"),
                                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
      )
    }else{
      return()
    }
  })

  ## Check which group is selected by the user
  observeEvent(input$msGroupSelect, {
    v$msGroupSelected = input$msPlot
    ## Group to get the overall medians
    v$dataToPlot = v$msData %>% subset(group==v$msGroupSelected) %>%
      select(-group, -fn) %>% group_by(ID) %>% summarise_each(funs(median)) %>% ungroup
    v$dataToPlot = v$dataToPlot %>% dplyr::select(ID, count, one_of(v$clusterColumns), percenttotal)
  })

  output$dataToShow = DT::renderDataTable({
    if (is.null(v$dataToPlot)){
      return()
    }else{
      datatable(v$dataToPlot, rownames = FALSE, options = list(pageLength = 75, scrollX = TRUE))
    }
  })

  ## ----------------------------------------
  ##                  Heatmap
  ## ----------------------------------------

  output$columnToAnnotate <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(v$dataToPlot)){
      return()
    }else{
      cnames <- colnames(v$dataToPlot)
      cnames <- cnames[cnames!="ID"]
      #       cnames <- cnames[cnames!="count"]
      #       cnames <- cnames[cnames!="percenttotal"]
      #       cnames <- cnames[!(cnames%in%colnames(datasetToCluster()))]
      # Create the checkboxes and select them all by default
      if(length(cnames)>0){
        selectInput('annotation', h4('Select column'), choices=c("None",cnames), selected = "None")
      }else{
        "No annotation column found"
      }
    }
  })


  observeEvent(input$HCPlotparams, {
    v$annotationColumn = input$annotation
    v$heatmap1Title = input$heatmap1Title
    v$heatmap1Titlefont = input$heatmap1Titlefont
    v$heatmap1Yfont = input$heatmap1Yfont
    v$heatmap1Xfont = input$heatmap1Xfont
    v$minimumIntensity = input$minimumIntensity
    v$dendroBoolean = input$dendroBoolean
    v$boxBoolean = input$boxBoolean
  })

  plotHC <- reactive({
    if(is.null(v$dataToPlot)){
      return(NULL)
    }else{
      df <- v$dataToPlot
      x = as.matrix(df%>%dplyr::select(-ID, -count, -percenttotal)%>%data.frame())
      rownames(x) = df$ID

      ## Color palette (color range of heatmap)
      #heat_palette <- colorRampPalette(c("black", "yellow", "#FAF7C9"))
      #pairs.breaks <- c(seq(0, max(df), by=0.1))
      #pairs.breaks <- seq(0,max(df),by = 0.1)
      if(v$dendroBoolean=="Yes"){
        if((!is.null(v$dataToPlot)) & is.null(v$annotationColumn) ){
          h1 = Heatmap(x,colorRamp2(breaks=c(min(x),0.25,max(x)),colors = c("black", "yellow", "red")),
                       row_names_gp = gpar(fontsize = v$heatmap1Yfont),
                       column_names_gp = gpar(fontsize = v$heatmap1Xfont),
                       column_title = paste0("SPADE node medians", "\n",v$heatmap1Title),
                       column_title_gp = gpar(fontsize = v$heatmap1Titlefont),
                       row_dend_width = unit(2, "cm"),
                       column_dend_height = unit(2, "cm"),
                       heatmap_legend_param = list(title = "Intensity",
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)))

          if(v$boxBoolean=="Yes"){
            ha_boxplot = HeatmapAnnotation(boxplot = anno_boxplot(x, which = "row"),
                                           which = "row", width = unit(2, "cm"))
            draw(ha_boxplot + h1, row_dend_side = "left", row_sub_title_side = "right")
          }else if (v$boxBoolean=="No"){
            draw(h1)
          }
          v$mainHeatRowOrder = unlist(row_order(h1))
          v$mainHeatColumnOrder = column_order(h1)
        }else if( !is.null(v$dataToPlot)  & v$annotationColumn=="None" ){
          h1 = Heatmap(x,colorRamp2(breaks=c(min(x),0.25,max(x)),colors = c("black", "yellow", "red")),
                       row_names_gp = gpar(fontsize = v$heatmap1Yfont),
                       column_names_gp = gpar(fontsize = v$heatmap1Xfont),
                       column_title = paste0("SPADE node medians", "\n",v$heatmap1Title),
                       column_title_gp = gpar(fontsize = v$heatmap1Titlefont),
                       row_dend_width = unit(2, "cm"),
                       column_dend_height = unit(2, "cm"),
                       heatmap_legend_param = list(title = "Intensity",
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)))
          if(v$boxBoolean=="Yes"){
            ha_boxplot = HeatmapAnnotation(boxplot = anno_boxplot(x, which = "row"),
                                           which = "row", width = unit(2, "cm"))
            draw(ha_boxplot + h1, row_dend_side = "left", row_sub_title_side = "right")
          }else if (v$boxBoolean=="No"){
            draw(h1)
          }
          v$mainHeatRowOrder = unlist(row_order(h1))
          v$mainHeatColumnOrder = column_order(h1)
        }else if( !is.null(v$dataToPlot)  & !is.null(v$annotationColumn) & v$annotationColumn!="None" ){

          h1 = Heatmap(x,colorRamp2(breaks=c(min(x),0.25,max(x)),colors = c("black", "yellow", "red")),
                       row_names_gp = gpar(fontsize = v$heatmap1Yfont),
                       column_names_gp = gpar(fontsize = v$heatmap1Xfont),
                       column_title = paste0("SPADE node medians", "\n",v$heatmap1Title),
                       column_title_gp = gpar(fontsize = v$heatmap1Titlefont),
                       row_dend_width = unit(2, "cm"),
                       column_dend_height = unit(2, "cm"),
                       heatmap_legend_param = list(title = "Intensity",
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)))

          h2 = Heatmap(v$dataToPlot[,v$annotationColumn], name = v$annotationColumn,
                       colorRamp2(c(0,max(v$dataToPlot[,v$annotationColumn])), c("blue","red")),
                       heatmap_legend_param = list(title = v$annotationColumn,
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)),
                       width = unit(6, "mm"))
          if(v$boxBoolean=="Yes"){
            ha_boxplot = HeatmapAnnotation(boxplot = anno_boxplot(x, which = "row"),
                                           which = "row", width = unit(2, "cm"))
            draw(ha_boxplot + h1 + h2, row_dend_side = "left", row_sub_title_side = "right")
          }else if (v$boxBoolean=="No"){
            draw(h1 + h2)
          }
          v$mainHeatRowOrder = unlist(row_order(h1))
          v$mainHeatColumnOrder = column_order(h1)
        }
      }else{
        if( !is.null(v$dataToPlot) & is.null(v$annotationColumn) ){
          h1 = Heatmap(x,colorRamp2(breaks=c(min(x),0.25,max(x)),colors = c("black", "yellow", "red")),
                       row_names_gp = gpar(fontsize = v$heatmap1Yfont),
                       column_names_gp = gpar(fontsize = v$heatmap1Xfont),
                       column_title = paste0("SPADE node medians", "\n",v$heatmap1Title),
                       column_title_gp = gpar(fontsize = v$heatmap1Titlefont),
                       row_dend_width = unit(2, "cm"),
                       column_dend_height = unit(2, "cm"),
                       show_row_dend=FALSE,
                       show_column_dend=FALSE,
                       heatmap_legend_param = list(title = "Intensity",
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)))

          draw(h1)
          v$mainHeatRowOrder = unlist(row_order(h1))
          v$mainHeatColumnOrder = column_order(h1)
        }else if( !is.null(v$dataToPlot)  & v$annotationColumn=="None"){
          h1 = Heatmap(x,colorRamp2(breaks=c(min(x),0.25,max(x)),colors = c("black", "yellow", "red")),
                       row_names_gp = gpar(fontsize = v$heatmap1Yfont),
                       column_names_gp = gpar(fontsize = v$heatmap1Xfont),
                       column_title = paste0("SPADE node medians", "\n",v$heatmap1Title),
                       column_title_gp = gpar(fontsize = v$heatmap1Titlefont),
                       row_dend_width = unit(2, "cm"),
                       column_dend_height = unit(2, "cm"),
                       show_row_dend=FALSE,
                       show_column_dend=FALSE,
                       heatmap_legend_param = list(title = "Intensity",
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)))
          draw(h1)
          v$mainHeatRowOrder = unlist(row_order(h1))
          v$mainHeatColumnOrder = column_order(h1)
        }else if( !is.null(v$dataToPlot) & !is.null(v$annotationColumn) & v$annotationColumn!="None" ){
          h1 = Heatmap(x,colorRamp2(breaks=c(min(x),0.25,max(x)),colors = c("black", "yellow", "red")),
                       row_names_gp = gpar(fontsize = v$heatmap1Yfont),
                       column_names_gp = gpar(fontsize = v$heatmap1Xfont),
                       column_title = paste0("SPADE node medians", "\n",v$heatmap1Title),
                       column_title_gp = gpar(fontsize = v$heatmap1Titlefont),
                       row_dend_width = unit(2, "cm"),
                       column_dend_height = unit(2, "cm"),
                       show_row_dend=FALSE,
                       show_column_dend=FALSE,
                       heatmap_legend_param = list(title = "Intensity",
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)))
          h2 = Heatmap(v$dataToPlot[,v$annotationColumn], name = v$annotationColumn,
                       colorRamp2(c(0,max(v$dataToPlot[,v$annotationColumn])), c("blue","red")),
                       heatmap_legend_param = list(title = v$annotationColumn,
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)),
                       width = unit(6, "mm"))
          draw(h1 + h2)
          v$mainHeatRowOrder = unlist(row_order(h1))
          v$mainHeatColumnOrder = column_order(h1)
        }
      }
    }
  })

  output$heatmapHC <- renderPlot({
    plotHC()
  },height = 1020)

  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data_clustering', Sys.Date(), '.tsv', sep='')
    },
    content = function(con) {
      write.table(v$dataToPlot, file=con, quote = F, row.names = F, sep = "\t")
    }
  )

  output$downloadHeatmap1 <- downloadHandler(
    filename = function() {
      paste('heatmap_clustering', Sys.Date(), '.pdf', sep='')
    },

    content = function(con) {
      pdf(con)
      draw(plotHC())
      dev.off()
    }
  )

  ## Exploration of optimal number of clusters
  ## http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
  output$plotwss = renderPlot({
    if(!is.null(v$dataToPlot)){
      d = v$dataToPlot
      wss <- (nrow(d)-1)*sum(apply(d,2,var))
      for (i in 2:15) wss[i] <- sum(kmeans(d,
                                           centers=i)$withinss)
      plot(1:15, wss, type="b", xlab="Number of Clusters",
           ylab="Within groups sum of squares")
    }
  })

  observeEvent(input$silc_button, {
    v$silc = input$silc
  })

  output$plotsil = renderPlot({
    if(!is.null(v$dataToPlot)){
      d = v$dataToPlot
      if(is.null(v$silc)){
        pamk.best <- pamk(d, krange = 2:20)
        plot(silhouette(pam(d, pamk.best$nc)))
      }else{
        plot(silhouette(pam(d, v$silc)))
      }
    }
  })

  observeEvent(input$hc_clust_button, {
    v$hc_clust = input$hc_clust
  })

  hcclusters_tbs = reactive({
    v$hcclusters_tbs = data.frame(Group = 1:v$hc_clust,
                                  IDs=rep("None",v$hc_clust),
                                  stringsAsFactors = FALSE)
  })


  output$hcclusters_tbs = renderRHandsontable({
    if(is.null(hcclusters_tbs())){
      return()
    }else{
      rhandsontable(hcclusters_tbs()) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }
  })

  observeEvent(input$submitClusters, {
    v$hcclusters_tb <- hot_to_r(input$hcclusters_tbs)

    ## Analyze clusters here
    ## Calculate z-score
    v$cluster_zscores = v$hcclusters_tb %>% select(Group, IDs) %>%
      mutate(IDs=strsplit(IDs, ",")) %>% unnest()
    d = v$dataToPlot
    rnames = d$ID
    d = d %>% select(-ID) %>% data.frame()
    rownames(d) = rnames
    d = data.frame(lapply(d, function(x) (x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)))
    d = d %>% tibble::rownames_to_column() %>% rename(ID=rowname)
    v$cluster_zscores = v$cluster_zscores %>% rename(ID=IDs) %>% left_join(d)

    clusters2IDs = v$cluster_zscores %>% select(Group, ID) %>% group_by(Group) %>% summarise(IDs=paste(ID, collapse=","))
    v$cluster_zscores = v$cluster_zscores %>% select(-ID)
    v$cluster_zscores = v$cluster_zscores %>% group_by(Group) %>% summarise_each(funs(mean)) %>% ungroup() %>% data.frame()
    v$cluster_zscores = v$cluster_zscores %>% gather(marker, value, -Group) %>% spread(Group, value)
  })

  output$cluster_zscores = DT::renderDataTable({
    if(is.null(v$cluster_zscores)) return()
    datatable(v$cluster_zscores,  rownames = FALSE, options = list(pageLength = 75, scrollX = TRUE))
  })

  hcclusters <- reactive({
    if(is.null(v$dataToPlot)){
      return(NULL)
    }else{
      d = v$dataToPlot
      #dd <- dist(scale(d))
      dd <- dist(d, method = "euclidean")
      hc <- hclust(dd)
      cl = cutree(hc, k = v$hc_clust)
      clusters = data.frame(table(cl, d$ID)) %>% subset(Freq==1)
      clusters = clusters %>% group_by(cl) %>% summarise(nodes=paste(Var2, collapse=",")) %>%
        ungroup() %>% mutate(Pick=TRUE)
      return(clusters)
    }
  })

  output$hcclusters = renderRHandsontable({
    if(is.null(hcclusters())){
      return()
    }else{
      rhandsontable(hcclusters()) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }
  })


  ######################
  ##  Post-processing
  ######################

  observeEvent(input$postProc, {
    v$marker1 = input$marker1
    v$marker2 = input$marker2
    v$marker3 = input$marker3
    v$heatmap2Title = input$heatmap2Title
    v$heatmap2Titlefont = input$heatmap2Titlefont
    v$heatmap2Yfont = input$heatmap2Yfont
    v$heatmap2Xfont = input$heatmap2Xfont

    if(!is.null(v$dataToPlot)  &
       (v$marker1!="" | v$marker2!="" | v$marker3!="") &
       (!is.null(v$marker1) | !is.null(v$marker2) | !is.null(v$marker3)) ){

        #d = v$dataToPlot %>% data.frame()
        d = v$dataToPlot
        rnames = d$ID
        d = d %>% select(-ID) %>% data.frame()
        rownames(d) = rnames
        d = data.frame(lapply(d, function(x){
          if (sd(x)==0){
            rep(0,length(x))
          }else{
            (x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
          }
        }))
        d = d %>% tibble::rownames_to_column() %>% rename(ID=rowname)
        #d = d %>% dplyr::select(-count, -percenttotal)
        ## For testing
        #print(d%>%head)

        marker1 = v$marker1
        marker2 = v$marker2
        marker3 = v$marker3

      if (tolower(marker1)!="" & tolower(marker1)%in%tolower(colnames(d))){
        colIndex = match(tolower(marker1), tolower(colnames(d)))
        #q1 = quantile(d[,colIndex])[2]
        #q3 = quantile(d[,colIndex])[4]
        d$marker1 = NA
        d$marker1[which(d[,colIndex]<= -1)] = paste0(colnames(d)[colIndex],"(L)")
        #d$marker1[which(d[,colIndex]<=q1)] = "L"
        d$marker1[which(d[,colIndex]> -1 & d[,colIndex]< 1)] = paste0(colnames(d)[colIndex],"(M)")
        #d$marker1[which(d[,colIndex]>q1 & d[,colIndex]<=q3)] = "M"
        d$marker1[which(d[,colIndex]>= 1)] = paste0(colnames(d)[colIndex],"(H)")
        #d$marker1[which(d[,colIndex]>q3)] = "H"
      }

      if (tolower(marker2)!="" & tolower(marker2)%in%tolower(colnames(d))){
        colIndex = match(tolower(marker2), tolower(colnames(d)))
        #q1 = quantile(d[,colIndex])[2]
        #q3 = quantile(d[,colIndex])[4]
        d$marker2 = NA
        d$marker2[which(d[,colIndex]<= -1)] = paste0(colnames(d)[colIndex],"(L)")
        #d$marker2[which(d[,colIndex]<=q1)] = "L"
        d$marker2[which(d[,colIndex]> -1 & d[,colIndex]< 1)] = paste0(colnames(d)[colIndex],"(M)")
        #d$marker2[which(d[,colIndex]>q1 & d[,colIndex]<=q3)] = "M"
        d$marker2[which(d[,colIndex]>= 1)] = paste0(colnames(d)[colIndex],"(H)")
        #d$marker2[which(d[,colIndex]>q3)] = "H"
      }

      if (tolower(marker3)!="" & tolower(marker3)%in%tolower(colnames(d))){
        colIndex = match(tolower(marker3), tolower(colnames(d)))
        #q1 = quantile(d[,colIndex])[2]
        #q3 = quantile(d[,colIndex])[4]
        d$marker3 = NA
        d$marker3[which(d[,colIndex]<= -1)] = paste0(colnames(d)[colIndex],"(L)")
        #d$marker3[which(d[,colIndex]<=q1)] = "L"
        d$marker3[which(d[,colIndex]> -1 & d[,colIndex]< 1)] = paste0(colnames(d)[colIndex],"(M)")
        #d$marker3[which(d[,colIndex]>q1 & d[,colIndex]<=q3)] = "M"
        d$marker3[which(d[,colIndex]>= 1)] = paste0(colnames(d)[colIndex],"(H)")
        #d$marker3[which(d[,colIndex]>q3)] = "H"
      }

      marker_cols = grep("marker", colnames(d))

      d$type = apply(d, 1, function(x) paste(x[marker_cols], collapse="/"))
      d = d[,-marker_cols]
      v$dataPostProc = d
    }

  })

  plotInput2 <- reactive({
    if (is.null(v$dataPostProc)){
      return(NULL)
    }else{
      d = v$dataPostProc
      x = as.matrix(d%>%dplyr::select(-ID, -count, -percenttotal, -type)%>%data.frame())
      rownames(x) = d$ID

      h1 = Heatmap(x,colorRamp2(breaks=c(min(x),-1, 1,max(x)),colors = c("blue", "grey50", "grey50", "red")),
                   row_names_gp = gpar(fontsize = input$heatmap2Yfont),
                   column_names_gp = gpar(fontsize = input$heatmap2Xfont),
                   column_title = input$heatmap2Title,
                   column_title_gp = gpar(fontsize = input$heatmap2Titlefont),
                   row_dend_width = unit(2, "cm"),
                   column_dend_height = unit(2, "cm"),
                   heatmap_legend_param = list(title = "z-score",
                                               title_gp = gpar(fontsize = 12, fontface = "bold"),
                                               color_bar = "continuous",
                                               grid_height = unit(6, "mm"),
                                               grid_width = unit(6, "mm"),
                                               labels_gp = gpar(fontsize = 12)),
                   split = d$type,
                   combined_name_fun = function(x) paste(unlist(strsplit(x, "/")), collapse = "\n"),
                   gap = unit(5, "mm"))
      draw(h1)
    }
  })

  output$heatmap2 <- renderPlot({
    plotInput2()
  },height = 1020)

  output$dataToShowPostProc = DT::renderDataTable({
    if (is.null(v$dataPostProc)){
      return()
    }else{
      datatable(v$dataPostProc, rownames = FALSE, options = list(pageLength = 75, scrollX = TRUE))
    }
  })

  output$downloadDataPostProcessing <- downloadHandler(
    filename = function() {
      paste('data_post_processing', Sys.Date(), '.tsv', sep='')
    },
    content = function(con) {
      write.table(v$dataPostProc, file=con, row.names=F, quote = F, sep = "\t")
    }
  )

  output$downloadHeatmap2 <- downloadHandler(
    filename = function() {
      paste('heatmap_post_processing', Sys.Date(), '.pdf', sep='')
    },

    content = function(con) {
      pdf(con)
      draw(plotInput2())
      dev.off()
    }
  )

  output$selectForcedGroup <- renderUI({
    if(!is.null(v$msData)){
      selectInput('forcedGroup', '', width="250px", choices=c("None",unique(v$msData$group)), selected = "None")
    }else{
      return()
    }
  })

  ## Get the reference and the second group
  observeEvent(input$forceHeatmap, {
      v$forcedGroup = input$forcedGroup

      ## Get the data for the second group
      v$dataToPlot_forced = v$msData %>% subset(group==v$forcedGroup) %>%
        select(-group, -fn) %>% group_by(ID) %>% summarise_each(funs(median)) %>% ungroup
      v$dataToPlot_forced = v$dataToPlot_forced %>% dplyr::select(ID, count, one_of(v$clusterColumnsG), percenttotal)
      ## Fix rows according to the main heatmap
      colorder = colnames(v$dataToPlot%>%dplyr::select(-ID, -count, -percenttotal))[v$mainHeatColumnOrder]
      v$dataToPlot_forced = v$dataToPlot_forced[match(v$mainHeatRowOrder, v$dataToPlot_forced$ID),c("ID", "count", colorder, "percenttotal")]
  })

  output$dataToShowForced = DT::renderDataTable({
    if (is.null(v$dataToPlot_forced)){
      return()
    }else{
      datatable(v$dataToPlot_forced, rownames = FALSE, options = list(pageLength = 75, scrollX = TRUE))
    }
  })

  output$downloadDataForced <- downloadHandler(
    filename = function() {
      paste('data_forced_heatmap', Sys.Date(), '.tsv', sep='')
    },
    content = function(con) {
      write.table(v$dataToPlot_forced, file=con, quote = F, row.names = F, sep = "\t")
    }
  )

  forcedHeatmap <- reactive({
    if(is.null(v$dataToPlot_forced)){
      return(NULL)
    }else{
      df <- v$dataToPlot_forced
      x = as.matrix(df%>%dplyr::select(-ID, -count, -percenttotal)%>%data.frame())
      rownames(x) = df$ID

      ## Color palette (color range of heatmap)
      #heat_palette <- colorRampPalette(c("black", "yellow", "#FAF7C9"))
      #pairs.breaks <- c(seq(0, max(df), by=0.1))
      #pairs.breaks <- seq(0,max(df),by = 0.1)
      if(v$dendroBoolean=="Yes"){
        if((!is.null(v$dataToPlot_forced)) & is.null(v$annotationColumn) ){
          h1 = Heatmap(x,colorRamp2(breaks=c(min(x),0.25,max(x)),colors = c("black", "yellow", "red")),
                       row_names_gp = gpar(fontsize = v$heatmap1Yfont),
                       column_names_gp = gpar(fontsize = v$heatmap1Xfont),
                       column_title = paste0("SPADE node medians", "\n",v$heatmap1Title),
                       column_title_gp = gpar(fontsize = v$heatmap1Titlefont),
                       cluster_rows = FALSE,
                       cluster_columns = FALSE,
                       row_dend_width = unit(2, "cm"),
                       column_dend_height = unit(2, "cm"),
                       heatmap_legend_param = list(title = "Intensity",
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)))

          if(v$boxBoolean=="Yes"){
            ha_boxplot = HeatmapAnnotation(boxplot = anno_boxplot(x, which = "row"),
                                           which = "row", width = unit(2, "cm"))
            draw(ha_boxplot + h1, row_dend_side = "left", row_sub_title_side = "right")
          }else if (v$boxBoolean=="No"){
            draw(h1)
          }

        }else if( !is.null(v$dataToPlot_forced)  & v$annotationColumn=="None" ){
          h1 = Heatmap(x,colorRamp2(breaks=c(min(x),0.25,max(x)),colors = c("black", "yellow", "red")),
                       row_names_gp = gpar(fontsize = v$heatmap1Yfont),
                       column_names_gp = gpar(fontsize = v$heatmap1Xfont),
                       column_title = paste0("SPADE node medians", "\n",v$heatmap1Title),
                       column_title_gp = gpar(fontsize = v$heatmap1Titlefont),
                       cluster_rows = FALSE,
                       cluster_columns = FALSE,
                       row_dend_width = unit(2, "cm"),
                       column_dend_height = unit(2, "cm"),
                       heatmap_legend_param = list(title = "Intensity",
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)))
          if(v$boxBoolean=="Yes"){
            ha_boxplot = HeatmapAnnotation(boxplot = anno_boxplot(x, which = "row"),
                                           which = "row", width = unit(2, "cm"))
            draw(ha_boxplot + h1, row_dend_side = "left", row_sub_title_side = "right")
          }else if (v$boxBoolean=="No"){
            draw(h1)
          }

        }else if( !is.null(v$dataToPlot_forced)  & !is.null(v$annotationColumn) & v$annotationColumn!="None" ){

          h1 = Heatmap(x,colorRamp2(breaks=c(min(x),0.25,max(x)),colors = c("black", "yellow", "red")),
                       row_names_gp = gpar(fontsize = v$heatmap1Yfont),
                       column_names_gp = gpar(fontsize = v$heatmap1Xfont),
                       column_title = paste0("SPADE node medians", "\n",v$heatmap1Title),
                       column_title_gp = gpar(fontsize = v$heatmap1Titlefont),
                       cluster_rows = FALSE,
                       cluster_columns = FALSE,
                       row_dend_width = unit(2, "cm"),
                       column_dend_height = unit(2, "cm"),
                       heatmap_legend_param = list(title = "Intensity",
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)))

          h2 = Heatmap(v$dataToPlot[,v$annotationColumn], name = v$annotationColumn,
                       colorRamp2(c(0,max(v$dataToPlot[,v$annotationColumn])), c("blue","red")),
                       heatmap_legend_param = list(title = v$annotationColumn,
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)),
                       width = unit(6, "mm"))
          if(v$boxBoolean=="Yes"){
            ha_boxplot = HeatmapAnnotation(boxplot = anno_boxplot(x, which = "row"),
                                           which = "row", width = unit(2, "cm"))
            draw(ha_boxplot + h1 + h2, row_dend_side = "left", row_sub_title_side = "right")
          }else if (v$boxBoolean=="No"){
            draw(h1 + h2)
          }
        }
      }else{
        if( !is.null(v$dataToPlot) & is.null(v$annotationColumn) ){
          h1 = Heatmap(x,colorRamp2(breaks=c(min(x),0.25,max(x)),colors = c("black", "yellow", "red")),
                       row_names_gp = gpar(fontsize = v$heatmap1Yfont),
                       column_names_gp = gpar(fontsize = v$heatmap1Xfont),
                       column_title = paste0("SPADE node medians", "\n",v$heatmap1Title),
                       column_title_gp = gpar(fontsize = v$heatmap1Titlefont),
                       cluster_rows = FALSE,
                       cluster_columns = FALSE,
                       row_dend_width = unit(2, "cm"),
                       column_dend_height = unit(2, "cm"),
                       show_row_dend=FALSE,
                       show_column_dend=FALSE,
                       heatmap_legend_param = list(title = "Intensity",
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)))

          draw(h1)
        }else if( !is.null(v$dataToPlot)  & v$annotationColumn=="None"){
          h1 = Heatmap(x,colorRamp2(breaks=c(min(x),0.25,max(x)),colors = c("black", "yellow", "red")),
                       row_names_gp = gpar(fontsize = v$heatmap1Yfont),
                       column_names_gp = gpar(fontsize = v$heatmap1Xfont),
                       column_title = paste0("SPADE node medians", "\n",v$heatmap1Title),
                       column_title_gp = gpar(fontsize = v$heatmap1Titlefont),
                       cluster_rows = FALSE,
                       cluster_columns = FALSE,
                       row_dend_width = unit(2, "cm"),
                       column_dend_height = unit(2, "cm"),
                       show_row_dend=FALSE,
                       show_column_dend=FALSE,
                       heatmap_legend_param = list(title = "Intensity",
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)))
          draw(h1)
        }else if( !is.null(v$dataToPlot) & !is.null(v$annotationColumn) & v$annotationColumn!="None" ){
          h1 = Heatmap(x,colorRamp2(breaks=c(min(x),0.25,max(x)),colors = c("black", "yellow", "red")),
                       row_names_gp = gpar(fontsize = v$heatmap1Yfont),
                       column_names_gp = gpar(fontsize = v$heatmap1Xfont),
                       column_title = paste0("SPADE node medians", "\n",v$heatmap1Title),
                       column_title_gp = gpar(fontsize = v$heatmap1Titlefont),
                       cluster_rows = FALSE,
                       cluster_columns = FALSE,
                       row_dend_width = unit(2, "cm"),
                       column_dend_height = unit(2, "cm"),
                       show_row_dend=FALSE,
                       show_column_dend=FALSE,
                       heatmap_legend_param = list(title = "Intensity",
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)))
          h2 = Heatmap(v$dataToPlot[,v$annotationColumn], name = v$annotationColumn,
                       colorRamp2(c(0,max(v$dataToPlot[,v$annotationColumn])), c("blue","red")),
                       heatmap_legend_param = list(title = v$annotationColumn,
                                                   title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                   color_bar = "continuous",
                                                   grid_height = unit(6, "mm"),
                                                   grid_width = unit(6, "mm"),
                                                   labels_gp = gpar(fontsize = 12)),
                       width = unit(6, "mm"))
          draw(h1 + h2)
        }
      }
    }
  })

  output$forcedHeatmap <- renderPlot({
    forcedHeatmap()
  },height = 1020)

  output$downloadForcedHeatmap <- downloadHandler(
    filename = function() {
      paste('forced_heatmap', Sys.Date(), '.pdf', sep='')
    },

    content = function(con) {
      pdf(con)
      draw(forcedHeatmap())
      dev.off()
    }
  )





})
