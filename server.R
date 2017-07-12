
source("dependencies.R")

Logged = FALSE

shinyServer(function(input, output, session) {
  ## Source the Login ui
  source("www/Login.R",  local = TRUE)
  
  ## reactive values
  v = reactiveValues(cyto_session=NULL, experiments=NULL, exp_id=NULL,
                     spade_idx=NULL, spade_name=NULL,
                     sampleTags=NULL, cyto_spade=NULL, all_spades=NULL, 
                     cytoGroups=NULL,
                     dataGroups_tb = NULL, columnCorrectionInput=NULL,
                     columnType=NULL, cellCount=NULL, percentTotal = NULL,
                     scalingMethod=NULL, coFactor=NULL, tSNECols=NULL,
                     mainData=NULL, msgroups=NULL, clusterColumns=NULL,
                     ssfileInput=NULL, ssfileInputName=NULL, cytoSamples=FALSE,
                     singleSample=FALSE, multipleSamples = FALSE,num_groups=NULL,
                     groupSelected=NULL, dataToPlot=NULL, annotationColumn=NULL,
                     sortColumn=NULL,mainHeatmap = NULL,
                     mainHeatmapTitle=NULL, mainHeatmapTitlefont=NULL, 
                     mainHeatmapYfont=NULL, mainHeatmapXfont=NULL, 
                     mainHeatmapRowOrder=NULL, mainHeatmapColumnOrder=NULL,
                     forcedGroup=NULL, dataToPlot_forced=NULL, forcedHeatmap=NULL,
                     forcedHeatmapTitle=NULL, forcedHeatmapTitlefont=NULL, 
                     forcedHeatmapYfont=NULL, forcedHeatmapXfont=NULL,
                     postHeatmapTitlefont=NULL, postHeatmapTitle=NULL,
                     postHeatmapYfont=NULL, postHeatmapXfont=NULL, dataPostProc=NULL,
                     dataToPlot_overlay=NULL, overlayHeatmap=NULL, 
                     overlayHeatmapTitle=NULL, overlayHeatmapTitlefont=NULL, 
                     overlayHeatmapYfont=NULL, overlayHeatmapXfont=NULL)
  
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
        v$cyto_session = authenticate(site=tolower(input$userSite), username = input$userName, password = input$passwd)
      }else if(input$userName=="" & input$passwd=="" & input$cytoToken!=""){
        v$cyto_session = authenticate(site=tolower(input$userSite), auth_token=input$cytoToken)
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
          ## Here create a temporary directory to save the s
          temp_dir = "~/cytocluster_temp"
            if(dir.exists(temp_dir)){
            unlink(temp_dir, recursive = T, force = T)
          }
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
  
  ## Select SPADE
  output$submitSpade_header <- renderUI({
    if(is.null(v$all_spades)){
      return()
    }else{
      h4(strong("Select SPADE analysis"))
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
  
  output$submitSpade_btn <- renderUI({
    if(is.null(v$all_spades)){
      return()
    }else{
      actionButton("submitSpade", "Get statistics tables",
                   icon("cloud-download"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
  ## End of select SPADE
  
  ## After selecting a spade analysis get the files and their tag
  observeEvent(input$submitSpade, {
    
    ## Print a message to the user
    my_spade_check_test <- "This may take a couple of minutes. Please wait..."
    js_string <- 'alert("SOMETHING");'
    js_string <- sub("SOMETHING",my_spade_check_test,js_string)
    session$sendCustomMessage(type='jsCode', list(value = js_string))
    
    ## Get the spade object (take the first if none is TRUE)
    as = hot_to_r(input$all_spades)
    if(sum(as$Pick)==0){
      spade_selected = hot_to_r(input$all_spades) %>% slice(1)
    }else if (sum(as$Pick)>1) {
      spade_selected = hot_to_r(input$all_spades) %>% subset(Pick==TRUE) %>% slice(1)
    }else if (sum(as$Pick)==1) {
      spade_selected = hot_to_r(input$all_spades) %>% subset(Pick==TRUE)
    }
    
    
    ## Always analyse one spade - only the first is picked
    v$spade_idx = spade_selected$id[1]
    v$spade_name = spade_selected$name[1]
    v$cyto_spade = spade.show(v$cyto_session, experiment_id = v$exp_id, spade_id = v$spade_idx)
    temp_dir = "~/cytocluster_temp"
    dir.create(temp_dir)
    ## Download the statistics tables
    spade.download_statistics_tables(v$cyto_session, spade=v$cyto_spade, directory = temp_dir)
    old_path = getwd()
    setwd(temp_dir)
    ## Change the file name in case it contains spaces
    system("for f in *.zip; do mv \"$f\" \"${f// /}\"; done")
    zipped_fn = list.files('.', pattern = ".zip_statistics.zip")
    system(paste0("unzip ", temp_dir, "/", zipped_fn))
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
                         Group=g, 
                         Files=length(g_fns), 
                         Output.Directory="",
                         stringsAsFactors = FALSE)
      v$cytoGroups = rbind(v$cytoGroups, g_add)
    }
  })
  
  output$submitCytoGroups_header <- renderUI({
    if(is.null(v$cytoGroups)){
      return()
    }else{
      h4(strong("Select groups to analyse"))
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
  
  ## Input options
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
                 numericInput('cellCountCyto', h4('Filter by cell count'), value = -1),
                 numericInput('percentTotalCyto', h4('Filter by percenttotal'), value = -1)
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
  
  ## Submit buttongs for cytogroups
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
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        actionButton("getToTagPage", "Change sample tags",
                     icon("edit"), 
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      )
    }
  })
  
  ## functionality for the two buttons in submitCytoGroups above
  ## Read in the groups
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
      v$mainData = NULL
      v$msgroups = NULL
    }else{
      dtb = v$dataGroups_tb
      dtb = dtb %>% subset(Pick==TRUE)
      if(sum(dtb$Input.Directory=="")==nrow(dtb)){
        v$mainData = NULL
        v$msgroups = NULL
      }else{
        dtb = dtb %>% subset(Input.Directory!="")
        for(i in 1:nrow(dtb)){
          pattern="csv|xlsx|xls"
          p = dtb$Input.Directory[i]
          g = dtb$Group[i]
          if(g=="" | g=="-"){
            g=paste0(as.character(i), "_group")
          }
  
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
              if(v$columnType=="Medians"){
                kept_cols = c("ID", "count", colnames(d)[grep("medians", colnames(d))], "percenttotal")
                kept_cols = kept_cols[!grepl("raw_median", kept_cols)]
                kept_cols = data.frame(col=kept_cols)
              }else if (v$columnType=="Raw medians"){
                kept_cols = c("ID", "count", colnames(d)[grep("raw_median", colnames(d))], "percenttotal")
                kept_cols = data.frame(col=kept_cols)
              }
              
              
              ## Check nameconversion file
              if (is.null(v$columnCorrectionInput)){
                ## Sometimes the user may supply a file with names already fixed
                ## Check that and return the file as it is if it is the case
                if (length(colnames(d)[grep("medians", colnames(d))])==0 & length(colnames(d)[grep("raw_median", colnames(d))])==0){
                  d = d
                }else{
                  d = d %>% select(one_of(kept_cols$col))
                }
              }else if (!is.null(v$columnCorrectionInput)){
                n = read.xlsx(v$columnCorrectionInput$datapath, 1)[,1:2]
                colnames(n) = c("row", "marker")
                n = n %>% mutate(row=as.character(row))

                n$col = apply(n, 1, function(x){
                  grep(x[1], kept_cols$col, value = T)
                })
                
                ## Check here if n$col is a list and then print a message
                if(is.list(n$col)){
                  my_message <- "Conversion column matched with multiple columns"
                  js_string <- 'alert("SOMETHING");'
                  js_string <- sub("SOMETHING",my_message,js_string)
                  session$sendCustomMessage(type='jsCode', list(value = js_string))
                }else{
                  kept_cols = kept_cols %>% left_join(n)
                  ids = c("ID", "count", "percenttotal")
                  kept_cols = kept_cols %>% mutate(marker=ifelse(col%in%ids, col, 
                                                                 ifelse(grepl("tSNE", col), col, marker)))
                  kept_cols = kept_cols %>% subset(!is.na(marker))
                  d = d %>% select(one_of(kept_cols$col))
                  colnames(d) = kept_cols$marker
                }
              }
              
              d = d %>% data.frame()
              
              ## Take in other parameters as well
              #log2(d/co-factor + sqrt(1 + (d/co-factor)^2) )
              if (v$scalingMethod=="arcsinh"){
                ds = d %>% dplyr::select(-ID, -count, -percenttotal)
                dns = d %>% dplyr::select(ID, count, percenttotal)
                ds = as.matrix(ds)
                ds = apply(ds, 2,function(x) log(x/as.numeric(v$coFactor) + sqrt(1 + (x/as.numeric(v$coFactor))^2) ))
                d = cbind(dns%>%dplyr::select(ID,count),ds, dns%>%dplyr::select(percenttotal))
              }else{
                d=d
              }
              
              ## Filters on cellCount and percentTotal
              if("count"%in%colnames(d) & v$cellCount>=0){
                d = d %>% subset(count > v$cellCount)
              }else if(v$cellCount == -1){
                d[is.na(d)] = -1  
              }
              
              if("percenttotal"%in%colnames(d) & v$percentTotal>=0){
                d = d %>% subset(percenttotal > v$percentTotal)
              }
              
              d = d %>% mutate(fn=fn, group=g)
              
              if(v$tSNECols=="No"){
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
      
      v$mainData = group_tb %>% data.frame()
      v$msgroups = unique(group_tb$group)
      v$cytoSamples=TRUE
    }
  })
  
  ## Clean raw files from cytobank
  observeEvent(input$saveCleanCytoGroups, {
    v$dataGroups_tb <- hot_to_r(input$cytoGroups)
    v$columnCorrectionInput = input$columnCorrectionInputCyto
    v$columnType = input$columnTypeCyto
    v$cellCount = input$cellCountCyto
    v$percentTotal = input$percentTotalCyto
    v$scalingMethod = input$scalingMethodCyto
    v$coFactor = input$coFactorCyto
    v$tSNECols = input$tSNEcolsCyto
    
    if(!is.null(v$dataGroups_tb)){
      ## Work on the input table to define the number of groups
      dtb = v$dataGroups_tb
      dtb = dtb %>% subset(Pick==TRUE)
      
      if(nrow(dtb)>0 & sum(dtb$Output.Directory!="" & dtb$Input.Directory!="")>0){
        dtb = dtb %>% subset(Input.Directory!="" & Output.Directory!="")
        for(i in 1:nrow(dtb)){
          pattern="csv|xlsx|xls"
          p = dtb$Input.Directory[i]
          g = dtb$Group[i]
          s = dtb$Output.Directory[i]
          
          ## Start reading in the data
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
              if(v$columnType=="Medians"){
                kept_cols = c("ID", "count", colnames(d)[grep("medians", colnames(d))], "percenttotal")
                kept_cols = kept_cols[!grepl("raw_median", kept_cols)]
                kept_cols = data.frame(col=kept_cols)
              }else if (v$columnType=="Raw medians"){
                kept_cols = c("ID", "count", colnames(d)[grep("raw_median", colnames(d))], "percenttotal")
                kept_cols = data.frame(col=kept_cols)
              }
                
              ## Check nameconversion file
              if (is.null(v$columnCorrectionInput)){
                ## Sometimes the user may supply a file with names already fixed
                ## Check that and return the file as it is if it is the case
                if (length(colnames(d)[grep("medians", colnames(d))])==0 & length(colnames(d)[grep("raw_median", colnames(d))])==0){
                  d = d
                }else{
                  d = d %>% select(one_of(kept_cols$col))
                }
              }else if (!is.null(v$columnCorrectionInput)){
                n = read.xlsx(v$columnCorrectionInput$datapath, 1)[,1:2]
                colnames(n) = c("row", "marker")
                n = n %>% mutate(row=as.character(row))
                
                n$col = apply(n, 1, function(x){
                  grep(x[1], kept_cols$col, value = T)
                })
                
                ## Check here if n$col is a list and then print a message
                if(is.list(n$col)){
                  my_message <- "Conversion column matched with multiple columns"
                  js_string <- 'alert("SOMETHING");'
                  js_string <- sub("SOMETHING",my_message,js_string)
                  session$sendCustomMessage(type='jsCode', list(value = js_string))
                }else{
                  kept_cols = kept_cols %>% left_join(n)
                  ids = c("ID", "count", "percenttotal")
                  kept_cols = kept_cols %>% mutate(marker=ifelse(col%in%ids, col, 
                                                                 ifelse(grepl("tSNE", col), col, marker)))
                  kept_cols = kept_cols %>% subset(!is.na(marker))
                  d = d %>% select(one_of(kept_cols$col))
                  colnames(d) = kept_cols$marker
                }
              }
                
              if(v$tSNECols=="No"){
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
              if (v$scalingMethod=="arcsinh"){
                ds = d %>% dplyr::select(-ID, -count, -percenttotal)
                dns = d %>% dplyr::select(ID, count, percenttotal)
                ds = as.matrix(ds)
                ds = apply(ds, 2,function(x) log(x/as.numeric(v$coFactor) + sqrt(1 + (x/as.numeric(v$coFactor))^2) ))
                d = cbind(dns%>%dplyr::select(ID,count),ds, dns%>%dplyr::select(percenttotal))
              }else{
                d=d
              }
                
              ## Filters on cellCount and percentTotal
              if("count"%in%colnames(d) & v$cellCount>=0){
                d = d %>% subset(count > v$cellCount)
              }else if(v$cellCount == -1){
                d[is.na(d)] = -1
              }
              
              if("percenttotal"%in%colnames(d) & v$percentTotal>=0){
                d = d %>% subset(percenttotal > v$percentTotal)
              }
              
              
              #save_dir=paste0(s, "/", g)
              save_dir=s

              if (!file.exists(save_dir)){
                dir.create(file.path(save_dir))
              }
              write.table(d, file=paste0(save_dir, "/", fn, "_cleaned.csv"), quote = F, row.names = F, sep = ",")
                
              # Increment the progress bar, and update the detail text.
              incProgress(1/y, detail = paste("Saving ", g,": ", y, " files"))
                
              # Pause for 0.1 seconds to simulate a long computation.
              Sys.sleep(0.1)
                
            }
          })
        }
      }else{
        ## display a mesage here to the user if there is no save directory
        save_dir_check <- "No Output.Directory found! Please supply a directory to save the cleaned files"
        js_string <- 'alert("SOMETHING");'
        js_string <- sub("SOMETHING",save_dir_check,js_string)
        session$sendCustomMessage(type='jsCode', list(value = js_string))
      }
    }
  })
  
  ## Go to sample tags page
  observeEvent(input$getToTagPage, {
    updateTabItems(session, "tabs", selected = "sampleTags")
  })
  
  ## Table for sample tags
  output$sampleTagstb = renderRHandsontable({
    if(is.null(v$cytoGroups)){
      return()
    }else{
      ## Get file names
      temp_dir = "~/cytocluster_temp/bySample/"
      fns = list.files(temp_dir)
      rhandsontable(data.frame(file=fns, group=rep("", length(fns)))) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }
  })
  
  observeEvent(input$submitSampleTags, {
    user_groups = hot_to_r(input$sampleTagstb)
    if(sum(!user_groups$group%in%c("", NA))>0){
      ## Exclude empty lines
      user_groups = user_groups %>% subset(file!="" & !is.na(file))
      ## Substitute empty strings for "Unknown group"
      user_groups$group[user_groups$group==""] = "Unknown"
      
      ## Trasnfer the files to new directories
      ## First delete the previous groups
      temp_dir = "~/cytocluster_temp/"
      dir_ns = list.dirs(temp_dir)
      dir_ns = dir_ns[2:length(dir_ns)] ## Parent directory is always first
      dir_ns = dir_ns[!grepl("byAttribute|byNodeID|bySample", dir_ns)]
      for(n in dir_ns){unlink(n, recursive = T, force = T)}
      
      v$sampleTags = cbind(v$sampleTags%>%arrange(FCS.Filename), user_groups%>%arrange(file))

      ## Create the new directories
      v$cytoGroups = NULL
      for(g in unique(v$sampleTags$group)){
        g_dir = paste0(temp_dir, "/", g)
        dir.create(g_dir)
        g_fns = v$sampleTags %>% subset(group==g) %>% .$file
        file.copy(paste0(temp_dir,"/bySample/",g_fns), g_dir)
        g_add = data.frame(Pick=TRUE, 
                           Input.Directory=g_dir, 
                           Group=g, 
                           Files=length(g_fns), 
                           Output.Directory="",
                           stringsAsFactors = FALSE)
        v$cytoGroups = rbind(v$cytoGroups, g_add)
      }
      
      ## Push sample tags to cytobank
      v$sampleTags = v$sampleTags %>% select(-Conditions, -file) %>% rename(Conditions=group)
      v$sampleTags = v$sampleTags[,c(1, length(v$sampleTags), 2:(length(v$sampleTags)-1))]
      ## write new group files
      #v$sampleTags$FCS.Filename = sub("_live.*", "", v$sampleTags$FCS.Filename)
      #write.table(v$sampleTags%>%select(FCS.Filename, Conditions)%>%rename(Sample.Type=Conditions), file=paste0(temp_dir,"/experiment_",v$exp_id,"_annotations.tsv"), row.names = F, col.names = F, quote=F, sep="\t")
      ## Upload sample tags (uncomment to test when ready)
      #sample_tags.upload(v$cyto_session, v$exp_id, file_path=paste0(temp_dir,"/experiment_",v$exp_id,"_annotations.tsv"))
      updateTabItems(session, "tabs", selected = "cytologin")
    }
  })
  
  ## Markers to cluster
  output$markersToClusterCyto <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(v$mainData) | v$cytoSamples==FALSE)
      return()
    
    # Get the data set with the appropriate name
    cnames <- colnames(v$mainData)
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
  ## buttons for selecting columns to cluster (Cytobank part)
  observeEvent(input$UncheckAllCyto,{
    cnames <- colnames(v$mainData)
    cnames <- cnames[cnames!="ID"]
    cnames <- cnames[cnames!="count"]
    cnames <- cnames[cnames!="percenttotal"]
    cnames <- cnames[cnames!="fn"]
    cnames <- cnames[cnames!="group"]
    updateCheckboxGroupInput(session=session, inputId="markersTCCyto", 
                             choices=cnames, 
                             selected=NULL,
                             inline = TRUE)
  })
  observeEvent(input$CheckAllCyto,{
    cnames <- colnames(v$mainData)
    cnames <- cnames[cnames!="ID"]
    cnames <- cnames[cnames!="count"]
    cnames <- cnames[cnames!="percenttotal"]
    cnames <- cnames[cnames!="fn"]
    cnames <- cnames[cnames!="group"]
    updateCheckboxGroupInput(session=session, inputId="markersTCCyto", 
                             choices=cnames, 
                             selected=cnames,
                             inline = TRUE)
  })
  ## Submit clustering columns
  observeEvent(input$submitCytoMode, {
    if(is.null(v$mainData)){
      v$clusterColumns = NULL
    }else{
      cc <- as.vector(input$markersTCCyto)
      v$clusterColumns = cc
      ## Change tab to hier clustering
      updateTabItems(session, "tabs", selected = "hierclus")
    }
  })
  
  ## UI to select group for main heatmap
  output$groupPlotSelect <- renderUI({
    if(!is.null(v$mainData) | v$singleSample){
      bootstrapPage(
        div(style="display:inline-block",selectInput('msPlot', '', width="250px", choices=c("None",unique(v$mainData$group)), selected = "None")),
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
    if(v$singleSample){
      return(NULL)
    }else{
      v$groupSelected = input$msPlot
      ## Group to get the overall medians
      v$dataToPlot = v$mainData %>% subset(group==v$groupSelected) %>% 
        select(-group, -fn) %>% group_by(ID) %>% summarise_each(funs(median)) %>% ungroup
      v$dataToPlot = v$dataToPlot %>% dplyr::select(ID, count, one_of(v$clusterColumns), percenttotal)
    }
  })
  
  ## Data to be shown in the "Data" tab
  output$dataToShow = DT::renderDataTable({
    if (is.null(v$dataToPlot)){
      return()
    }else{
      datatable(v$dataToPlot, rownames = FALSE, options = list(pageLength = 75, scrollX = TRUE))
    }
  })
  
  
  ###########################################
  ##            Single sample
  ###########################################
  observeEvent(input$submitSSparams, {
    v$ssfileInput = input$FileInputSS
    v$ssfileInputName = input$FileInputSS[["name"]]
    v$columnCorrectionInput = input$columnCorrectionInputSS
    v$columnType = input$columnTypeSS
    v$cellCount = input$cellCountSS
    v$percentTotal = input$percentTotalSS
    v$scalingMethod = input$scalingMethodSS
    v$coFactor = input$coFactorSS
    v$tSNECols = input$tSNEcolsSS
    
    if(!is.null(v$ssfileInput)){
      ## capture the type of the file
      splits = unlist(strsplit(v$ssfileInputName, "\\."))
      ext = splits[length(splits)]
      if (ext=="xlsx" | ext=="xls"){
        d = read.xlsx(v$ssfileInput$datapath, 1)
      }else if (ext=="csv"){
        d = read.table(v$ssfileInput$datapath, sep=",", header = T)
      }
      
      
      ## exclude raw_medians if you select the medians column
      if(v$columnType=="Medians"){
        kept_cols = c("ID", "count", colnames(d)[grep("medians", colnames(d))], "percenttotal")
        kept_cols = kept_cols[!grepl("raw_median", kept_cols)]
        kept_cols = data.frame(col=kept_cols)
      }else if (v$columnType=="Raw medians"){
        kept_cols = c("ID", "count", colnames(d)[grep("raw_median", colnames(d))], "percenttotal")
        kept_cols = data.frame(col=kept_cols)
      }
      
      
      ## Check nameconversion file
      if (is.null(v$columnCorrectionInput)){
        ## Sometimes the user may supply a file with names already fixed
        ## Check that and return the file as it is if it is the case
        if (length(colnames(d)[grep("medians", colnames(d))])==0 & length(colnames(d)[grep("raw_median", colnames(d))])==0){
          d = d
        }else{
          d = d %>% select(one_of(kept_cols$col))
        }
      }else if (!is.null(v$columnCorrectionInput)){
        n = read.xlsx(v$columnCorrectionInput$datapath, 1)[,1:2]
        colnames(n) = c("row", "marker")
        n = n %>% mutate(row=as.character(row))
        
        n$col = apply(n, 1, function(x){
          grep(x[1], kept_cols$col, fixed = T, value = T)
        })
        
        ## Check here if n$col is a list and then print a message
        if(is.list(n$col)){
          my_message <- "Conversion column matched with multiple columns"
          js_string <- 'alert("SOMETHING");'
          js_string <- sub("SOMETHING",my_message,js_string)
          session$sendCustomMessage(type='jsCode', list(value = js_string))
        }else{
          kept_cols = kept_cols %>% left_join(n)
          ids = c("ID", "count", "percenttotal")
          kept_cols = kept_cols %>% mutate(marker=ifelse(col%in%ids, col, 
                                                         ifelse(grepl("tSNE", col), col, marker)))
          kept_cols = kept_cols %>% subset(!is.na(marker))
          d = d %>% select(one_of(kept_cols$col))
          colnames(d) = kept_cols$marker
        }
      }
      
      ## Take in other parameters as well
      #log2(d/co-factor + sqrt(1 + (d/co-factor)^2) )
      if (v$scalingMethod=="arcsinh"){
        ds = d %>% dplyr::select(-ID, -count, -percenttotal)
        dns = d %>% dplyr::select(ID, count, percenttotal)
        ds = as.matrix(ds)
        ds = apply(ds, 2,function(x) log(x/as.numeric(v$coFactor) + sqrt(1 + (x/as.numeric(v$coFactor))^2) ))
        d = cbind(dns%>%dplyr::select(ID,count),ds, dns%>%dplyr::select(percenttotal))
      }else{
        d=d
      }
      
      ## Filters on cellCount and percentTotal
      if("count"%in%colnames(d) & v$cellCount>=0){
        d = d %>% subset(count > v$cellCount)
      }else if(v$cellCount == -1){
        d[is.na(d)] = -1
      }
      
      if("percenttotal"%in%colnames(d) & v$percentTotal>=0){
        d = d %>% subset(percenttotal > v$percentTotal)
      }
      
      ## Check if tSNE cols are needed or not
      if(v$tSNECols=="No"){
        if(length(grep("tSNE", colnames(d)))==0){
          d = d
        }else{
          d = d[, -grep("tSNE", colnames(d))]
        }
      }else{
        d = d
      }
      
      v$mainData = d %>% data.frame()
      v$singleSample=TRUE
    }
    
  })
  
  ## Markers to cluster
  output$markersToClusterSS <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(v$mainData) | v$singleSample==FALSE)
      return()
    
    # Get the data set with the appropriate name
    cnames <- colnames(v$mainData)
    cnames <- cnames[cnames!="ID"]
    cnames <- cnames[cnames!="count"]
    cnames <- cnames[cnames!="percenttotal"]
    cnames <- cnames[cnames!="fn"]
    cnames <- cnames[cnames!="group"]
    # Create the checkboxes and select them all by default
    list(
      wellPanel(
        h2("Select Markers to cluster"),
        checkboxGroupInput("markersTCSS",h4(""),
                           choices  = cnames,
                           selected = cnames,
                           inline = TRUE),
        bootstrapPage(
          div(style="display:inline-block",actionButton("UncheckAllSS", "Uncheck all", 
                                                        icon("square-o"),
                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
          div(style="display:inline-block",actionButton("CheckAllSS", "Check All",
                                                        icon("check-square-o"),
                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        )
      ),
      br(),
      actionButton("submitSSMode", "Go!",
                   icon("line-chart"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  })
  ## buttons for selecting columns to cluster (Cytobank part)
  observeEvent(input$UncheckAllSS,{
    cnames <- colnames(v$mainData)
    cnames <- cnames[cnames!="ID"]
    cnames <- cnames[cnames!="count"]
    cnames <- cnames[cnames!="percenttotal"]
    cnames <- cnames[cnames!="fn"]
    cnames <- cnames[cnames!="group"]
    updateCheckboxGroupInput(session=session, inputId="markersTCSS", 
                             choices=cnames, 
                             selected=NULL,
                             inline = TRUE)
  })
  observeEvent(input$CheckAllSS,{
    cnames <- colnames(v$mainData)
    cnames <- cnames[cnames!="ID"]
    cnames <- cnames[cnames!="count"]
    cnames <- cnames[cnames!="percenttotal"]
    cnames <- cnames[cnames!="fn"]
    cnames <- cnames[cnames!="group"]
    updateCheckboxGroupInput(session=session, inputId="markersTCSS", 
                             choices=cnames, 
                             selected=cnames,
                             inline = TRUE)
  })
  ## Submit clustering columns
  observeEvent(input$submitSSMode, {
    if(is.null(v$mainData)){
      v$clusterColumns = NULL
    }else{
      cc <- as.vector(input$markersTCSS)
      v$clusterColumns = cc
      ## Change tab to hier clustering
      v$dataToPlot = v$mainData %>% dplyr::select(ID, count, one_of(v$clusterColumns), percenttotal)
      updateTabItems(session, "tabs", selected = "hierclus")
    }
  })
  
  ###########################################
  ##            Multiple samples
  ###########################################
  output$dataGroups_tbs = renderRHandsontable({
      d = data.frame(Group = 1:input$num_groups, Pick = TRUE,
                                  Input.Directory=rep("None",input$num_groups),
                                  Name=rep("None",input$num_groups),
                                  Output.Directory=rep("None",input$num_groups),
                                  stringsAsFactors = FALSE)
      rhandsontable(d) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  observeEvent(input$submitMSparams, {
    v$dataGroups_tb <- hot_to_r(input$dataGroups_tbs)
    v$columnCorrectionInput = input$columnCorrectionInputMS
    v$columnType = input$columnTypeMS
    v$cellCount = input$cellCountMS
    v$percentTotal = input$percentTotalMS
    v$scalingMethod = input$scalingMethodMS
    v$coFactor = input$coFactorMS
    v$tSNECols = input$tSNEcolsMS
    
    group_tb = NULL
    if(is.null(v$dataGroups_tb)){
      v$msData = NULL
      v$msgroups = NULL
    }else{
      dtb = v$dataGroups_tb
      dtb = dtb %>% subset(Pick==TRUE)
      if(sum(dtb$Input.Directory=="None")==nrow(dtb)){
        v$msData = NULL
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
              if(v$columnType=="Medians"){
                kept_cols = c("ID", "count", colnames(d)[grep("medians", colnames(d))], "percenttotal")
                kept_cols = kept_cols[!grepl("raw_median", kept_cols)]
                kept_cols = data.frame(col=kept_cols)
              }else if (v$columnType=="Raw medians"){
                kept_cols = c("ID", "count", colnames(d)[grep("raw_median", colnames(d))], "percenttotal")
                kept_cols = data.frame(col=kept_cols)
              }
              
              ## Check nameconversion file
              if (is.null(v$columnCorrectionInput)){
                ## Sometimes the user may supply a file with names already fixed
                ## Check that and return the file as it is if it is the case
                if (length(colnames(d)[grep("medians", colnames(d))])==0 & length(colnames(d)[grep("raw_median", colnames(d))])==0){
                  d = d
                }else{
                  d = d %>% select(one_of(kept_cols$col))
                }
              }else if (!is.null(v$columnCorrectionInput)){
                n = read.xlsx(v$columnCorrectionInput$datapath, 1)[,1:2]
                n = n %>% subset(!is.na(marker))
                colnames(n) = c("row", "marker")
                n = n %>% mutate(row=as.character(row))
                
                n$col = apply(n, 1, function(x){
                  grep(x[1], kept_cols$col, value = T)
                })
                
                ## Check here if n$col is a list and then print a message
                if(is.list(n$col)){
                  my_message <- "Conversion column matched with multiple columns"
                  js_string <- 'alert("SOMETHING");'
                  js_string <- sub("SOMETHING",my_message,js_string)
                  session$sendCustomMessage(type='jsCode', list(value = js_string))
                }else{
                  kept_cols = kept_cols %>% left_join(n)
                  ids = c("ID", "count", "percenttotal")
                  kept_cols = kept_cols %>% mutate(marker=ifelse(col%in%ids, col, 
                                                                 ifelse(grepl("tSNE", col), col, marker)))
                  kept_cols = kept_cols %>% subset(!is.na(marker))
                  d = d %>% select(one_of(kept_cols$col))
                  colnames(d) = kept_cols$marker
                }
              }
              
              ## Take in other parameters as well
              #log2(d/co-factor + sqrt(1 + (d/co-factor)^2) )
              if (v$scalingMethod=="arcsinh"){
                ds = d %>% dplyr::select(-ID, -count, -percenttotal)
                dns = d %>% dplyr::select(ID, count, percenttotal)
                ds = as.matrix(ds)
                
                ds = apply(ds, 2,function(x) log(x/as.numeric(v$coFactor) + sqrt(1 + (x/as.numeric(v$coFactor))^2) ))
                d = cbind(dns%>%dplyr::select(ID,count),ds, dns%>%dplyr::select(percenttotal))
              }else{
                d=d
              }
              
              ## Filters on cellCount and percentTotal
              if("count"%in%colnames(d) & v$cellCount>=0){
                d = d %>% subset(count > v$cellCount)
              }else if(v$cellCount == -1){
                d[is.na(d)] = -1
              }
              
              if("percenttotal"%in%colnames(d) & v$percentTotal>=0){
                d = d %>% subset(percenttotal > v$percentTotal)
              }
              
              d = d %>% mutate(fn=fn, group=g)
              
              if(v$tSNECols=="No"){
                if(length(grep("tSNE", colnames(d)))==0){
                  d = d
                }else{
                  d = d[, -grep("tSNE", colnames(d))]
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
      
      v$mainData = group_tb %>% data.frame()
      v$multipleSamples=TRUE
    }
  })
  
  ## Get the data for each group
  observeEvent(input$saveCleanMSGroups, {
    v$dataGroups_tb <- hot_to_r(input$dataGroups_tbs)
    v$columnCorrectionInput = input$columnCorrectionInputMS
    v$columnType = input$columnTypeMS
    v$cellCount = input$cellCountMS
    v$percentTotal = input$percentTotalMS
    v$scalingMethod = input$scalingMethodMS
    v$coFactor = input$coFactorMS
    v$tSNECols = input$tSNEcolsMS
    
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
                if(v$columnType=="Medians"){
                  kept_cols = c("ID", "count", colnames(d)[grep("medians", colnames(d))], "percenttotal")
                  kept_cols = kept_cols[!grepl("raw_median", kept_cols)]
                  kept_cols = data.frame(col=kept_cols)
                }else if (v$columnType=="Raw medians"){
                  kept_cols = c("ID", "count", colnames(d)[grep("raw_median", colnames(d))], "percenttotal")
                  kept_cols = data.frame(col=kept_cols)
                }
                
                
                ## Check nameconversion file
                if (is.null(v$columnCorrectionInput)){
                  ## Sometimes the user may supply a file with names already fixed
                  ## Check that and return the file as it is if it is the case
                  if (length(colnames(d)[grep("medians", colnames(d))])==0 & length(colnames(d)[grep("raw_median", colnames(d))])==0){
                    d = d
                  }else{
                    d = d %>% select(one_of(kept_cols$col))
                  }
                }else if (!is.null(v$columnCorrectionInput)){
                  n = read.xlsx(v$columnCorrectionInput$datapath, 1)[,1:2]
                  colnames(n) = c("row", "marker")
                  n = n %>% mutate(row=as.character(row))
                  
                  n$col = apply(n, 1, function(x){
                    grep(x[1], kept_cols$col, value = T)
                  })
                  
                  ## Check here if n$col is a list and then print a message
                  if(is.list(n$col)){
                    my_message <- "Conversion column matched with multiple columns"
                    js_string <- 'alert("SOMETHING");'
                    js_string <- sub("SOMETHING",my_message,js_string)
                    session$sendCustomMessage(type='jsCode', list(value = js_string))
                  }else{
                    kept_cols = kept_cols %>% left_join(n)
                    ids = c("ID", "count", "percenttotal")
                    kept_cols = kept_cols %>% mutate(marker=ifelse(col%in%ids, col, 
                                                                   ifelse(grepl("tSNE", col), col, marker)))
                    kept_cols = kept_cols %>% subset(!is.na(marker))
                    d = d %>% select(one_of(kept_cols$col))
                    colnames(d) = kept_cols$marker
                  }
                }
                
                if(v$tSNECols=="No"){
                  if(length(grep("tSNE", colnames(d)))==0){
                    d = d
                  }else{
                    d = d[, -grep("tSNE", colnames(d))]
                  }
                }else{
                  d = d %>% data.frame()
                }
                
                d = d %>% data.frame()
                
                ## Take in other parameters as well
                #log2(d/co-factor + sqrt(1 + (d/co-factor)^2) )
                if (v$scalingMethod=="arcsinh"){
                  ds = d %>% dplyr::select(-ID, -count, -percenttotal)
                  dns = d %>% dplyr::select(ID, count, percenttotal)
                  ds = as.matrix(ds)
                  ds = apply(ds, 2,function(x) log(x/as.numeric(v$coFactor) + sqrt(1 + (x/as.numeric(v$coFactor))^2) ))
                  d = cbind(dns%>%dplyr::select(ID,count),ds, dns%>%dplyr::select(percenttotal))
                }else{
                  d=d
                }
                
                ## Filters on cellCount and percentTotal
                if("count"%in%colnames(d) & v$cellCount>=0){
                  d = d %>% subset(count > v$cellCount)
                }else if(v$cellCount == -1){
                  d[is.na(d)] = -1
                }
                
                if("percenttotal"%in%colnames(d) & v$percentTotal>=0){
                  d = d %>% subset(percenttotal > v$percentTotal)
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
  
  output$markersToClusterMS <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(v$mainData) | v$multipleSamples==FALSE)
      return()
    
    # Get the data set with the appropriate name
    cnames <- colnames(v$mainData)
    cnames <- cnames[cnames!="ID"]
    cnames <- cnames[cnames!="count"]
    cnames <- cnames[cnames!="percenttotal"]
    cnames <- cnames[cnames!="fn"]
    cnames <- cnames[cnames!="group"]
    # Create the checkboxes and select them all by default
    list(
      wellPanel(
        h2("Select Markers to cluster"),
        checkboxGroupInput("markersTCMS",h4(""),
                           choices  = cnames,
                           selected = cnames,
                           inline = TRUE),
        bootstrapPage(
          div(style="display:inline-block",actionButton("UncheckAllMS", "Uncheck all", 
                                                        icon("square-o"),
                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
          div(style="display:inline-block",actionButton("CheckAllMS", "Check All",
                                                        icon("check-square-o"),
                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        )
      ),
      br(),
      actionButton("submitMSMode", "Go!",
                   icon("line-chart"), 
                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  })
  
  observeEvent(input$UncheckAllMS,{
    cnames <- colnames(v$mainData)
    cnames <- cnames[cnames!="ID"]
    cnames <- cnames[cnames!="count"]
    cnames <- cnames[cnames!="percenttotal"]
    cnames <- cnames[cnames!="fn"]
    cnames <- cnames[cnames!="group"]
    updateCheckboxGroupInput(session=session, inputId="markersTCMS", 
                             choices=cnames, 
                             selected=NULL,
                             inline = TRUE)
  })
  
  observeEvent(input$CheckAllMS,{
    cnames <- colnames(v$mainData)
    cnames <- cnames[cnames!="ID"]
    cnames <- cnames[cnames!="count"]
    cnames <- cnames[cnames!="percenttotal"]
    cnames <- cnames[cnames!="fn"]
    cnames <- cnames[cnames!="group"]
    updateCheckboxGroupInput(session=session, inputId="markersTCMS", 
                             choices=cnames, 
                             selected=cnames,
                             inline = TRUE)
  })
  
  observeEvent(input$submitMSMode, {
    if(is.null(v$mainData)){
      v$clusterColumns = NULL
    }else{
      cc <- as.vector(input$markersTCMS)
      v$clusterColumns = cc
      ## Change tab to hier clustering
      updateTabItems(session, "tabs", selected = "hierclus")
    }
  })
  
  
  ###########################################
  ##            Main Heatmap
  ###########################################
  
  output$columnToAnnotate <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(v$dataToPlot)){
      return()
    }else{
      cnames <- colnames(v$dataToPlot)
      cnames <- cnames[!cnames%in%c("ID", "count")]
      if(length(cnames)>0){
        selectInput('annotateMainHeatmap', h4('Add column:'), choices=c("None",cnames), selected = "None")
      }else{
        "No column found"
      }
    }
  })
  
  output$columnToSort <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(v$dataToPlot)){
      return()
    }else{
      cnames <- colnames(v$dataToPlot)
      cnames <- cnames[!cnames%in%c("ID", "count")]
      if(length(cnames)>0){
        selectInput('sortMainHeatMap', h4('Sort by:'), choices=c("None",cnames), selected = "None")
      }else{
        "No column found"
      }
    }
  })
  
  ## Main heatmap parameters
  observeEvent(input$mainHeatmapParams, {
    v$annotationColumn = input$annotateMainHeatmap
    v$sortColumn = input$sortMainHeatMap
    v$mainHeatmapTitle = input$mainHeatmapTitle
    v$mainHeatmapTitlefont = input$mainHeatmapTitlefont
    v$mainHeatmapYfont = input$mainHeatmapYfont
    v$mainHeatmapXfont = input$mainHeatmapXfont
  })
  
  ## Main heatmap
  mainHeatmap <- reactive({
     if(is.null(v$dataToPlot)){
       return(NULL)
     }else{
       df <- v$dataToPlot
       x = as.matrix(df%>%dplyr::select(-ID, -count, -percenttotal)%>%data.frame())
       rownames(x) = df$ID
       if((!is.null(v$dataToPlot)) & is.null(v$annotationColumn) & is.null(v$sortColumn)){
         h1 = Heatmap(x,
                      col = colorRamp2(breaks=seq(min(x),max(x), (max(x)-min(x))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                      row_names_gp = gpar(fontsize = v$mainHeatmapYfont),
                      column_names_gp = gpar(fontsize = v$mainHeatmapXfont),
                      column_title = paste0("SPADE node medians", "\n",v$mainHeatmapTitle),
                      column_title_gp = gpar(fontsize = v$mainHeatmapTitlefont),
                      row_dend_width = unit(2, "cm"),
                      column_dend_height = unit(2, "cm"),
                      heatmap_legend_param = list(title = paste0("Expression ", v$groupSelected),
                                                  title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                  color_bar = "continuous",
                                                  grid_height = unit(6, "mm"),
                                                  grid_width = unit(6, "mm"),
                                                  labels_gp = gpar(fontsize = 12)))
  
         draw(h1)
         v$mainHeatmap = h1
         v$mainHeatmapRowOrder = unlist(row_order(h1))
         v$mainHeatmapColumnOrder = unlist(column_order(h1))
       }else if( !is.null(v$dataToPlot) & !is.null(v$annotationColumn) & !is.null(v$sortColumn)){
         if(v$annotationColumn!="None" & v$sortColumn=="None"){
           h1 = Heatmap(x,
                        col = colorRamp2(breaks=seq(min(x),max(x), (max(x)-min(x))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                        row_names_gp = gpar(fontsize = v$mainHeatmapYfont),
                        column_names_gp = gpar(fontsize = v$mainHeatmapXfont),
                        column_title = paste0("SPADE node medians", "\n",v$mainHeatmapTitle),
                        column_title_gp = gpar(fontsize = v$mainHeatmapTitlefont),
                        row_dend_width = unit(2, "cm"),
                        column_dend_height = unit(2, "cm"),
                        heatmap_legend_param = list(title = paste0("Expression ", v$groupSelected),
                                                    title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                    color_bar = "continuous",
                                                    grid_height = unit(6, "mm"),
                                                    grid_width = unit(6, "mm"),
                                                    labels_gp = gpar(fontsize = 12)))
           
           annot = df[,v$annotationColumn]
           h2 = Heatmap(annot, name = v$annotationColumn,
                        col = colorRamp2(breaks=seq(min(annot),max(annot), (max(annot)-min(annot))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                        show_row_names = FALSE,
                        heatmap_legend_param = list(title = paste0(v$annotationColumn, " ", v$groupSelected),
                                                    title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                    color_bar = "continuous",
                                                    grid_height = unit(6, "mm"),
                                                    grid_width = unit(6, "mm"),
                                                    labels_gp = gpar(fontsize = 12)),
                        width = unit(6, "mm"))
    
           draw(h1 + h2)
           v$mainHeatmap = h1 + h2
           v$mainHeatmapRowOrder = unlist(row_order(h1))
           v$mainHeatmapColumnOrder = unlist(column_order(h1))
       }else if(v$annotationColumn=="None" & v$sortColumn=="None"){
         h1 = Heatmap(x,
                      col = colorRamp2(breaks=seq(min(x),max(x), (max(x)-min(x))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                      row_names_gp = gpar(fontsize = v$mainHeatmapYfont),
                      column_names_gp = gpar(fontsize = v$mainHeatmapXfont),
                      column_title = paste0("SPADE node medians", "\n",v$mainHeatmapTitle),
                      column_title_gp = gpar(fontsize = v$mainHeatmapTitlefont),
                      row_dend_width = unit(2, "cm"),
                      column_dend_height = unit(2, "cm"),
                      heatmap_legend_param = list(title = paste0("Expression ", v$groupSelected),
                                                  title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                  color_bar = "continuous",
                                                  grid_height = unit(6, "mm"),
                                                  grid_width = unit(6, "mm"),
                                                  labels_gp = gpar(fontsize = 12)))
         
         draw(h1)
         v$mainHeatmap = h1
         v$mainHeatmapRowOrder = unlist(row_order(h1))
         v$mainHeatmapColumnOrder = unlist(column_order(h1))
       }else if(v$annotationColumn=="None" & v$sortColumn!="None"){
         y = df %>% arrange_(v$sortColumn)
         rnames = y$ID
         y = as.matrix(y%>%dplyr::select(-ID, -count, -percenttotal)%>%data.frame())
         rownames(y) = rnames
         h1 = Heatmap(y,
                      col = colorRamp2(breaks=seq(min(y),max(y), (max(y)-min(y))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                      cluster_rows = F, cluster_columns = F,
                      row_names_gp = gpar(fontsize = v$mainHeatmapYfont),
                      column_names_gp = gpar(fontsize = v$mainHeatmapXfont),
                      column_title = paste0("SPADE node medians", "\n",v$mainHeatmapTitle),
                      column_title_gp = gpar(fontsize = v$mainHeatmapTitlefont),
                      row_dend_width = unit(2, "cm"),
                      column_dend_height = unit(2, "cm"),
                      heatmap_legend_param = list(title = paste0("Expression ", v$groupSelected),
                                                  title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                  color_bar = "continuous",
                                                  grid_height = unit(6, "mm"),
                                                  grid_width = unit(6, "mm"),
                                                  labels_gp = gpar(fontsize = 12)))
         
         draw(h1)
         v$mainHeatmap = h1
         v$mainHeatmapRowOrder = unlist(row_order(h1))
         v$mainHeatmapColumnOrder = unlist(column_order(h1))
       }else if(v$annotationColumn!="None" & v$sortColumn!="None"){
          y = df %>% arrange_(v$sortColumn)
          rnames = y$ID
          y = as.matrix(y%>%dplyr::select(-ID, -count, -percenttotal)%>%data.frame())
          rownames(y) = rnames
          h1 = Heatmap(y,
                      col = colorRamp2(breaks=seq(min(y),max(y), (max(y)-min(y))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                      cluster_rows = F, cluster_columns = F,
                      row_names_gp = gpar(fontsize = v$mainHeatmapYfont),
                      column_names_gp = gpar(fontsize = v$mainHeatmapXfont),
                      column_title = paste0("SPADE node medians", "\n",v$mainHeatmapTitle),
                      column_title_gp = gpar(fontsize = v$mainHeatmapTitlefont),
                      row_dend_width = unit(2, "cm"),
                      column_dend_height = unit(2, "cm"),
                      heatmap_legend_param = list(title = paste0("Expression ", v$groupSelected),
                                                  title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                  color_bar = "continuous",
                                                  grid_height = unit(6, "mm"),
                                                  grid_width = unit(6, "mm"),
                                                  labels_gp = gpar(fontsize = 12)))
          annot = df[,v$annotationColumn]
          h2 = Heatmap(annot, name = v$annotationColumn,
                      col = colorRamp2(breaks=seq(min(annot),max(annot), (max(annot)-min(annot))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                      show_row_names = FALSE,
                      heatmap_legend_param = list(title = paste0(v$annotationColumn, " ", v$groupSelected),
                                                  title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                  color_bar = "continuous",
                                                  grid_height = unit(6, "mm"),
                                                  grid_width = unit(6, "mm"),
                                                  labels_gp = gpar(fontsize = 12)),
                      width = unit(6, "mm"))
          
          draw(h1 + h2)
          v$mainHeatmap = h1 + h2
          v$mainHeatmapRowOrder = unlist(row_order(h1))
          v$mainHeatmapColumnOrder = unlist(column_order(h1))
       }
      }
     }
   })

   output$mainHeatmap <- renderPlot({
     mainHeatmap()
   },height = 1020)
  
   output$downloadMainHeatmap <- downloadHandler(
     filename = function() {
       paste('heatmap_clustering', Sys.Date(), '.pdf', sep='')
     },
  
     content = function(con) {
       pdf(con, width = 20, height = 20)
       draw(mainHeatmap())
       dev.off()
     }
   )
   
   output$downloadDataMainHeatmap <- downloadHandler(
     filename = function() {
       paste('data_clustering', Sys.Date(), '.tsv', sep='')
     },
     content = function(con) {
       write.table(v$dataToPlot, file=con, quote = F, row.names = F, sep = "\t")
     }
   )
   
   ##############################################
   ##           Forced Heatmap
   ##############################################
   
   output$selectForcedGroup <- renderUI({
     if(!is.null(v$mainData) & v$singleSample==FALSE){
       selectInput('forcedGroup', '', width="250px", choices=c("None",unique(v$mainData$group)), selected = "None")
     }else{
       return()
     }
   })
   
   ## Get the reference and the second group  
   observeEvent(input$forceHeatmap, {
     v$forcedGroup = input$forcedGroup
     ## Get the data for the second group
     v$dataToPlot_forced = v$mainData %>% subset(group==v$forcedGroup) %>% 
       select(-group, -fn) %>% group_by(ID) %>% summarise_each(funs(median)) %>% ungroup
     v$dataToPlot_forced = v$dataToPlot_forced %>% dplyr::select(ID, count, one_of(v$clusterColumns), percenttotal)
     ## Fix rows according to the main heatmap
     colorder = colnames(v$dataToPlot%>%dplyr::select(-ID, -count, -percenttotal))[v$mainHeatmapColumnOrder]
     v$dataToPlot_forced = v$dataToPlot_forced[,c("ID", "count", colorder, "percenttotal")]
   })
   
   ## Main heatmap parameters
   observeEvent(input$forcedHeatmapParams, {
     v$forcedHeatmapTitle = input$forcedHeatmapTitle
     v$forcedHeatmapTitlefont = input$forcedHeatmapTitlefont
     v$forcedHeatmapYfont = input$forcedHeatmapYfont
     v$forcedHeatmapXfont = input$forcedHeatmapXfont
   })
   
   forcedHeatmap <- reactive({
     if(is.null(v$dataToPlot_forced)){
       return(NULL)
     }else{
       df <- v$dataToPlot_forced
       x = as.matrix(df%>%dplyr::select(-ID, -count, -percenttotal)%>%data.frame())
       rownames(x) = df$ID
       if((!is.null(v$dataToPlot_forced)) & is.null(v$annotationColumn) & is.null(v$sortColumn)){
         h1 = Heatmap(x,
                      col = colorRamp2(breaks=seq(min(x),max(x), (max(x)-min(x))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                      row_names_gp = gpar(fontsize = v$forcedHeatmapYfont),
                      column_names_gp = gpar(fontsize = v$forcedHeatmapXfont),
                      column_title = paste0("SPADE node medians", "\n",v$forcedHeatmapTitle),
                      column_title_gp = gpar(fontsize = v$forcedHeatmapTitlefont),
                      cluster_rows = FALSE,
                      cluster_columns = FALSE,
                      heatmap_legend_param = list(title = paste0("Expression ", v$forcedGroup),
                                                  title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                  color_bar = "continuous",
                                                  grid_height = unit(6, "mm"),
                                                  grid_width = unit(6, "mm"),
                                                  labels_gp = gpar(fontsize = 12)))

         draw(v$mainHeatmap + h1)
         v$forcedHeatmap = h1
       }else if( !is.null(v$dataToPlot_forced) & !is.null(v$annotationColumn) & !is.null(v$sortColumn)){
         if(v$annotationColumn!="None" & v$sortColumn=="None"){
           h1 = Heatmap(x,
                        col = colorRamp2(breaks=seq(min(x),max(x), (max(x)-min(x))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                        row_names_gp = gpar(fontsize = v$forcedHeatmapYfont),
                        column_names_gp = gpar(fontsize = v$forcedHeatmapXfont),
                        column_title = paste0("SPADE node medians", "\n",v$forcedHeatmapTitle),
                        column_title_gp = gpar(fontsize = v$forcedHeatmapTitlefont),
                        cluster_rows = FALSE,
                        cluster_columns = FALSE,
                        heatmap_legend_param = list(title = paste0("Expression ", v$forcedGroup),
                                                    title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                    color_bar = "continuous",
                                                    grid_height = unit(6, "mm"),
                                                    grid_width = unit(6, "mm"),
                                                    labels_gp = gpar(fontsize = 12)))
           annot = df[,v$annotationColumn]
           h2 = Heatmap(annot, name = v$annotationColumn,
                        col = colorRamp2(breaks=seq(min(annot),max(annot), (max(annot)-min(annot))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                        show_row_names = FALSE,
                        heatmap_legend_param = list(title = paste0(v$annotationColumn, " ", v$forcedGroup),
                                                    title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                    color_bar = "continuous",
                                                    grid_height = unit(6, "mm"),
                                                    grid_width = unit(6, "mm"),
                                                    labels_gp = gpar(fontsize = 12)),
                        width = unit(6, "mm"))

           draw(v$mainHeatmap + h1 + h2)
           v$forcedHeatmap = h1 + h2
         }else if(v$annotationColumn=="None" & v$sortColumn=="None"){
           h1 = Heatmap(x,
                        col = colorRamp2(breaks=seq(min(x),max(x), (max(x)-min(x))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                        row_names_gp = gpar(fontsize = v$forcedHeatmapYfont),
                        column_names_gp = gpar(fontsize = v$forcedHeatmapXfont),
                        column_title = paste0("SPADE node medians", "\n",v$forcedHeatmapTitle),
                        column_title_gp = gpar(fontsize = v$forcedHeatmapTitlefont),
                        cluster_rows = FALSE,
                        cluster_columns = FALSE,
                        heatmap_legend_param = list(title = paste0("Expression ", v$forcedGroup),
                                                    title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                    color_bar = "continuous",
                                                    grid_height = unit(6, "mm"),
                                                    grid_width = unit(6, "mm"),
                                                    labels_gp = gpar(fontsize = 12)))
           
           draw(v$mainHeatmap + h1)
           v$forcedHeatmap = h1
         }else if(v$annotationColumn=="None" & v$sortColumn!="None"){
           y = df %>% arrange_(v$sortColumn)
           rnames = y$ID
           y = as.matrix(y%>%dplyr::select(-ID, -count, -percenttotal)%>%data.frame())
           rownames(y) = rnames
           h1 = Heatmap(y,
                        col = colorRamp2(breaks=seq(min(y),max(y), (max(y)-min(y))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                        cluster_rows = F, cluster_columns = F,
                        row_names_gp = gpar(fontsize = v$forcedHeatmapYfont),
                        column_names_gp = gpar(fontsize = v$forcedHeatmapXfont),
                        column_title = paste0("SPADE node medians", "\n",v$forcedHeatmapTitle),
                        column_title_gp = gpar(fontsize = v$forcedHeatmapTitlefont),
                        row_dend_width = unit(2, "cm"),
                        column_dend_height = unit(2, "cm"),
                        heatmap_legend_param = list(title = paste0("Expression ", v$forcedGroup),
                                                    title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                    color_bar = "continuous",
                                                    grid_height = unit(6, "mm"),
                                                    grid_width = unit(6, "mm"),
                                                    labels_gp = gpar(fontsize = 12)))
           
           draw(v$mainHeatmap + h1)
           v$forcedHeatmap = h1
         }else if(v$annotationColumn!="None" & v$sortColumn!="None"){
           y = df %>% arrange_(v$sortColumn)
           rnames = y$ID
           y = as.matrix(y%>%dplyr::select(-ID, -count, -percenttotal)%>%data.frame())
           rownames(y) = rnames
           h1 = Heatmap(y,
                        col = colorRamp2(breaks=seq(min(y),max(y), (max(y)-min(y))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                        cluster_rows = F, cluster_columns = F,
                        row_names_gp = gpar(fontsize = v$forcedHeatmapYfont),
                        column_names_gp = gpar(fontsize = v$forcedHeatmapXfont),
                        column_title = paste0("SPADE node medians", "\n",v$forcedHeatmapTitle),
                        column_title_gp = gpar(fontsize = v$forcedHeatmapTitlefont),
                        row_dend_width = unit(2, "cm"),
                        column_dend_height = unit(2, "cm"),
                        heatmap_legend_param = list(title = paste0("Expression ", v$forcedGroup),
                                                    title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                    color_bar = "continuous",
                                                    grid_height = unit(6, "mm"),
                                                    grid_width = unit(6, "mm"),
                                                    labels_gp = gpar(fontsize = 12)))
           annot = df[,v$annotationColumn]
           h2 = Heatmap(annot, name = v$annotationColumn,
                        show_row_names = FALSE,
                        col = colorRamp2(breaks=seq(min(annot),max(annot), (max(annot)-min(annot))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                        heatmap_legend_param = list(title = paste0(v$annotationColumn, " ", v$forcedGroup),
                                                    title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                    color_bar = "continuous",
                                                    grid_height = unit(6, "mm"),
                                                    grid_width = unit(6, "mm"),
                                                    labels_gp = gpar(fontsize = 12)),
                        width = unit(6, "mm"))
           
           draw(v$mainHeatmap + h1 + h2)
           v$forcedHeatmap = h1 + h2
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
       pdf(con, width = 20, height = 20)
       draw(forcedHeatmap())
       dev.off()
     }
   )
   
   output$downloadDataForced <- downloadHandler(
     filename = function() {
       paste('data_forced_heatmap', Sys.Date(), '.tsv', sep='')
     },
     content = function(con) {
       write.table(v$dataToPlot_forced, file=con, quote = F, row.names = F, sep = "\t")
     }
   )
   
   ######################
   ##  Post-processing
   ######################
   
   observeEvent(input$postProc, {
     marker1 = input$marker1
     marker2 = input$marker2
     marker3 = input$marker3
     v$postHeatmapTitle = input$postHeatmapTitle
     v$postHeatmapTitlefont = input$postHeatmapTitlefont
     v$postHeatmapYfont = input$postHeatmapYfont
     v$postHeatmapXfont = input$postHeatmapXfont
     
     if(!is.null(v$dataToPlot)  &
        (marker1!="" | marker2!="" | marker3!="") &
        (!is.null(marker1) | !is.null(marker2) | !is.null(marker3)) ){
       
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
   
   postHeatmap <- reactive({
     if (is.null(v$dataPostProc)){
       return(NULL)
     }else{
       d = v$dataPostProc
       x = as.matrix(d%>%dplyr::select(-ID, -count, -percenttotal, -type)%>%data.frame())
       rownames(x) = d$ID
       
       h1 = Heatmap(x,
                    col = colorRamp2(breaks=seq(min(x),max(x), (max(x)-min(x))/(input$colorBreaks-1)),colors = bluered(input$colorBreaks)),
                    row_names_gp = gpar(fontsize = v$postHeatmapYfont),
                    column_names_gp = gpar(fontsize = v$postHeatmapXfont),
                    column_title = v$postHeatmapTitle,
                    column_title_gp = gpar(fontsize = v$postHeatmapTitlefont),
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
   
   output$postHeatmap <- renderPlot({
     postHeatmap()
   },height = 1020)
   
   output$downloadPostHeatmap <- downloadHandler(
     filename = function() {
       paste('heatmap_post_processing', Sys.Date(), '.pdf', sep='')
     },
     
     content = function(con) {
       pdf(con, width = 20, height = 20)
       draw(postHeatmap())
       dev.off()
     }
   )
   
   
   ##################################
   ##       Overlay heatmaps
   ##################################
   
   observe({
     if(!is.null(v$dataToPlot) & !is.null(v$dataToPlot_forced)){
       x = v$dataToPlot %>% gather(marker, expr_main, -ID)
       y = v$dataToPlot_forced %>% gather(marker, expr_forced, -ID)
       ## Merge the two groups
       x = x %>% left_join(y)
       ## Do the abstraction
       x = x %>% mutate(diff=expr_main-expr_forced)
       ## Expand the data frame to plot it
       x = x %>% select(ID, marker, diff) %>% spread(marker, diff)
       v$dataToPlot_overlay = x %>% data.frame()
     }
   })
   
   ## Main heatmap parameters
   observeEvent(input$overlayHeatmapParams, {
     v$overlayHeatmapTitle = input$overlayHeatmapTitle
     v$overlayHeatmapTitlefont = input$overlayHeatmapTitlefont
     v$overlayHeatmapYfont = input$overlayHeatmapYfont
     v$overlayHeatmapXfont = input$overlayHeatmapXfont
   })
   
   overlayHeatmap <- reactive({
     if(is.null(v$dataToPlot_overlay)){
       return(NULL)
     }else{
       ## Order columns
       colorder = colnames(v$dataToPlot%>%dplyr::select(-ID, -count, -percenttotal))[v$mainHeatmapColumnOrder]
       v$dataToPlot_overlay = v$dataToPlot_overlay[,c("ID", "count", colorder, "percenttotal")]
       df <- v$dataToPlot_overlay
       x = as.matrix(df%>%dplyr::select(-ID, -count, -percenttotal)%>%data.frame())
       rownames(x) = df$ID
       if((!is.null(v$dataToPlot_overlay)) & is.null(v$annotationColumn) & is.null(v$sortColumn)){
         h1 = Heatmap(x,
                      #col = colorRamp2(breaks=seq(min(x),max(x), (max(x)-min(x))/(input$colorBreaks-1)),colors = greenred(input$colorBreaks)),
                      col = colorRamp2(breaks=c(min(x),0, max(x)),colors = c("green", "black", "red")),
                      row_names_gp = gpar(fontsize = v$overlayHeatmapYfont),
                      column_names_gp = gpar(fontsize = v$overlayHeatmapXfont),
                      column_title = paste0("SPADE node medians", "\n",v$overlayHeatmapTitle),
                      column_title_gp = gpar(fontsize = v$overlayHeatmapTitlefont),
                      cluster_rows = FALSE,
                      cluster_columns = FALSE,
                      heatmap_legend_param = list(title = paste0("Expression difference"),
                                                  title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                  color_bar = "continuous",
                                                  grid_height = unit(6, "mm"),
                                                  grid_width = unit(6, "mm"),
                                                  labels_gp = gpar(fontsize = 12)))
         
         draw(v$mainHeatmap + v$forcedHeatmap + h1)
       }else if( !is.null(v$dataToPlot_overlay) & !is.null(v$annotationColumn) & !is.null(v$sortColumn)){
         if(v$annotationColumn!="None" & v$sortColumn=="None"){
           h1 = Heatmap(x,
                        #col = colorRamp2(breaks=seq(min(x),max(x), (max(x)-min(x))/(input$colorBreaks-1)),colors = greenred(input$colorBreaks)),
                        col = colorRamp2(breaks=c(min(x),0, max(x)),colors = c("green", "black", "red")),
                        row_names_gp = gpar(fontsize = v$overlayHeatmapYfont),
                        column_names_gp = gpar(fontsize = v$overlayHeatmapXfont),
                        column_title = paste0("SPADE node medians", "\n",v$overlayHeatmapTitle),
                        column_title_gp = gpar(fontsize = v$overlayHeatmapTitlefont),
                        cluster_rows = FALSE,
                        cluster_columns = FALSE,
                        heatmap_legend_param = list(title = paste0("Expression difference"),
                                                    title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                    color_bar = "continuous",
                                                    grid_height = unit(6, "mm"),
                                                    grid_width = unit(6, "mm"),
                                                    labels_gp = gpar(fontsize = 12)))
           annot = df[,v$annotationColumn]
           h2 = Heatmap(annot, name = v$annotationColumn,
                        #col = colorRamp2(breaks=seq(min(annot),max(annot), (max(annot)-min(annot))/(input$colorBreaks-1)),colors = greenred(input$colorBreaks)),
                        col = colorRamp2(breaks=c(min(annot),0, max(annot)),colors = c("green", "black", "red")),
                        show_row_names = FALSE,
                        heatmap_legend_param = list(title = paste0(v$annotationColumn, " difference"),
                                                    title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                    color_bar = "continuous",
                                                    grid_height = unit(6, "mm"),
                                                    grid_width = unit(6, "mm"),
                                                    labels_gp = gpar(fontsize = 12)),
                        width = unit(6, "mm"))
           
           draw(v$mainHeatmap + v$forcedHeatmap + h1 + h2)
         }else if(v$annotationColumn=="None" & v$sortColumn=="None"){
           h1 = Heatmap(x,
                        #col = colorRamp2(breaks=seq(min(x),max(x), (max(x)-min(x))/(input$colorBreaks-1)),colors = greenred(input$colorBreaks)),
                        col = colorRamp2(breaks=c(min(x),0, max(x)),colors = c("green", "black", "red")),
                        row_names_gp = gpar(fontsize = v$overlayHeatmapYfont),
                        column_names_gp = gpar(fontsize = v$overlayHeatmapXfont),
                        column_title = paste0("SPADE node medians", "\n",v$overlayHeatmapTitle),
                        column_title_gp = gpar(fontsize = v$overlayHeatmapTitlefont),
                        cluster_rows = FALSE,
                        cluster_columns = FALSE,
                        heatmap_legend_param = list(title = paste0("Expression difference"),
                                                    title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                    color_bar = "continuous",
                                                    grid_height = unit(6, "mm"),
                                                    grid_width = unit(6, "mm"),
                                                    labels_gp = gpar(fontsize = 12)))
           
           draw(v$mainHeatmap + v$forcedHeatmap + h1)
           
         }else if(v$annotationColumn=="None" & v$sortColumn!="None"){
           y = df %>% arrange_(v$sortColumn)
           rnames = y$ID
           y = as.matrix(y%>%dplyr::select(-ID, -count, -percenttotal)%>%data.frame())
           rownames(y) = rnames
           h1 = Heatmap(y,
                        #col = colorRamp2(breaks=seq(min(y),max(y), (max(y)-min(y))/(input$colorBreaks-1)),colors = greenred(input$colorBreaks)),
                        col = colorRamp2(breaks=c(min(y),0, max(y)),colors = c("green", "black", "red")),
                        cluster_rows = F, cluster_columns = F,
                        row_names_gp = gpar(fontsize = v$overlayHeatmapYfont),
                        column_names_gp = gpar(fontsize = v$overlayHeatmapXfont),
                        column_title = paste0("SPADE node medians", "\n",v$overlayHeatmapTitle),
                        column_title_gp = gpar(fontsize = v$overlayHeatmapTitlefont),
                        row_dend_width = unit(2, "cm"),
                        column_dend_height = unit(2, "cm"),
                        heatmap_legend_param = list(title = paste0("Expression difference"),
                                                    title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                    color_bar = "continuous",
                                                    grid_height = unit(6, "mm"),
                                                    grid_width = unit(6, "mm"),
                                                    labels_gp = gpar(fontsize = 12)))
           
           draw(v$mainHeatmap + v$forcedHeatmap + h1)
         }else if(v$annotationColumn!="None" & v$sortColumn!="None"){
           y = df %>% arrange_(v$sortColumn)
           rnames = y$ID
           y = as.matrix(y%>%dplyr::select(-ID, -count, -percenttotal)%>%data.frame())
           rownames(y) = rnames
           h1 = Heatmap(y,
                        #col = colorRamp2(breaks=seq(min(y),max(y), (max(y)-min(y))/(input$colorBreaks-1)),colors = greenred(input$colorBreaks)),
                        col = colorRamp2(breaks=c(min(y),0, max(y)),colors = c("green", "black", "red")),
                        cluster_rows = F, cluster_columns = F,
                        row_names_gp = gpar(fontsize = v$overlayHeatmapYfont),
                        column_names_gp = gpar(fontsize = v$overlayHeatmapXfont),
                        column_title = paste0("SPADE node medians", "\n",v$overlayHeatmapTitle),
                        column_title_gp = gpar(fontsize = v$overlayHeatmapTitlefont),
                        row_dend_width = unit(2, "cm"),
                        column_dend_height = unit(2, "cm"),
                        heatmap_legend_param = list(title = paste0("Expression difference"),
                                                    title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                    color_bar = "continuous",
                                                    grid_height = unit(6, "mm"),
                                                    grid_width = unit(6, "mm"),
                                                    labels_gp = gpar(fontsize = 12)))
           annot = df[,v$annotationColumn]
           h2 = Heatmap(annot, name = v$annotationColumn,
                        #col = colorRamp2(breaks=seq(min(annot),max(annot), (max(annot)-min(annot))/(input$colorBreaks-1)),colors = greenred(input$colorBreaks)),
                        col = colorRamp2(breaks=c(min(annot),0, max(annot)),colors = c("green", "black", "red")),
                        show_row_names = FALSE,
                        heatmap_legend_param = list(title = paste0(v$annotationColumn, " difference"),
                                                    title_gp = gpar(fontsize = 12, fontface = "bold"),
                                                    color_bar = "continuous",
                                                    grid_height = unit(6, "mm"),
                                                    grid_width = unit(6, "mm"),
                                                    labels_gp = gpar(fontsize = 12)),
                        width = unit(6, "mm"))
           
           draw(v$mainHeatmap + v$forcedHeatmap + h1 + h2)
         }
       }
     }
   })
   
   output$overlayHeatmap <- renderPlot({
     overlayHeatmap()
   },height = 1020)
   
   output$downloadOverlayHeatmap <- downloadHandler(
     filename = function() { 
       paste('overlay_heatmap', Sys.Date(), '.pdf', sep='') 
     },
     
     content = function(con) {
       pdf(con, width = 20, height = 20)
       draw(overlayHeatmap())
       dev.off()
     }
   )
   
   output$downloadDataOverlay <- downloadHandler(
     filename = function() {
       paste('data_overlay_heatmap', Sys.Date(), '.tsv', sep='')
     },
     content = function(con) {
       write.table(v$dataToPlot_overlay, file=con, quote = F, row.names = F, sep = "\t")
     }
   )
   
   ## Table for node identification
   output$nodeGroupstb = renderRHandsontable({
     if(is.null(v$dataToPlot)){
       return()
     }else{
       ids = unique(v$dataToPlot$ID)
       rhandsontable(data.frame(Node=as.character(ids), group=rep("", length(ids))), rowHeaders = NULL) %>%
         hot_table(highlightCol = TRUE, highlightRow = TRUE)
     }
   })
   
   # observeEvent(input$submitNodeGroups, {
   #   nodeGroups = hot_to_r(input$nodeGroupstb)
   #   nodeGroups = nodeGroups %>% subset(group!="")
   #   bubble_list = list()
   #   for(g in unique(nodeGroups$group)){
   #     nodes = nodeGroups %>% subset(group==g) %>% .$Node %>% unique
   #     bubble_list[[g]] = nodes
   #   }
   # 
   #   ## Check if we are connected to Cytobank
   #   if(!is.null(v$cyto_session)){
   # 
   #     spade.bubbles_set(v$cyto_session, 
   #                       spade=v$cyto_spade, 
   #                       bubbles=bubble_list)
   #     my_spade_check_test <- "Your SPADE bubbles have been set"
   #     js_string <- 'alert("SOMETHING");'
   #     js_string <- sub("SOMETHING",my_spade_check_test,js_string)
   #     session$sendCustomMessage(type='jsCode', list(value = js_string))
   #   }
   # })
  
})
