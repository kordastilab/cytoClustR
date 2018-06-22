## Install/Load dependencies

## First check if java is installed

# tryCatch(system('pandoc -v', intern = T))
# possibleError <- tryCatch(library(rJava), error=function(e) e)
# 
# if(!inherits(possibleError, "error")){
# 


pkgsR  =  c("shiny",
            "xlsx",
            "tidyr",
            "DT",
            "RColorBrewer",
            "ggdendro",
            "dendsort",
            "grid",
            "gridExtra",
            "igraph",
            "circlize",
            "gplots",
            "shinyFiles",
            "shinydashboard",
            "shinyjs",
            "devtools",
            "fpc",
            "cluster",
            "readr",
            "CytobankAPI",
            "stringr",
            "dplyr",
            "tibble",
            "Rcpp",
            "gtools",
            "stringi")
for (pkgR in pkgsR) 
  if (!pkgR %in% rownames(installed.packages())) { 
    install.packages(pkgR)
    library(pkgR, character.only = TRUE, quietly = TRUE)
  } else { 
    library(pkgR, character.only = TRUE, quietly = TRUE)
  }

source("http://bioconductor.org/biocLite.R")

pkgs  =  c("ComplexHeatmap")
for (pkg in pkgs) 
  if (!pkg %in% rownames(installed.packages())) { 
    biocLite(pkg)
    library(pkg, character.only = TRUE, quietly = TRUE)
  } else { 
    library(pkg, character.only = TRUE, quietly = TRUE)
  }

#update.packages(repos=biocinstallRepos(), ask=FALSE)

## Install github repos
rm(pkg,pkgR,pkgs,pkgsR)
gits = c("trestletech/shinyTable",
         "jrowen/rhandsontable",
         "ayayron/shinydnd")
for(git in gits){
    g = unlist(strsplit(git, "/"))[2]
    if(git=="ayayron/shinydnd"){
      g = "shinyDND"
    }
    if (!(g %in% rownames(installed.packages()))){
        install_github(git)
        library(g, character.only = TRUE, quietly = TRUE)
    }else{
        library(g, character.only = TRUE, quietly = TRUE)
    }
}

rm(g, git, gits)

## And options
options(stringsAsFactors=FALSE)
