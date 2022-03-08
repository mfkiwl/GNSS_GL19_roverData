### HEADER #####################################################################
##'  
##' @title        Read GNSS data from TBC excel outputs into R
##' @author       Urs A. Treier
##' @contact      urs.treier at gmail.com
##' @date         06/10/2021
##' @description  This script will import and process "PointList" excel export
##'               files from Trimble Business Center (TBC). Thereafter the point
##'               lists and metadata (e.g., coordinate system, datum, and geoid
##'               model) will be exported as a tab delimited text file.
##'                 The user defined target directory and its sub-directories 
##'               will be searched for *.xls files (unless files are placed in a 
##'               folder named "DoNotUse"). If these files have the user defined
##'               identifier in their file name, they will be processed. If the
##'               "EPSG_database.txt" is provided with the script, additional
##'               metadata is added (e.g. EPSG).    
##' @note         DEFINITION OF VARIABLES section: Define appropriately.
##' @log          During data processing logs files will be written and saved in
##'               the tmp folder created by the script within each of the folder
##'               with a processed file.
##' @dependencies readxl
##' @licencing    GPL (see GNU General Public License at
##'               <http://www.gnu.org/licenses/>)
##'               Copyright (C) @date  Urs A. Treier
##'                 This program is free software: you can redistribute it
##'               and/or modify it under the terms of the GNU General Public
##'               License as published by the Free Software Foundation, either
##'               version 3 of the License, or (at your option) any later
##'               version.
##'                 This program is distributed in the hope that it will be
##'               useful,but WITHOUT ANY WARRANTY; without even the implied
##'               warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
##'               PURPOSE. See the GNU General Public License for more details.
##'                 You should have received a copy of the GNU General Public
##'               License along with this program. If not, see
##'               <http://www.gnu.org/licenses/>.
##'           
### END OF HEADER ##############################################################
##'_____________________________________________________________________________
##'
### DEFINITION OF VARIABLES ####################################################
##'_____________________________________________________________________________
##'
##' !! MODIFY WHERE NECESSARY !!
##'_____________________________________________________________________________
##'
if(!exists("target.directory")){
  rm(list = ls())
  ##'
  ##' define the target directory
  target.directory <- paste0("C:/Users/Urs/OneDrive/DataUrs",
                             "/_ac_Projects/Current/DroneEcol",
                             "/R-WorkingDirectory/GNSS_Processing",
                             "/GNSS_GL_2019/GNSS_GL19_roverData")
}
##'
if(!exists("pointlist.file")){
  ##'
  ##'  define the point list file identifier
  file.identifyer <- "PointList"
  ##'
}else{file.identifyer  <- pointlist.file}
##'
##' selects worksheet name in the excel point list files
workbook.sheets  <- c("PointReport")
##'
### END OF DEFINITION OF VARIABLES #############################################
##'_____________________________________________________________________________
##'
### LOAD FUNCTIONS #############################################################
##'_____________________________________________________________________________
##'
##' function that installs and loads required packages
loadPackages <- function(Pkgs){
  new.Pkgs <- Pkgs[!(Pkgs %in% installed.packages()[, "Package"])]
  if (length(new.Pkgs) > 0){
    install.packages(new.Pkgs, dependencies = TRUE)
  }  
  sapply(Pkgs, require, character.only = TRUE)
} 
##' 
##' function that converts coordinates from DMS to Decimal Degrees (DD)
ConvertCoordinates<-function(c) {
  #options(digits=16)
  D_chars <- paste0("od", intToUtf8(c(176, 186, 9702)))
  M_chars <- paste0("m'\u2019", intToUtf8(c(96, 180)))
  S_chars <- paste0("\"'\u2019", intToUtf8(c(96, 180)))
  x <- gsub("[,]", ".", c)
  x <- as.character(sub("[NSE?WVnse?wvm]", "", x))
  for(i in 1:length(x)) {
    y <- x[i]
    if(!is.na(suppressWarnings(as.numeric(y)))) y <- as.numeric(y) 
    if(is.numeric(y)){
      if(grepl("[SsWwVv]",c[i])) {y <- as.numeric(paste0("-",y))
      }
      x[i] <- y
    } else {
      #y  <- as.character(sub("[NSE?WVnse?wvm\"]","",y))
      df.xi <- strsplit(y,paste0("[", D_chars, M_chars, "?]"))
      df.xi <- sapply(strsplit(y,paste0("[", D_chars, M_chars, "?]")),
                      as.character)
      if(!is.na(y) & is.null(nrow(df.xi))) {
        warning(paste0("Check for inconsistencies/errors in the input data!\n",
                       "Some of the data have not been converted."))
      } else { 
        if(!is.null(nrow(df.xi) > 2) && length(nrow(df.xi) > 2) == 1 &&
           !is.na(nrow(df.xi) > 2) && nrow(df.xi) > 2){
          y <- (as.numeric(df.xi[1, ]) +
                  as.numeric(df.xi[2, ])/60 +
                  as.numeric(sub(paste0("[", S_chars, "]"), "",
                                 df.xi[3, ]))/3600)
        }
        if(!is.null(nrow(df.xi) == 2) && length(nrow(df.xi) == 2) == 1 &&
           !is.na(nrow(df.xi) == 2) && nrow(df.xi) == 2) {
          y <- as.numeric(df.xi[1, ]) + as.numeric(df.xi[2, ]/60)
        }
        if(grepl("[SsWwVv]", c[i])) {
          y <- as.numeric(paste0("-",y))
        }
        x[i] <- y
      }
    }
  }
  return(as.numeric(x))
}
##'
### FUNCTION LOADED ############################################################
##'_____________________________________________________________________________
##'
### INSTALL / LOAD REQUIRED PACKAGES ###########################################
##'_____________________________________________________________________________
##'
requiredPackages <- c("readxl")
loadPackages(requiredPackages)
##'
### REQUIRED PACKAGES INSTALLED / LOADED #######################################
##'_____________________________________________________________________________
##'
### DATA PROCESSING ############################################################
##'                                                                              
##' Start of data processing section, no modifications are needed here           
##'_____________________________________________________________________________
##'
##' searching folder containing files to process
##'_____________________________________________________________________________
##'
##' define base directory, i.e. from where this code is run
if(!exists("base.dir")) base.dir <- getwd()
##'
##' set the target directory
setwd(target.directory)
##'
##' Load EPSG data base, if available
df.EPSG <- list.files(base.dir,
                      pattern = "EPSG_database\\.txt", recursive = TRUE)
if(length(df.EPSG) == 0){
  df.EPSG <- list.files(".", pattern = "EPSG_database\\.txt", recursive = TRUE)
}
EPSG.db <- FALSE
if(length(df.EPSG) > 0){
  df.EPSG <- read.delim(paste0(base.dir,"/",df.EPSG[1]))
  EPSG.db <- TRUE
}else {rm("df.EPSG")}
##'

##' creating a list of directories containing files to process
a <- list.files(".", pattern=file.identifyer, recursive=TRUE, full.names=TRUE)
DIRS <- unique(dirname(a)[!grepl("DoNotUse", dirname(a), ignore.case=TRUE)])
DIRS <- gsub("\\.", "", paste0(target.directory, DIRS))
PointList.DIRS <- NULL
rm(a)
#DIR = DIRS[1]
for(DIR in DIRS){
  Excel.files <- list.files(DIR, pattern = "\\.xls")
  FILES <- Excel.files[grep(file.identifyer, Excel.files)]
  FILES <- unique(sub("\\~\\$", "", FILES))
  if(length(FILES) > 0) {
    tmp.dir <- paste0(DIR, "\\tmp")
    PointList.DIRS <- paste0(PointList.DIRS, "  - ", DIR, "\n")
    setwd(DIR)
    ##' defines & creates a temporary directory
    if(length(intersect(list.dirs(DIR, full.names=FALSE), "tmp")) == 0) {
      dir.create(tmp.dir)
    }
    ##'
    ##' write log
    ##'_________________________________________________________________________
    ##'
    setwd(tmp.dir)
    ##' log
    cat(paste0("\n", "log for \"GNSS_PointList_import.r\" script   -  ",
               Sys.time(),"\n\n", "The following files will be processed:\n\n"),
        append = FALSE, file = "GNSS_PointList_import.log")
    ##' log
    a <- length(FILES)
    for(a1 in 1:a) {  
      cat(paste0("   (", sprintf("%03.0f", a1), ") ", FILES[(a1)], "\t\n"),
          append = TRUE, file = "GNSS_PointList_import.log")
    }
    b <- sub("[0-9][0-9][0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]_", "", FILES)
    if(a!=length(unique(b))) {
      for(c in unique(b)) {
        d<-length(which(b==c))
        if(d>1) { 
          cat(paste0("\n - file \"", c, "\" occures ", d, " times!"),
              append = TRUE, file = "GNSS_PointList_import.log")
        }
      }
      rm(c,d)
    }
    rm(a,a1,b)
    ##' log
    cat("\n", append = TRUE, file = "GNSS_PointList_import.log")
    ##' log
    ##'
    ##' reading data from target files
    ##'_________________________________________________________________________
    ##'
    # FILE = FILES[1]
    for(FILE in FILES){
      setwd(DIR)
      df.f <- read_excel(FILE, sheet = workbook.sheets,
                         col_names = FALSE, .name_repair = "minimal")
      df.f <- as.data.frame(df.f)
      setwd(tmp.dir)
      ##' removes rows and columns with only NA entries
      df.f <- df.f[rowSums(is.na(df.f)) < ncol(df.f), ]
      df.f <- df.f[, colSums(is.na(df.f)) < nrow(df.f)]
      row.names(df.f) <- c(1:nrow(df.f))
      colnames(df.f) <- c(1:ncol(df.f))
      ##' finding the start row of coordinate system entries
      for(i in 1:ncol(df.f)) {
        a <- df.f[, i] == "Coordinate System"
        a[is.na(a)] <- 0
        a <- as.logical(a)
        b <- rownames(df.f)[a]  
        if(length(b) != 0) start.row <- as.numeric(b)
      }; rm("a", "b", "i")
      ##' finding the start column of coordinate system entries   
      for(i in 1:ncol(df.f)) {                                                         
        a <- df.f[i, ] == "Coordinate System"
        a[is.na(a)] <- 0
        a <- as.logical(a)
        b <- colnames(df.f)[a]  
        if(length(b) != 0) start.col <- as.numeric(b)
      }; rm("a", "b", "i")
      ##' finding the end row of coordinate system entries   
      for(i in 1:ncol(df.f)) {
        a <- df.f[, i] == "Calibrated site:"
        a[is.na(a)] <- 0
        a <- as.logical(a)
        b <- rownames(df.f)[a]  
        if(length(b) != 0) end.row <- as.numeric(b)
      }; rm("a", "b", "i")
      end.col <- as.numeric(ncol(df.f))
      ##' data.frame with coordinate system information   
      df.CS <- df.f[start.row:end.row, start.col:end.col]
      df.CS <- df.CS[rowSums(is.na(df.CS)) < ncol(df.CS), ]
      df.CS <- df.CS[, colSums(is.na(df.CS)) < nrow(df.CS)]
      df.CS[, 1] <- sub(":", "", df.CS[, 1])
      df.CS <- as.data.frame(t(df.CS[-1, ]))
      names(df.CS) <- as.character(df.CS[1, ])
      df.CS <- df.CS[-1, ]
      row.names(df.CS)<-c(1:nrow(df.CS))
      names(df.CS)[names(df.CS) == "Name"] <- "TBC_Name"
      if(EPSG.db) {
        i <- intersect(names(df.CS), names(df.EPSG))
        test <- merge(df.CS, df.EPSG, by = i, all.x = TRUE)
        df.CS <- merge(df.CS, df.EPSG, by = i); rm("i")
      }; rm("start.row", "end.row", "start.col", "end.col")
      ##' finding the start of coordinate entries
      for(i in 1:ncol(df.f)) {
        a <- df.f[, i] == "Point List"
        a[is.na(a)] <- 0
        a <- as.logical(a)
        b <- rownames(df.f)[a]  
        if(length(b) !=0) start.row <- as.numeric(b)
      }; rm("a", "b", "i")
      ##' finding the end of coordinate entries 
      for(i in 1:ncol(df.f)) {
        a <- df.f[, i] == "Trimble Business Center"
        a[is.na(a)] <- 0
        a <- as.logical(a)
        b <- rownames(df.f)[a]  
        if(length(b) !=0) end.row <- as.numeric(b)
      }; rm("a", "b", "i")
      ##' keeping only rows with GPS entries and headers  
      df.f <- df.f[(start.row + 1):(end.row - 1), ]
      rm("start.row", "end.row")
      df.f <- df.f[rowSums(is.na(df.f)) < ncol(df.f), ]
      df.f <- df.f[, colSums(is.na(df.f)) < nrow(df.f)]
      ##' reading and subsequently editing column names
      x <- as.character(as.vector(df.f[1, ]))
      x <- gsub("Global", "", x)
      x <- gsub("Meter", "", x)
      x <- gsub("[[:punct:],[:space:]]", "", x)
      x <- gsub("Height", "HeightAE", x)      
      x <- gsub("FeatureCode", "GCP", x)
      ##' column names of the final data frame
      colnames(df.f) <- x
      ##' deleting redundant 1st row
      df.f <- df.f[-1, ]
      row.names(df.f) <- c(1:nrow(df.f))
      rm("x")
      ##' labeling the BASE entries GCP column with the IDs
      df.f[is.na(df.f[, "GCP"]), "GCP"] <- df.f[is.na(df.f[, "GCP"]), "ID"]
      df.f <- df.f[!is.na(df.f[, "GCP"]), ]
      ##'
      if(length(intersect(names(df.f), "Latitude")) == 1) {
        df.f$Latitude  <- ConvertCoordinates(df.f$Latitude)      
      }   
      if(length(intersect(names(df.f), "Longitude")) == 1) {
        df.f$Longitude <- ConvertCoordinates(df.f$Longitude)      
      }   
      if(length(intersect(names(df.f), "HeightAE")) == 1) {
        df.f$HeightAE  <- as.numeric(gsub(",", ".", df.f$HeightAE))     
      }  
      ##'
      setwd(DIR)
      x <- as.character(gsub(" ", "", (gsub(".xlsx", ".txt", FILE))))
      write.table(df.f, file=x, row.names = FALSE, quote = FALSE, sep="\t")
      x <- as.character(gsub(" ", "", (gsub(".xlsx", "", FILE))))
      x <- paste0(x, "_metadata.txt")
      write.table(df.CS, file=x, row.names = FALSE, quote = FALSE, sep="\t")
      ##' log
      setwd(tmp.dir)
      cat(paste0("\n - worksheet \"", workbook.sheets[1],
                 "\" of file \"", FILE, "\" processed!\n",
                 "   ", Sys.time(), "\n",
                 if(!is.null(warnings())){
                   paste0(" -> warnings: ", names(warnings()), "\n")
                 },
                 "\n"
                ),
          append = TRUE, file = "GNSS_PointList_import.log")
    }
  }
}
setwd(base.dir)
##'
### END OF DATA PROCESSING #####################################################
##'_____________________________________________________________________________
##'
### UNLOAD REQUIRED PACKAGES ###################################################
##'_____________________________________________________________________________
##'
listOfAllAddOnPackages <- names(sessionInfo()$otherPkgs)
if(length(listOfAllAddOnPackages) > 0) {
  print(paste0("List of add on packages: ", listOfAllAddOnPackages))
  sapply(paste0("package:", requiredPackages),
         detach, unload = TRUE, character.only = TRUE)
  listOfAllAddOnPackages <- names(sessionInfo()$otherPkgs)
  print(paste0("List of add on packages: ", listOfAllAddOnPackages))
}; rm("listOfAllAddOnPackages")
##'
##' clean work space
rm(list= setdiff(ls(),
                 c("df", "df.imp", "df.final", "fileIDs", "fileID", "OutName",
                   "base.dir", "export.dir", "tmp", "target.directory",
                   "PointList.DIRS", "pointlist.file",
                   "import.file", "processing.file",
                   "StartTime", "CodeExecutionDuration")))
##'_____________________________________________________________________________
##'
### REQUIRED PACKAGES UNLOADED #################################################
##'_____________________________________________________________________________
##'