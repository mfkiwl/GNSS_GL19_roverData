### HEADER #####################################################################
##' @title        List of all labels (5-GNSS_Assemble_FeatureCodes-PointIDs.R)
##' @author       Urs A. Treier
##' @contact      urs.treier at gmail.com
##' @date         25/11/2021
##' @description  This script compiles all labels of GNSS measurements contained
##'               in a set of cvs files.
##' @log          Processing logs will be saved in the exports tmp directory.
##' @dependencies leaflet
##' @licencing    GPL (see GNU General Public License at
##'               <http://www.gnu.org/licenses/>)
##'               Copyright (C) 2017  Urs A. Treier
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
##'
### DEFINITION OF VARIABLES ####################################################
##'_____________________________________________________________________________
##'
##' !! MODIFY WHERE NECESSARY !!
##'_____________________________________________________________________________
##'
##' define absolute path to working directory
if(!exists("base.dir")) {
  rm(list=ls())
  base.dir <- getwd()
  export.dir <- file.path(base.dir, "DataExport")
}
##' Output name appendix/ID
if(!exists("OutName")) OutName <- "GL19"
##'
##' creates the working/export directories
if(length(intersect(list.dirs(base.dir, full.names = F), "DataExport")) == 0) {
  dir.create(export.dir)}
tmp <- file.path(export.dir, "tmp")
if(length(intersect(list.dirs(export.dir, full.names = F),"tmp")) == 0) {
  dir.create(tmp)}
##'
### END OF DEFINITION OF VARIABLES #############################################
##'_____________________________________________________________________________
##'
### LOAD FUNCTIONS #############################################################
##'_____________________________________________________________________________
##'
##' function that transforms Coordinates
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
### DATA PROCESSING ############################################################
##'                                                                              
##' Start of data processing section, no modifications are needed here
##'_____________________________________________________________________________
##'
##' Directory with cvs files
search.dir <- paste0("O:/Nat_UAS4Ecology/D4E_GNSSdata/ProcessedData",
                     "/GNSS_GL_2019/GNSS_GL19_TBC/RoverProcessing",
                     "/HandheldExport")
##' 
##' Reading files and combine content in a data frame
FILES <- list.files(search.dir, pattern = "csv",
                    recursive = T, full.names = T)
df.codes <- Reduce(rbind, lapply(FILES, read.csv, header = FALSE, quote=""))
##'
##' add column names
names(df.codes) <- c("ID", "Latitude", "Longitude","HeightAE", "GCP")
df.codes <- df.codes[, c("ID", "GCP", "Latitude", "Longitude", "HeightAE")]
##'
##' Convert DMS coordinates to Degree format 
df.codes$Latitude  <- ConvertCoordinates(df.codes$Latitude)
df.codes$Longitude  <- ConvertCoordinates(df.codes$Longitude)    
##'
##' remove duplicated entries 
df.codes <- unique(df.codes)
##'
##' add variable with character length of the GCP name
df.codes$char <- nchar(df.codes$GCP)
##' 
##' add variable with  strings trimmed 16 characters
df.codes$GCPtrim <- strtrim(df.codes$GCP, 16)
##'
##' some stats on the uniqueness of Feature Codes
a <- nrow(unique(df.codes[df.codes$char > 16, ]))
b <- nrow(unique(df.codes[df.codes$char > 16, c("ID", "GCP")]))
c <- nrow(unique(df.codes[df.codes$char > 16, c("ID", "GCPtrim")]))
##'
##' start log entry
LogFile <- paste0("GNSS_Data_", OutName, "_FeatureCodes-PointIDs.log")
setwd(tmp) 
cat(paste0(Sys.time(), "\nAssemble Feature Codes and Point IDs\n",
           "-> GNSS_Data: ", OutName, "\n\n",
           "Directory with files of which information has been assembled:\n",
           search.dir, "\nFile List:\n",
           paste0(basename(FILES), collapse = "; "), "\n\n",
           "-> ", a, " unique entries with lables longer than 16 characters;\n",
           "-> of these, ", b, " unique entries based on ID and GCP label;\n",
           "-> of these, ", c, " unique entries based on ID and trimmed GCP ",
           "label, i.e. trimmed to 16 characters.\n\n"
          ),
    append = FALSE, file = LogFile)
rm("a", "b", "c")
##'
if(!is.null(warnings())){
  cat(paste0("\n\n-> warnings: ", names(warnings()), "\n\n"),
      append = TRUE, file = LogFile)
}
setwd(base.dir)
##' end log entry  
##'
##' save data in R data format
OUTID <- paste0("GNSS_Data_", OutName, "_FeatureCodes-PointIDs.rda")
setwd(export.dir)
save(df.codes, file = OUTID)
setwd(base.dir)
##'
##' start log entry
setwd(tmp) 
cat(paste0("Data in R data format saved in export directory.\nFile name: ",
           OUTID, "\n", Sys.time()
           ),
append = TRUE, file = LogFile)
setwd(base.dir)
##' end log entry 
##'