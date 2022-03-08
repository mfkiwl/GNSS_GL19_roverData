### HEADER #####################################################################
##' @title        Merge post-processing info (1-GNSS_Data_import-processing.R)
##' @author       Urs A. Treier
##' @contact      urs.treier at gmail.com
##' @date         02/11/2021
##' @description  This script imports and merges information from GNSS rover
##'               data post-processing done with Trimble Business Center (TBC). 
##'               The "RoverImport" .txt and "BaselineProcessing" .txt files 
##'               provided information (copy/paste) seen during the TBC steps.
##'               Output is a R data frame combining Point "ID", Feature "Code",
##'               rover "File" Name, "Time" of measurement, baseline processing
##'               precision (i.e., 95% horizontal precision "hPrec" & vertical
##'               precision "VPrec", and "RMS"), and the distance to the base
##'               "DistanceTo[base]".
##'               If selected, the data frame can be exported as a txt file.
##'               The name of the output file is the intersection of the all
##'               parts of the input file names delimited by underscores, i.e.
##'               "ProcessingInfo_[optional add]_[from input file names].txt
##' @note         DEFINITION OF VARIABLES section: Define appropriately.
##' @log                                 
##' @dependencies
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
  target.directory <- paste0("C:/Users/au261432/OneDrive/DataUrs",
                             "/_ac_Projects/Current/DroneEcol",
                             "/R-WorkingDirectory/GNSS_Processing",
                             "/GNSS_GL_2019/GNSS_GL19_roverData",
                             "/Data_test/perDate")
}
setwd(target.directory)
if(!exists("imp.file") | !exists("proc.file")){
  ##'
  ##'  define names of the files to process
  imp.file <- "20190717/20190717_03941980_RoverImport.txt"
  proc.file <- "20190717/20190717_03941980_BaselineProcessing.txt"
  ##'
  ##'  optional: Output name ID
  OUTID <- "GL"
}
##'
##' Export the data frame (TURE/FALSE)? 
EXPORT <- FALSE  
##'  
##'
### END OF DEFINITION OF VARIABLES #############################################
##'
### DATA PROCESSING ############################################################
##'
##' Start of data processing section, no modifications are needed here
##'_____________________________________________________________________________
##'
##' importing "RoverImport" *.txt file  
imp.file.colnames <- c("Import", "PointID", "FileName",
                       "StartTime", "EndTime", "Duration",
                       "FeatureCode", "Send.to.RTX.PP")
df.imp <- read.delim(imp.file, header = FALSE, sep = "\t")
names(df.imp) <- imp.file.colnames
##' skip first line as well as first and last column
df.imp <- df.imp[-1, 2:7]
##' skip intermediate trajectories, skip "StartTime" and "Duration" column
df.imp <- df.imp[(!df.imp$PointID == "Continuous Segment" &
                  !df.imp$PointID == "Roving Segment"), -3]
##' rename columns
names(df.imp) <- c("ID", "File", "Time", "Duration", "Code")
##'
##' importing "BaselineProcessing" *.txt file
proc.file.colnames <- c("Save",	"Observation", "SolutionType",
                        "hPrec",	"vPrec", "RMS", "DistToBase")
df.proc <- read.delim(proc.file, header = FALSE, sep = "\t")
names(df.proc) <- proc.file.colnames
##' skip first line and first column
df.proc <- df.proc[-1, -1]
##' selecting point measurements only, either with fixed or floating solution
df.proc <- df.proc[(df.proc$SolutionType == "Fixed" |
                    df.proc$SolutionType == "Float"), ]
##' split Observation column into two new variables, i.e. Base and ID
df.proc$Base <- sapply(strsplit(df.proc$Observation, " --- "), "[", 1)
df.proc$ID <- sapply(strsplit(df.proc$Observation, " --- "), "[", 2)
##' order columns and skip "Observation" column
df.proc <- df.proc[, c("ID", "Base", "DistToBase", "hPrec", "vPrec", "RMS")]
##'
##' extracting point ID's and Base ID's
PointIDs <- intersect(unique(df.proc$ID), unique(df.imp$ID))
BaseIDs <- sort(setdiff(unique(df.proc$ID), unique(df.imp$ID)))
BaseIDs <- sort(unique(c(BaseIDs, unique(df.proc$Base))))
##'
##' adding base statistics and base IDs to import data frame  
for(Base in BaseIDs){
  df.imp[, paste0("DistTo_", Base)] <- NA
  df.imp[, paste0("hPrec_", Base)] <- NA
  df.imp[, paste0("vPrec_", Base)] <- NA
  df.imp[, paste0("RMS_", Base)] <- NA
  x <- df.imp[1, ]
  x[!is.na(x)] <- NA
  x[, "ID"] <- Base
  df.imp <- rbind(df.imp, x)
  rm(x)
}
##'
##' assigning relevant data to the point measurements
for(Base in intersect(BaseIDs, unique(df.proc$Base))) {
  df.proc.sub <- df.proc[!df.proc$ID %in% BaseIDs & df.proc$Base == Base, ]
  df.proc.sub <- df.proc.sub[, -2]
  colnames(df.proc.sub)[2] <- "DistTo"
  new.names <- c("ID", paste0(names(df.proc.sub[, 2:5]), "_", Base))
  names(df.proc.sub) <- new.names
  for(ID in unique(df.proc.sub$ID)){
    x <- df.proc.sub[df.proc.sub$ID == ID, ]
    df.imp[df.imp$ID == ID, names(df.proc.sub)] <- x
    rm(x)
  }
  ##' adding the data for the bases
  df.proc.sub <- df.proc[df.proc$ID %in% BaseIDs & df.proc$Base == Base, ]
  for (ID in unique(df.proc.sub$ID)) {
    df.proc.s <- df.proc.sub[df.proc.sub$ID == ID, ]
    for (c in c("DistToBase", "hPrec", "vPrec", "RMS")) {
      df.proc.s[1, c] <- mean(as.numeric(df.proc.s[, c]))
    }
    df.proc.s <- df.proc.s[1, ]
    df.proc.s <- df.proc.s[, -2]
    colnames(df.proc.s)[2] <- "DistTo"
    new.names <- c("ID", paste0(names(df.proc.s[,2:5]), "_", Base))
    names(df.proc.s) <- new.names
    df.imp[df.imp$ID %in% df.proc.s$ID, names(df.proc.s)] <- df.proc.s
  }
}
##'
##' re-name Code variable to GCP
names(df.imp)[names(df.imp) == "Code"] <- "GCP"
##'
##' labeling the BASE entries GCP column with the IDs
df.imp[is.na(df.imp[, "GCP"]), "GCP"] <- df.imp[is.na(df.imp[, "GCP"]), "ID"]
df.imp <- df.imp[!is.na(df.imp[, "GCP"]), ]
##'
##' export data frame
if (EXPORT){
  ##' output file name
  x <- sub("\\.[[:alnum:]]+$", "", basename(c(imp.file, proc.file)))
  x <- sub("BaselineProcessing", "", sub("RoverImport", "", x))
  x <- unique(intersect(unlist(strsplit(x[1], "_")),
                        unlist(strsplit(x[2], "_")))
              )
  OUTID <- paste("ProcessingInfo", OUTID, paste(x, collapse = "_"), sep = "_")
  OUTID <- paste0(OUTID, ".txt")
  rm(x)
  ##'
  ##'  write output file
  write.table(df.imp, file=OUTID, row.names=FALSE, quote=FALSE, sep ="\t")  
}
rm(list= setdiff(ls(),
                 c("df", "df.imp", "df.final",
                   "target.directory", "OutName",
                   "fileIDs", "fileID.n", "fileID",
                   "imp.file", "import.files", "proc.file", "processing.files",
                   "base.dir", "WorkingDirectory", "tmp",
                   "StartTime", "CodeExecutionDuration")))
##' 
### END OF DATA PROCESSING #####################################################
##'_____________________________________________________________________________
