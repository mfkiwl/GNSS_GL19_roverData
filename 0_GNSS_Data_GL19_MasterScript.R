### HEADER #####################################################################
##' @title        Import rover data processing info (Import_GNSS_Data.R)
##' @author       Urs A. Treier
##' @contact      urs.treier at gmail.com
##' @date         03/11/2021
##' @description  This script compiles GNSS rover data and base-line processing
##'               information from data processed with Trimble Business Center
##'               (TBC). It will search through all sub-directories of the
##'               defined target directory. Files with the user defined file
##'               identifier in their names will be processed and compiled as
##'               follows:
##'               1. Trimble Business Center (TBC) "PointList" excel files
##'                  will read and converted in text files. CRS info will be
##'                  added from the "EPSG_database.txt" file
##'                  -> script: 1-GNSS_PointList_import.R
##'               2. Rover data import and baseline processing information
##'                  will be combined in a data frame. Input files identifier
##'                  need to be provided in this master script, e.g.
##'                  "RoverImport" and  "BaselineProcessing".
##'                  -> script: 2-GNSS_Data_import-processing.R
##'               3. The "PointList" text file information will be  combined to 
##'                  a data frame.
##'                  -> script: 2-GNSS_Compiling_PointList_data.R
##'               4. Data frames from step 2 and 3 are combined to final
##'                  data frame
##'               5. Final data frame is saved as tab delimited text file and as
##'                  in R data format as a rda file.
##' @note         DEFINITION OF VARIABLES section: Define appropriately.
##'               New folders will be created (...Data_Export and tmp folder).
##'               These folder are the working directories and are sub-folders
##'               of the directory from which this script is run.
##'               The temp folder can be deleted once the the computations are
##'               finished.
##' @log          Processing logs will be saved in the exports tmp directory.                       
##' @dependencies lutz (Look Up Time Zones of Point Coordinates)
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
rm(list=ls()) # clean workspace
StartTime <- Sys.time()  # stores start time when executing the code
##'
##' define the absolute path to the directory with the rover data to process
target.directory <- paste0("O:/Nat_UAS4Ecology/D4E_GNSSdata/ProcessedData",
                           "/GNSS_GL_2019/GNSS_GL19_TBC/RoverProcessing",
                           "/perDate")
##'
##' define the output file name
OutName <- "GL19"

##'
##' file identifier
import.file <- "RoverImport"
processing.file <- "BaselineProcessing"
pointlist.file <- "PointList"
##'
##' GNSS "PointList" file extension for geoid files (e.g.: UTM, LV95, ...)
Fext <- "UTM"
##'
##' define absolute path to working directory
if(!exists("base.dir")) base.dir <- getwd()
export.dir <- file.path(base.dir, "DataExport")
##'
##' creates the working/export directories
if(length(intersect(list.dirs(base.dir, full.names = F), "DataExport")) == 0){
  dir.create(export.dir)}
tmp <- file.path(export.dir, "tmp")
if(length(intersect(list.dirs(export.dir, full.names = F),"tmp")) == 0){
  dir.create(tmp)}
##'
### END OF DEFINITION OF VARIABLES #############################################
##'_____________________________________________________________________________
##'
### LOAD FUNCTIONS #############################################################
##'_____________________________________________________________________________
##'
##' function that installs and loads required packages
loadPackages <- function(Pkgs) {
  new.Pkgs <- Pkgs[!(Pkgs %in% installed.packages()[, "Package"])]
  if (length(new.Pkgs) > 0) {
    install.packages(new.Pkgs, dependencies = TRUE)
  }  
  sapply(Pkgs, require, character.only = TRUE)
} 
##'
##' function that returns the time to run a code
CodeExecutionDuration <- function(StartTime){
  deltaT <- as.numeric(difftime(Sys.time(), StartTime, units = "secs"))
  S <- round(deltaT - 60*floor(deltaT/60), digits = 0)
  M <- floor(deltaT/60)-60*floor(deltaT/(60*60))
  H <- floor(deltaT/(60*60))-24*floor(deltaT/(60*60*24))
  D <- floor(deltaT/(60*60*24))-7*floor(deltaT/(60*60*24*7))
  W <- floor(deltaT/(60*60*24*7))
  paste0(if(W != 0) paste0(W, "w "),
         if(H != 0) paste0(H, "h "),
         if(M != 0) paste0(M, "m "),
         if(S != 0) paste0(S, "s")
  )
}
##'
### FUNCTIONS LOADED ###########################################################
##'_____________________________________________________________________________
##'
### INSTALL / LOAD REQUIRED PACKAGES ###########################################
##'_____________________________________________________________________________
##'
requiredPackages <- c("lutz")
loadPackages(requiredPackages)
##'
### REQUIRED PACKAGES INSTALLED / LOADED #######################################
##'_____________________________________________________________________________
##'_____________________________________________________________________________
##'
### DATA PROCESSING ############################################################
##'
##' Start of data processing section, no modifications are needed here
##'_____________________________________________________________________________
##'
##' creating log file, 1st entry
setwd(tmp) 
cat(paste0(Sys.time(), "\n\n",
           "Processing Log GNSS_Data_", OutName, "\n\n",
           "Target Directory (", format(Sys.time(), "%X"), "):\n",
           target.directory,"\n\n"),
    append = FALSE, file = paste0("GNSS_Data_", OutName, ".log"))
setwd(base.dir)
##'
##' process TBC "PointList" Excel files into text files
source(paste0(base.dir,"/1-GNSS_PointList_import.R"))
##'
##'  writing log entry
setwd(tmp) 
cat(paste0("PointList files processed ",
           "(", format(Sys.time(), "%X"), "):\n",
           " for success/errors check logs in the directory of each file:\n",
           PointList.DIRS, "\n\n"),
    append=TRUE, file = paste0("GNSS_Data_", OutName, ".log"))
setwd(base.dir)
##'
##' creating file lists
setwd(target.directory) 
txt.files <- list.files(target.directory, pattern = "\\.txt", recursive = TRUE)
import.files <- txt.files[grep(import.file, txt.files)]
processing.files <- txt.files[grep(processing.file, txt.files)]
pointlist.files <- txt.files[grep(pointlist.file, txt.files)]
setwd(base.dir) 
##'
rm(txt.files)
##'
##' extracting unique file IDs
x.i <- unlist(strsplit(basename(sub("\\.txt","", import.files)), "_"))
x.p <- unlist(strsplit(basename(sub("\\.txt","", processing.files)), "_"))
x.l <- unlist(strsplit(basename(sub("\\.txt","", pointlist.files)), "_"))
fileIDs <- unique(intersect(x.l,intersect(x.i,x.p)))
##'
rm(x.i, x.l, x.p)
##'
##' writing log entry
setwd(tmp) 
cat(paste0("File Identifyer (", format(Sys.time(), "%X"), "):\n",
           paste0(fileIDs, collapse=", "),"\n\n"),
    append = TRUE, file = paste0("GNSS_Data_", OutName, ".log"))
setwd(base.dir)
##'
##' creating data frame to compile the data 
df.final.names <- c("ID", "GCP", "Time", "Duration",
                    "Latitude", "Longitude", "HeightAE",
                    "Easting", "Northing", "Elevation",
                    "Datum", "Zone", "Geoid", "EPSG", "Name",
                    "UTM.Time", "localTZ",
                    "File", "PointListFile")
df.final <- setNames(data.frame(matrix(ncol = 19, nrow = 0)), df.final.names)
##'
##' import and merge info for file triplets selected with file ID
for (fileID.n in 1:length(fileIDs)){
  #fileID.n = 1
  fileID = fileIDs[fileID.n]
  ##'
  ##' defining path to "RoverImport" and "BaselineProcessing" file
  imp.file <- import.files[grep(fileID, import.files)]
  proc.file <- processing.files[grep(fileID, processing.files)]
  ##'
  ##' import and merge "RoverImport" and "BaselineProcessing" *.txt files
  source(paste0(base.dir,"/2-GNSS_Data_import-processing.R"))
  ##'
  ##' writing log entry
  setwd(tmp)
  cat(paste0("Import-Processing Info extracted from files",
             " (", format(Sys.time(), "%X"), "):\n",
             "  - ", imp.file, "\n", "  - ", proc.file, "\n",
             "  - ", nrow(df.imp), " observations\n", 
             if(!is.null(warnings())){
               paste0(" -> warnings: ", names(warnings()), "\n")
             }
            ),
      append = TRUE, file = paste0("GNSS_Data_", OutName, ".log"))
  setwd(base.dir)
  ##' 
  ##' import and merge "PointList" information
  source(paste0(base.dir,"/3-GNSS_Compiling_PointList_data.R"))
  df.pl <- df
  ##'
  ##' writing log entry
  setwd(tmp)
  cat(paste0("Point list data extracted (", nrow(df.pl), " observations) ",
             "and compiled for files with ID <", fileID,">, ",  
             "(", format(Sys.time(), "%X"), ").\n",
             if(!is.null(warnings())){
               paste0(" -> warnings: ", warnings(), "\n")
             }
            ),
      append = TRUE, file = paste0("GNSS_Data_", OutName, ".log"))
  setwd(base.dir)
  ##'
  ##' check for ambiguities in Import-Processing Info
  Ambig <- df.imp[duplicated(df.imp[, c("ID", "GCP")]), c("ID", "GCP")]
  ##'
  Ambig.keep <- NULL
  Ambig.all <- NULL
  for(r in 1 : nrow(Ambig)){
    a.all <- df.imp[df.imp$ID == Ambig$ID[r] & df.imp$GCP == Ambig$GCP[r], ]
    a.keep <- a.all[as.difftime(a.all$Duration, units = "secs") > 4 &
                    as.difftime(a.all$Duration, units = "secs") <= 6, ]
    if(nrow(a.keep) > 1) a.keep <- tail(a.keep, 1)
    if(nrow(a.keep) == 0){
      a.keep <- a.all[as.difftime(a.all$Duration, units = "secs") > 6 &
                      as.difftime(a.all$Duration, units = "secs") <= 12, ]
      if(nrow(a.keep) > 1) a.keep <- tail(a.keep, 1)
    }
    if(nrow(a.keep) == 0){
      a.keep <- a.all[as.difftime(a.all$Duration, units = "secs") > 12, ]
      if(nrow(a.keep) > 1) a.keep <- tail(a.keep, 1)
    } 
    ##' all ambiguous entries
    Ambig.all <- rbind(Ambig.all, a.all) 
    ##' entries to keep
    Ambig.keep <- rbind(Ambig.keep, a.keep)
  }  
  ##' ambiguous entries to skip
  Ambig.skip <- Ambig.all[!(row.names(Ambig.all) %in% row.names(Ambig.keep)), ]
  Ambig.skip$Ambig <- paste(Ambig.skip$ID,
                            Ambig.skip$GCP,
                            as.difftime(Ambig.skip$Duration, units = "secs"),
                            sep=", ")
  ##'
  ##' remove ambiguous entries from Import-Processing Info data frame
  df.imp.orig <- df.imp 
  df.imp <- df.imp[!(row.names(df.imp) %in% row.names(Ambig.skip)), ]
  ##'
  ##' merge data frames    
  df <- merge(df.pl, df.imp, by = intersect(names(df.pl), names(df.imp)))
  ##'
  ##' writing log entry
  setwd(tmp)
  if(nrow(df.imp.orig) != nrow(df.pl)){
    cat(paste0("\n-> Missmatch between observations of Import-Processing Info ",
               "and extracted Point List data for file ID <", fileID,">!\n"
    ),
    append = TRUE, file = paste0("GNSS_Data_", OutName, ".log")) 
  }
  if(nrow(df.pl)-nrow(df.imp.orig) < 0){
    cat(paste0("\n-> There are ambiguities in the Import-Processing Info!",
               "\n   If available, the last entry with a measurement duration ",
               "of 5-6 seconds (or 6-12s / >12s) is keept.",
               "\n   ", nrow(Ambig.skip), " entry/entries have been skipped.",
               "\n-> Check the follwing entry/entries (ID, label, duration): ",
               paste(Ambig.skip$Ambig, collapse = "; "), "\n"
    ),
    append = TRUE, file = paste0("GNSS_Data_", OutName, ".log")) 
  }
  if(nrow(df.pl) - nrow(df) > 0){
    a <- unique(setdiff(df.pl$GCP, df$GCP), setdiff(df.pl$GCP, df$GCP))
    a <- paste(a, collapse = "; ")
    cat(paste0("\n-> Import-Processing Info not successfully merged with ",
               "extracted Point List data for file ID <", fileID,">.",
               "\n   ", nrow(df.pl)-nrow(df), " observation(s) has/have been ",
               "skipped with the label(s): ", a, "\n",
               "\n-> The merged data has ", nrow(df), " observations.\n"
    ),
    append = TRUE, file = paste0("GNSS_Data_", OutName, ".log"))
    rm(a)
  }
  if(nrow(df) == nrow(df.pl) & nrow(df.imp) == nrow(df.pl)){
    cat(paste0("\n-> Import-Processing Info successfully merged with ",
               "extracted Point List data for file ID <", fileID,">.",
               "\n-> The merged data has ", nrow(df), " observations.\n"
    ),
    append = TRUE, file = paste0("GNSS_Data_", OutName, ".log"))
  }
  setwd(base.dir)
  ##'
  ##' finding local time zone for each entry and changing UTC time to local
  df$Time <- as.POSIXct(df$Time, tz = "UTC", format = "%d/%m/%Y %H:%M:%OS")
  df$localTZ <- NA
  df$lTime <- NA
  for(i in 1:nrow(df)) {
    df[i, "localTZ"] <- tz_lookup_coords(as.numeric(df[i, "Latitude"]),
                                         as.numeric(df[i, "Longitude"]),
                                         method = "accurate")
    df[i, "lTime"] <- format(df[i, "Time"], tz = df[i, "localTZ"], usetz = TRUE)
  }    
  names(df)[names(df) == "Time"] <- "UTM.Time"
  names(df)[names(df) == "lTime"] <- "Time"
  ##'
  df.final.names <- union(names(df.final), names(df))
  df <- df[, intersect(df.final.names, names(df))]
  if(fileID.n != 1 && length(names(df.final)) > length(names(df))){
    missing.Cols <- setdiff(names(df.final), names(df))
    df[, missing.Cols] <- NA
  }
  if(fileID.n != 1 && length(names(df)) > length(names(df.final))){
    missing.Cols <- setdiff(names(df), names(df.final))
    df.final[, missing.Cols] <- NA
  }
  df.final <- rbind(df.final, df)
  ##'
  ##' writing log entry
  setwd(tmp)
  cat(paste0("\nData added to the final data frame and time of point records ",
             "converted to local time (", format(Sys.time(), "%X"), ").\n",
             if(!is.null(warnings())){
               paste0(" -> warnings: ", names(warnings()), "\n")
             },
             "\n"
            ),
      append=TRUE, file = paste0("GNSS_Data_", OutName, ".log"))
  setwd(base.dir)
}  
##'
##' register "NA" entries as real NA values for the entire data frame
df.final[] <- lapply(df.final, function(x) {
  is.na(levels(x)) <- levels(x) == "NA"
  x
})
##'
##'  remove rows and columns with only NA entries
df.final <- df.final[rowSums(is.na(df.final)) < ncol(df.final), ]
df.final <- df.final[, colSums(is.na(df.final)) < nrow(df.final)]
##'
##' Convert duration of measurements to seconds as a numeric format
df.final$Duration <- as.numeric(as.difftime(df.final$Duration, units = "secs"))
##'
##' writing log entry
if(fileID.n != length(fileIDs)){
  setwd(tmp)
  cat(paste0("\nData not added to the final data frame, data merging error!\n",
             Sys.time(), "\n\n",
             if(!is.null(warnings())){
               paste0(" -> warnings: ", names(warnings()), "\n")
             }
            ),
      append=TRUE, file = paste0("GNSS_Data_", OutName, ".log"))
  setwd(base.dir)
} else {
  ##' find and average repeated measurements
  source(paste0(base.dir,"/4-GNSS_Merge_Repeated_Measurements.R"))
  ##'
  ##' writing log entry
  setwd(tmp)
  cat(paste0("Replicated measurements found and replaced by their mean.\n",
             " -> see <Merge_Repeated_Measurements.log> for details",
             " (", format(Sys.time(), "%X"), ").\n\n",
             if(!is.null(warnings())){
               paste0(" -> warnings: ", names(warnings()), "\n")
             }
            ),
      append = TRUE, file = paste0("GNSS_Data_", OutName, ".log"))
  setwd(base.dir)
  setwd(tmp)
  cat(paste0("Compiling of all data finshed!\n",
             Sys.time(), "\n\n",
             if(!is.null(warnings())){
               paste0(" -> warnings: ", names(warnings()), "\n")
             }
            ),
    append=TRUE, file = paste0("GNSS_Data_", OutName, ".log"))
  setwd(base.dir)
  ##'
}
##'
##' clean work space
rm(list= setdiff(ls(),
                 c("df.final", "df.o", "df.q", "df.d", "df.l",
                   "fileIDs", "fileID.n", "OutName",
                   "target.directory", "base.dir", "export.dir", "tmp",
                   "StartTime", "CodeExecutionDuration")))
##'
### END OF DATA PROCESSING #####################################################
##'_____________________________________________________________________________
##'
### EXPORT OF DATA #############################################################
##'_____________________________________________________________________________
##'
##' Check that all file ID have been processed
if(fileID.n == length(fileIDs)){
  rm("fileID.n")
  ##'
  ##' text file export function, including file naming and directory
  Write_To_txt <- function(Data, Name, Folder){
    CurrentWD <- getwd()
    setwd(Folder)
    x <- paste0(Name, "_", format(Sys.time(), "%Y%m%d%H%M"), ".txt")
    write.table(Data, file = x, row.names = FALSE, quote = FALSE, sep = "\t")
    setwd(CurrentWD)
  }
  ##'
  ##' data export function (rda), including file naming and directory 
  Save_To_rda <- function(Objects, Name, Folder){
    CurrentWD <- getwd()
    setwd(Folder)
    x <- paste0(Name, "_", format(Sys.time(), "%Y%m%d%H%M"), ".rda")
    save(list = Objects, file = x)
    setwd(CurrentWD)
  }
  ##'
  ##' write data to tab delimited text file
  if(exists("df.final")){
    Write_To_txt(df.final, paste0("GNSS_Data_", OutName), export.dir)
    ##' save data in R data format
    Save_To_rda(df.final, paste0("GNSS_Data_", OutName), export.dir)
    ##'
    rm("Write_To_txt", "Save_To_rda")
  }
  if(exists("df.o")){
    Write_To_txt(df.o, paste0("GNSS_Data_", OutName), export.dir)
    Write_To_txt(df.q, paste0("GNSS_Data_", OutName, "_merged"), export.dir)
    if(exists("df.q") && exists("df.d") && exists("df.l")){
      Save_To_rda(c("df.o", "df.q", "df.l", "df.d"),
                  paste0("GNSS_Data_", OutName),
                  export.dir)
    }
    if(exists("df.q") && !exists("df.d") && exists("df.l")){
      Save_To_rda(c("df.o", "df.q", "df.l"),
                  paste0("GNSS_Data_", OutName),
                  export.dir)
    }
    if(exists("df.q") && exists("df.d") && !exists("df.l")){
      Save_To_rda(c("df.o", "df.q", "df.d"),
                  paste0("GNSS_Data_", OutName),
                  export.dir)
    }
    if(exists("df.q") && !exists("df.d") && !exists("df.l")){
      Save_To_rda(c("df.o", "df.q"),
                  paste0("GNSS_Data_", OutName),
                  export.dir)
    }
  }; rm("Write_To_txt", "Save_To_rda")
  ##'
  ##' writing log entry
  setwd(tmp)
  cat(paste0("Data files saved to:\n",
             export.dir,  "\n", Sys.time(), "\n",
             if(!is.null(warnings())){
               paste0(" -> warnings: ", names(warnings()), "\n")
             }, "\n",
             "Code Execution Time: ", CodeExecutionDuration(StartTime),"\n\n"
            ),
      append=TRUE, file = paste0("GNSS_Data_", OutName, ".log"))
  setwd(base.dir)
  ##'
}
rm(list= c("StartTime", "CodeExecutionDuration"))
setwd(base.dir)
##'
##'_____________________________________________________________________________
##'
### UNLOAD REQUIRED PACKAGES ###################################################
##'_____________________________________________________________________________
##'
listOfAllAddOnPackages <- names(sessionInfo()$otherPkgs)
if(length(listOfAllAddOnPackages) > 0) {
  print(paste0("List of add on packages: ", listOfAllAddOnPackages))
  sapply(paste0("package:", listOfAllAddOnPackages),
       detach, unload = TRUE, character.only = TRUE)
  listOfAllAddOnPackages <- names(sessionInfo()$otherPkgs)
  print(paste0("List of add on packages: ", listOfAllAddOnPackages))
}; rm("listOfAllAddOnPackages")
##'
##'_____________________________________________________________________________
##'
### REQUIRED PACKAGES UNLOADED #################################################
##'_____________________________________________________________________________
##'

