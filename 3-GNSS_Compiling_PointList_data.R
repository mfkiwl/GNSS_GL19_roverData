### HEADER #####################################################################
##' @title        Compiling GNSS Data (Compiling_GNSS_Data.R)
##' @author       Urs A. Treier
##' @contact      urs.treier at gmail.com
##' @date         06/10/2021
##' @description  A script to compile GNSS rover data that have been
##'               post-processed with Trimble Business Center (TBC).
##'               The defined "directories to process" are screened for folder
##'               pairs named "WGS84" and "UTM". These folders should contain
##'               extracted TBC processed point coordinates (see TBC_[Date].R).
##'               The coordinates should be either processed in the WGS84
##'               coordinate system and/or a UTM coordinate system. All files
##'               need to be named with the same point list file identifier.
##'               If processed in a UTM coordinate system, the file extension
##'               of the geoid GNSS data files needs to be specified.
##'               Carefully check the PointList_Processing.log!
##' @note         DEFINITION OF VARIABLES section: Define appropriately.
##'               The temp folder created in the Working Directory during
##'               computations can be deleted after finalizing data compilation. 
##' @log          PointList_Processing.log in the Target Directory
##' @dependencies sf
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
if(!exists("target.directory")) {
  rm(list = ls())
  ##'
  ##' define the target directory
  target.directory <- paste0("C:/Users/au261432/OneDrive/DataUrs",
                             "/_ac_Projects/Current/DroneEcol",
                             "/R-WorkingDirectory/GNSS_Processing",
                             "/GNSS_GL_2019/GNSS_GL19_roverData",
                             "/Data_test/perDate")
}
##'
##'  Export the data frame (TURE/FALSE)? 
EXPORT <- FALSE  
##'  
##' Output name appendix/ID
OUTID <- "GL19"

##'
if(!exists("pointlist.file")) {
  ##'
  ##'  define the point list file identifier
  pointlist.file <- "PointList"
  ##'
  ##' file extension of geoid GNSS data files (e.g.: UTM, LV95, ...)
  Fext <- "UTM"
}
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
##' function that finds unique identifier in file names
FileIdFinder <- function(FileList, FileType, NameSeparator) {
  FileType <- paste0("\\.", FileType)
  x <- strsplit(basename(sub(FileType,"", FileList)), NameSeparator)
  x <- unique(setdiff(unlist(x), Reduce(intersect, x)))
  return(x)}
##'
##' function that transforms WGS84 lat/long to UTM Easting/Northing coordinates
LatLong_to_UTM <- function(id, lat, long){
  # dependencies: sf
  df.org <- data.frame(ID = id, X = long, Y = lat)
  df.new <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Zone", "EPSG"))
  for(i in 1:length(lat)){
    X.i <- lat[i]
    Y.i <- long[i]
    # Special zones for Svalbard and southern Norway
    if(X.i >= 72 && X.i < 84 && Y.i >= 0 && Y.i < 9){zone <- 31
    } else if(X.i >= 72 && X.i < 84 && Y.i >=  9 && Y.i < 21) {zone <- 33 
    } else if(X.i >= 72 && X.i < 84 && Y.i >= 21 && Y.i < 33) {zone <- 35
    } else if(X.i >= 72 && X.i < 84 && Y.i >= 33 && Y.i < 42) {zone <- 37
    } else if(X.i >= 56 && X.i < 64 && Y.i >=  3 && Y.i < 12) {zone <- 32
    } else {zone <- (floor((Y.i + 180) / 6) %% 60) + 1}
    # define the hemisphere
    hemisphere <- ifelse(X.i > 0, "N", "S")
    UTMzone <- paste0("UTM ", zone, hemisphere)
    UTMepsg <- ifelse(X.i > 0, 32600 + zone, 32700 + zone)
    df.i <- setNames(data.frame(t(c(UTMzone, UTMepsg))), c("Zone", "EPSG"))
    sf.i <- st_as_sf(df.org[i, ], coords=c("X", "Y"), crs = 4326)
    sf.i <- st_transform(sf.i, crs = UTMepsg)
    df.i <- cbind(as.data.frame(sf.i), st_coordinates(sf.i), df.i)
    df.i$Name <- paste0("WGS 84 / UTM zone ", zone, hemisphere)
    df.i$Datum <- "World Geodetic System 1984" 
    df.i <- df.i[, c("ID", "X", "Y", "Name", "Datum", "Zone", "EPSG")]
    names(df.i)[names(df.i) == "X"] <- "Easting"
    names(df.i)[names(df.i) == "Y"] <- "Northing"
    df.new <- rbind(df.new, df.i)
  }
  return(df.new)
}  
##'
##' function that re-projects coordinates to WGS84 lat/long
To_WGS84 <- function(id, x, y, epsg){
  # dependencies: sf
  df.org <- data.frame(ID = id, X = x, Y = y, EPSG = epsg)
  df.new <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                     c("ID", "Latitude", "Longitude"))
  for(i in 1:length(x)){
    CRS <- as.numeric(df.org[i, "EPSG"])
    sf.i <- st_as_sf(df.org[i, ], coords=c("X", "Y"), crs = CRS)
    sf.i <- st_transform(sf.i, crs = 4326)
    df.i <- cbind(as.data.frame(sf.i), st_coordinates(sf.i))
    df.i <- df.i[, c("ID", "Y", "X")]
    names(df.i)[names(df.i) == "X"] <- "Longitude"
    names(df.i)[names(df.i) == "Y"] <- "Latitude"
    df.new <- rbind(df.new, df.i)
  }
  return(df.new)
}  
##'
##'
### FUNCTIONS LOADED ###########################################################
##'_____________________________________________________________________________
##'
### INSTALL / LOAD REQUIRED PACKAGES ###########################################
##'_____________________________________________________________________________
##'
requiredPackages <- c("sf")
loadPackages(requiredPackages)
##'
### REQUIRED PACKAGES INSTALLED / LOADED #######################################
##'_____________________________________________________________________________
##'
### CREATE DIRECTORIES #########################################################
##'_____________________________________________________________________________
##'  
##' define absolute path to working directory
if(!exists("base.dir")) base.dir <- getwd()
if(!exists("export.dir")) {
  export.dir <- file.path(base.dir, "DataExport")
}
##'
##'  creates the working/export directories
if(length(intersect(list.dirs(base.dir, full.names = FALSE),
                    "DataExport")) == 0) {
  dir.create(export.dir)
}
tmp <- file.path(export.dir, "tmp")
if(length(intersect(list.dirs(export.dir, full.names = FALSE),
                    "tmp")) == 0) {
  dir.create(tmp)
}
##'
##' Output name appendix/ID
if(exists("OutName"))  OUTID <- OutName
##'
### DIRECTORIES CREATED ########################################################
##'_____________________________________________________________________________
##'
##'_____________________________________________________________________________
##'
### DATA PROCESSING ############################################################
##'                                                                              
##' Start of data processing section, no modifications are needed here           
##'_____________________________________________________________________________
##'
setwd(target.directory)
##'
##' creating file names lists and extracting file name identifier
##'_____________________________________________________________________________
##'
##' finding all point list files in the target directory
txt.files <- list.files(target.directory, pattern = "\\.txt", recursive = TRUE)
txt.files <- txt.files[!grepl("DataExport", txt.files)]
pointlist.files <- txt.files[grep(pointlist.file, txt.files)]
UTM.files <- pointlist.files[grep(Fext, basename(pointlist.files))]
WGS.files <- setdiff(pointlist.files, UTM.files)
UTM.m.files <- UTM.files[grep("metadata", UTM.files)]
UTM.files <- setdiff(UTM.files, UTM.m.files)
WGS.m.files <- WGS.files[grep("metadata", WGS.files)]
WGS.files <- setdiff(WGS.files, WGS.m.files)
rm(txt.files)
##'
##' finding file name identifier
IDs.UTM <- NULL
IDs.WGS <- NULL
if(!exists("fileID")) {
  if(length(length(UTM.files)) == 1) {
    IDs.UTM <- basename(sub("_UTM\\.txt", "", UTM.files))
    IDs.UTM <- sub(paste0(pointlist.file, "_"), "", IDs.UTM)
  }  
  if(length(length(UTM.files)) > 1) {
    IDs.UTM <- FileIdFinder(UTM.files, "txt", "_")
  }
  if(length(length(WGS.files)) == 1) {
    IDs.WGS <- basename(sub("\\.txt","", WGS.files))
    IDs.WGS <- sub(paste0(pointlist.file, "_"), "", IDs.WGS)
  }
  if(length(length(WGS.files)) > 1) {
    IDs.WGS <- FileIdFinder(WGS.files, "txt", "_")
  }
  IDs <- sort(unique(c(IDs.UTM, IDs.WGS)))
} else {IDs <- fileID}
##'
##' creating data frames for import and compiling data 
df.names <- c("ID","GCP","Latitude", "Longitude","HeightAE",
              "Easting", "Northing", "Elevation",
              "Datum", "Zone", "Geoid", "EPSG", "Name", "File")
df <- setNames(data.frame(matrix(ncol = 14, nrow = 0)), df.names)

##'
##' writing first log file entry
log.entry <- paste0("Compiling TBC point list files (", Sys.time(), "):\n\n")
log.entry.WGS <- "Only WGS files in target directory:\n"; c.WGS = 0
log.entry.UTM <- "Only UTM files in target directory:\n"; c.UTM = 0
##'
##' extracting and merging file information
##'___________________________________________________________________________
##'
##' loop through the file name identifier
for(i in 1:length(IDs)) {
  ID <- paste0("_", IDs[i], "[_\\.]")
  n.WGS.files <- length(grep(ID, WGS.files))
  n.UTM.files <- length(grep(ID, UTM.files))
  ##' check if file name ID is ambiguous 
  if(n.WGS.files > 1 | n.UTM.files > 1) {
    warning(paste0("The file name identifier is ambiguous.\n",
                   "No file with identifier \"", IDs[i],
                   "\" has been processed.\n", "Check LOG for detail!"))
    log.entry <- paste0(log.entry,
                        "File(s) with name ID \"", IDs[i],
                        "\" has/have not been processed due to ID ambiguity (",
                        format(Sys.time(), "%X"), "):\n",
                        "- file(s) <", WGS.files[grep(ID, WGS.files)],
                        "> and\n",
                        "- file(s) <", UTM.files[grep(ID, UTM.files)],
                        "> \n\n")
  }
  ##' loop through UTM and WGS file pairs
  if(n.WGS.files == 1 && n.UTM.files == 1) {
    df.WGS <- read.delim(WGS.files[grep(ID, WGS.files)])
    df.UTM <- read.delim(UTM.files[grep(ID, UTM.files)])
    df.i <- merge(df.WGS, df.UTM, by = intersect(names(df.WGS), names(df.UTM)))
    FileName <- basename(WGS.files[grep(ID, WGS.files)])
    df.i$File <- FileName
    if(length(grep(ID, UTM.m.files)) == 1) {
      df.UTM.m <- read.delim(UTM.m.files[grep(ID, UTM.m.files)], header = TRUE)
      COLS <- c("Datum", "Zone", "Geoid", "EPSG", "Name")
      df.i[, COLS] <- df.UTM.m[, COLS]; rm(COLS)
    }
    if(length(df.names) != length(names(df.i))) {
      df.i[, setdiff(df.names, names(df.i))] <- NA
    }
    df.i <- df.i[, intersect(df.names, names(df.i))]
    df <- df[, names(df.i)]
    df <- rbind(df,df.i)
    log.entry <- paste0(log.entry,
                        "Files with name ID \"", IDs[i], "\" processed (",
                        format(Sys.time(), "%X"), "):\n",
                        "- file <", WGS.files [grep(ID, WGS.files)], "> and\n",
                        "- file <", UTM.files [grep(ID, UTM.files)], "> \n\n",
                        if(!is.null(warnings())) {
                          paste0("Warnings:", names(warnings()), "\n\n")
                        })
  }
  ##' loop through WGS only files
  else if(n.WGS.files == 1 && n.UTM.files == 0) {
    c.WGS = c.WGS + 1
    df.WGS <- read.delim(WGS.files[grep(ID, WGS.files)])
    df.UTM <- LatLong_to_UTM(df.WGS$ID, df.WGS$Latitude, df.WGS$Longitude)    
    df.i <- merge(df.WGS, df.UTM, by = intersect(names(df.WGS), names(df.UTM)))
    FileName <- basename(WGS.files[grep(ID, WGS.files)])
    df.i$File <- FileName
    if(length(df.names) != length(names(df.i))) {
      df.i[, setdiff(df.names, names(df.i))] <- NA
    }
    df.i <- df.i[, intersect(df.names, names(df.i))]
    df <- df[, names(df.i)]
    df <- rbind(df,df.i)
    log.entry.WGS <- paste0(log.entry.WGS, 
                            "- file <", WGS.files [grep(ID, WGS.files)], ">\n",
                            if(!is.null(warnings())) {
                              paste0("  warnings:", names(warnings()), "\n\n")
                            })
  }
  ##' loop through UTM only files
  else if(n.WGS.files == 0 && n.UTM.files == 1) {
    c.UTM = c.UTM + 1
    df.i <- read.delim(UTM.files[grep(ID, UTM.files)])
    if(length(grep(ID, UTM.m.files)) == 1) {
      df.i.m <- read.delim(UTM.m.files[grep(ID, UTM.m.files)], header = TRUE)
      COLS <- c("Datum", "Zone", "Geoid", "EPSG", "Name")
      df.i[, COLS] <- df.i.m[, COLS]; rm(COLS)
    }
    if(length(df.i$EPSG) != 0){
      df.WGS <- To_WGS84(df.i$ID, df.i$Easting, df.i$Northing, df.i$EPSG)
      df.i <- merge(df.WGS, df.i, by = intersect(names(df.WGS), names(df.i)))
    }
    FileName <- basename(UTM.files[grep(ID, UTM.files)])
    df.i$File <- FileName
    if(length(df.names) != length(names(df.i))) {
      df.i[, setdiff(df.names, names(df.i))] <- NA
    }
    df.i <- df.i[, intersect(df.names, names(df.i))]
    df <- df[, names(df.i)]
    df <- rbind(df,df.i)
    log.entry.UTM <- paste0(log.entry.UTM, 
                            "- file <", UTM.files [grep(ID, UTM.files)], ">\n",
                            if(!is.null(warnings())) {
                              paste0("  warnings:", names(warnings()), "\n\n")
                            })
  } else {warning(paste0("No file with identifier \"", IDs[i],
                         "\" in the target directory!"))}
}  
##'
##' re-name File column
names(df)[names(df) == "File"] <- "PointListFile"
##'
##'
log.entry <- paste0(log.entry, "\n",
                    if(c.WGS > 0) {paste0(log.entry.WGS, "\n")},
                    if(c.UTM > 0) {paste0(log.entry.UTM, "\n")},
                    "End of processing (",
                    format(Sys.time(), "%X"), ")\n",
                    if(!is.null(warnings())) {
                      paste0("Warnings:", names(warnings()), "\n\n")
                    })
##' 
##' output data frame and write log file 
setwd(tmp)
df <- df[order(df$PointListFile, df$GCP), ]
if(exists("fileID")){
  pl.f <- paste0(pointlist.file, "_", fileID, "_")
} else {pl.f <- paste0(pointlist.file, "_")} 
OUTID <- paste0(pl.f, OUTID,".txt")
if(EXPORT){
  write.table(df, file = OUTID, row.names = FALSE, quote = FALSE, sep = "\t")
  save(df, file = paste0(sub(".txt", "", OUTID), ".Rda"))
}
OUTID <- paste0(sub(".txt", "", OUTID), "_Processing.log")
cat(log.entry, append = FALSE, file = OUTID)
setwd(base.dir)
##' 
##' 
### END OF DATA PROCESSING #####################################################
##'_____________________________________________________________________________
##'
### UNLOAD REQUIRED PACKAGES ###################################################
##'_____________________________________________________________________________
##'
if(EXPORT){
  listOfAllAddOnPackages <- names(sessionInfo()$otherPkgs)
  if(length(listOfAllAddOnPackages) > 0) {
    print(paste0("List of add on packages: ", listOfAllAddOnPackages))
    sapply(paste0("package:", requiredPackages),
           detach, unload = TRUE, character.only = TRUE)
    listOfAllAddOnPackages <- names(sessionInfo()$otherPkgs)
    print(paste0("List of add on packages: ", listOfAllAddOnPackages))
  }; rm("listOfAllAddOnPackages")
}
##'
##'  clean work space
rm(list= setdiff(ls(),
                 c("df", "df.imp", "df.final",
                   "target.directory", "OutName",
                   "fileIDs", "fileID.n", "fileID", 
                   "imp.file", "import.files", "proc.file", "processing.files",
                   "base.dir", "export.dir", "tmp",
                   "StartTime", "CodeExecutionDuration")))
##'_____________________________________________________________________________
##'
### REQUIRED PACKAGES UNLOADED #################################################
##'_____________________________________________________________________________
##'