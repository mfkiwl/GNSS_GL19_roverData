### HEADER #####################################################################
##' @title        Data Cleaning (6-GNSS_Data_Cleaning.R)
##' @author       Urs A. Treier
##' @contact      urs.treier at gmail.com
##' @date         31/05/2021
##' @description  This script compiles steps of data cleaning and manual editing
##'               of the GL19 GNSS quality data set, i.e. the df.q data frame.
##'               The cleaning / editing steps comprise:
##'                - consistent labeling of measurements
##'                - adding / substitute incomplete labels
##'                - deletion of non merged (bad quality) measurements
##'                - correction of faulty automatic merged measurements.
##'                
##' @note
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
##'
##' Name of file to be processed
if(!exists("df.q")) {
  FILE <- "GNSS_Data_GL19_202203021452.rda"
}
##'
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
##' function that installs and loads required packages
loadPackages <- function(Pkgs) {
  new.Pkgs <- Pkgs[!(Pkgs %in% installed.packages()[, "Package"])]
  if (length(new.Pkgs) > 0) {
    install.packages(new.Pkgs, dependencies = TRUE)
  }  
  sapply(Pkgs, require, character.only = TRUE)
} 
##'  
# function that creates map widgets showing points of imputed coordinates
map_widget <- function(id, lat, long) {
  # dependencies: leaflet
  mw <- leaflet()
  # select map providers (call "providers" to see options)
  esri <- grep("^Esri", providers, value = TRUE)[c(5, 2, 4, 10)]
  for (provider in esri) {
    # add map tiles and define options, e.g. min/max zoom)
    mw <- mw %>%
      addProviderTiles(provider, group = provider,
                       options = providerTileOptions(minZoom = 0,
                                                     maxZoom = 100))    
  }
  mw <- mw %>%
    # widgets and layout, e.g. marker options
    addCircleMarkers(lat = lat, lng = long, label = id,
                     opacity = 0.8, fillOpacity = 0.8,
                     weight = 1, radius = 5,
                     color = "black", fillColor = "white") %>%  
    addLayersControl(baseGroups = names(esri),
                     options = layersControlOptions(collapsed = TRUE)) %>%
    addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters")
  return(mw)
}
##'
### FUNCTIONS LOADED ###########################################################
##'_____________________________________________________________________________
##'
### INSTALL / LOAD REQUIRED PACKAGES ###########################################
##'_____________________________________________________________________________
##'
requiredPackages <- c("leaflet")
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
##' load the file to process: df.final
if(exists("export.dir") & exists("FILE")) {
  setwd(export.dir) 
  load(FILE)
  if(exists("Data")) {
    df.final <- Data
    rm(Data)
  }
  setwd(base.dir)
}
##'
##' load file containing feature codes
if(!exists("df.codes")) {
  FeatureCodes <- list.files(export.dir,
                             pattern = paste0("GNSS_Data_",
                                              OutName,
                                              "_FeatureCodes",
                                              "-PointIDs.rda"),
                             recursive = T, full.names = T)
  if(length(FeatureCodes) == 1) load(FeatureCodes)  
}
##'
##' creating the new, edited data frame
df.e <- df.q
##'
##' start log entry
LogFile <- paste0("GNSS_Data_", OutName, "_Editing_of_Measurements.log")
setwd(tmp) 
cat(paste0(Sys.time(), "\n -> Editing of Data Entries\n\n",
           if(!exists("OutName")) {
             paste0("File: ", FILE)
           } else {paste0("GNSS_Data: ", OutName)},
           "\n\n"),
    append = FALSE, file = LogFile)
setwd(base.dir)
##' end log entry 
##'
##' Start correcting typos in data labels
##' -> LDI label editing
df.e[df.e$GCP == "LDI_12", "GCP"] <- "LDI_012"
df.e[df.e$GCP == "LDI_61", "GCP"] <- "LDI_061"
df.e[df.e$GCP == "LDI_62", "GCP"] <- "LDI_062"
df.e[df.e$GCP == "LDI_63", "GCP"] <- "LDI_063"
df.e[df.e$GCP == "LDI_64", "GCP"] <- "LDI_064"
df.e[df.e$GCP == "LDI_65", "GCP"] <- "LDI_065"
df.e[df.e$GCP == "LDI_66", "GCP"] <- "LDI_066"
df.e[df.e$GCP == "LdI_042", "GCP"] <- "LDI_042"
##'
##' -> correcting missing LDI_067 label, i.e. LDI_070? to LDI_067
LDI_067 <- paste0(df.codes[grep("LDI_067", df.codes[, "GCP"]), "ID"],
                  collapse = ";")
df.e[df.e$ID == LDI_067 & df.e$GCP == "LDI_070?", "GCP"] <- "LDI_067"
rm(LDI_067)
##'
##' -> correcting wrong LDI_022 label, i.e. LDI_023 to LDI_022
LDI_022 <- paste0(df.codes[grep("LDI_022", df.codes[, "GCP"]), "ID"],
                  collapse = ";")
df.e[df.e$ID == LDI_022 & df.e$GCP == "LDI_023", "GCP"] <- "LDI_022"
rm(LDI_022)
##'
##' -> discard the first of the two measurements of LDI_033 
df.search <- df.e[grep("LDI_033", df.e[, "GCP"]), ]
df.skip <- df.search[df.search[, "Time"] == min(df.search[, "Time"]), ]
df.e <- df.e[!(row.names(df.e) %in% row.names(df.skip)), ]
##'
##' -> discard the non-merged measurements of LDI_084 
df.search <- df.e[grep("LDI_084", df.e[, "GCP"]), ]
df.skip <- df.search[df.search[, "GrE"] == 1, ]
df.e <- df.e[!(row.names(df.e) %in% row.names(df.skip)), ]
##'
##'
##'
##'
##'
df.LDI <- df.e[grep("LDI",df.e[, "GCP"]), ]
df.search <- df.e[grep("LDI_0[6-7]", df.e[, "GCP"]), ]
df.search <- df.e[grep("LDI_023", df.e[, "GCP"]), ]
##'
map_widget(df.search$GCP, df.search$Latitude, df.search$Longitude)
##'
##'
##'
##'
if(!is.null(warnings())){
  cat(paste0("Warnings:", names(warnings()), "\n\n"),
      append = TRUE, file = LogFile)
}
##' end log entry 
##'
##'
##'
##' "(", format(Sys.time(), "%X"), "):\n",'
##'
##'
##'
##' grouping measurements with equal labels from the same date
 
##' start log entry
setwd(tmp)
cat(paste0("\n -> finished processing i.e., ",
           "replicated measurements replaced by their mean.\n",
           "    ", Sys.time(), "\n",
           " -> warnings: ", names(warnings()), "\n"),
    append = TRUE, file = LogFile)
setwd(base.dir)
##' end log entry 
### END OF DATA PROCESSING #####################################################
##'
### DATA EXPORT ################################################################
##'
##' LDI_061-066 -> data provided to Beat Frei, WSL, CH
setwd(export.dir) 
df.search <- df.e[grep("LDI_06[1-6]", df.e[, "GCP"]),
                  c("GCP", "Latitude", "Longitude", "HeightAE",
                    "Easting", "Northing", "Elevation",
                    "Datum", "Zone", "Geoid", "EPSG", "Name",
                    "hPrec", "vPrec")]

colnames(df.search)[colnames(df.search) == "GCP"] <- "plot_id"
write.table(df.search,
            file = "Core1_Coordinates.csv",
            row.names = FALSE, quote = FALSE, sep = ",", dec = ".",)  
setwd(base.dir)
##'
##' LDI_001-090 -> data provided to Beat Frei, WSL, CH
setwd(export.dir)
df.search <- df.e[grep("LDI_0[0-9][0-9]", df.e[, "GCP"]),
                  c("GCP", "Latitude", "Longitude", "HeightAE",
                    "Easting", "Northing", "Elevation",
                    "Datum", "Zone", "Geoid", "EPSG", "Name",
                    "hPrec", "vPrec", "GrE", "Time")]
df.search$Time <- paste0("'", df.search$Time)
df.search$Time <- gsub(";", "; ", df.search$Time)
colnames(df.search)[colnames(df.search) == "GCP"] <- "PlotID"
colnames(df.search)[colnames(df.search) == "GrE"] <- "Measurements"
df.search <- df.search[order(df.search$PlotID, df.search$Time), ]
x <- paste0("LDI_Coordinates_", format(Sys.time(), "%Y%m%d%H%M"), ".csv")
write.table(df.search,
            file = x, row.names = FALSE, quote = FALSE, sep = ",", dec = ".",)  
rm(x)
setwd(base.dir)
##'
##'
##'
### END OF DATA EXPORT #########################################################


### CALCULATION OF SOME STATS ##################################################
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
##' function that installs and loads required packages
loadPackages <- function(Pkgs) {
  new.Pkgs <- Pkgs[!(Pkgs %in% installed.packages()[, "Package"])]
  if (length(new.Pkgs) > 0) {
    install.packages(new.Pkgs, dependencies = TRUE)
  }  
  sapply(Pkgs, require, character.only = TRUE)
} 
##' Install / Load Required Packages ###########################################
requiredPackages <- c("sf")
loadPackages(requiredPackages)
##'
##' Calculated error statistics in meter #######################################
df.search <- df.e[grep("LDI_06[1-2]", df.e[, "GCP"]),
                  c("GCP", "Longitude", "Latitude")]
df.search.orig <- df.search
df.search.UTM <- LatLong_to_UTM(df.search$GCP,
                                df.search$Latitude,
                                df.search$Longitude)  
##'
df.search$Longitude <- df.search.orig$Longitude + 0.00000001
df.search$Latitude <- df.search.orig$Latitude + 0.00000015
df.search.UTM.min <- LatLong_to_UTM(df.search$GCP,
                                    df.search$Latitude,
                                    df.search$Longitude)
##'
df.search$Longitude <- df.search.orig$Longitude + 0.00002140
df.search$Latitude <- df.search.orig$Latitude + 0.00002649
df.search.UTM.max <- LatLong_to_UTM(df.search$GCP,
                                    df.search$Latitude,
                                    df.search$Longitude)
##'
df.search$Longitude <- df.search.orig$Longitude + 0.00000618
df.search$Latitude <- df.search.orig$Latitude + 0.00000351
df.search.UTM.average <- LatLong_to_UTM(df.search$GCP,
                                        df.search$Latitude,
                                        df.search$Longitude)
##'
mean(df.search.UTM$Easting - df.search.UTM.min$Easting)
mean(df.search.UTM$Northin - df.search.UTM.min$Northing)
##'
mean(df.search.UTM$Easting - df.search.UTM.max$Easting)
mean(df.search.UTM$Northin - df.search.UTM.max$Northing)
##'
mean(df.search.UTM$Easting - df.search.UTM.average$Easting)
mean(df.search.UTM$Northin - df.search.UTM.average$Northing)
##'
##' Unload Required Packages ###################################################
##'
listOfAllAddOnPackages <- names(sessionInfo()$otherPkgs)
if(length(listOfAllAddOnPackages) > 0) {
  print(paste0("List of add on packages: ", listOfAllAddOnPackages))
  sapply(paste0("package:", requiredPackages),
         detach, unload = TRUE, character.only = TRUE)
  listOfAllAddOnPackages <- names(sessionInfo()$otherPkgs)
  print(paste0("List of add on packages: ", listOfAllAddOnPackages))
}; rm("listOfAllAddOnPackages")