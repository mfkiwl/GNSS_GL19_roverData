### HEADER #####################################################################
##' @title        Merge Repeated Measures (4-GNSS_Merge_Repeated_Measurements.R)
##' @author       Urs A. Treier
##' @contact      urs.treier at gmail.com
##' @date         06/10/2021
##' @description  This script attempts to find replicated GNSS measurements.
##'               Replicated measurements, i.e. measurements within the user
##'               defined tolerance will be merged. The user needs to define the
##'               tolerance for the GNSS measurements in meters:
##'               horizontal = hTol, vertical = vTol
##'               Quality levels for merging are defined as follows:
##'               1: 1x tolerance of the GNSS measures or
##'                  the measurement appears only once, is not replicated.
##'               2: 2x tolerance of the GNSS measures
##'               4: 4x tolerance of the GNSS measures
##'               5: outside the tolerance levels
##'               If the individual measurements come with precision data, these 
##'               are taken into account in the quality assignments.
##'               Final output is a data frame (df.q) with attached quality
##'               levels and no replicated measurements withing the defined
##'               tolerance. Measurements that have not been discarded in the
##'               merging/averaging process listed in another data frame (df.l).
##'               See log file for details. 
##' @note         I follow the UTM convention:
##'                 X increases from west to east, and is called "easting"
##'                 Y increases from south to north, and is called "northing"
##'                 Z increases from down to up, and is called "elevation"
##' @log          Processing logs will be saved in the exports tmp directory.
##' @dependencies 
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
  ##'
  ##' Name of file to be processed
  FILE <- "GNSS_Data_GL19_20210531190201.rda"
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
##' define the tolerance of the GNSS measurements [meters]:
##' horizontal = hTol, vertical = vTol
hTol <- 0.02
vTol <- 0.04
##'
### END OF DEFINITION OF VARIABLES #############################################
##'
### DATA PROCESSING ############################################################
##
##' Start of data processing section, no modifications are needed here
##'_____________________________________________________________________________
##'
##' 
##' load the file to process: df.final
if(exists("export.dir") & exists("FILE")) {
  setwd(export.dir) 
  load(FILE)
  if(exists("Data")) df.final <- Data ; rm(Data)
  setwd(base.dir)
}
##'
##' start log entry
LogFile <- paste0("GNSS_Data_", OutName, "_Merge_Repeated_Measurements.log")
setwd(tmp) 
cat(paste0(Sys.time(), "\n-> Find and merge repeated GNSS measurements\n\n",
           if(!exists("OutName")) {
             paste0("File: ", FILE)
           } else {paste0("GNSS_Data: ", OutName)},
           "\n\n"),
    append = FALSE, file = LogFile)
##'
if(!is.null(warnings())){
  cat(paste0("-> warnings: ", names(warnings()), "\n\n"),
      append = TRUE, file = LogFile)
}
##'
cat(paste0("Set tolerance levels for GNSS measurements [m]:\n",
           "delta X/Y = ", hTol, "; ", "delta Z = ", vTol, "\n\n"),
           append = TRUE, file = LogFile)
setwd(base.dir)
##' end log entry 
##'
##' grouping measurements with equal labels from the same date
df <- df.final
df$Date <- as.character(as.Date(df$Time))
df$Time <- as.character(df$Time)
df$UTM.Time <- as.character(df$UTM.Time)
df <- df[order(df$Date, df$GCP, df$Time), ]
df$GrID <- c(1:nrow(df))
n <- union(union(c("GrID", "GCP", "Date"), names(df)[names(df) != "ID"]), "ID")
df <- df[, n]; rm(n)
row.names(df) <- c(1:nrow(df))
##'
##' averaging horizontal and vertical precision data, if available
df$hPrec <- NA
df$vPrec <- NA
df$dBase <- NA
df$nBase <- NA
##'
##' register "NA" entries as real NA values for the entire data frame
df[] <- lapply(df, function(x) {
  is.na(levels(x)) <- levels(x) == "NA"
  x
})
##'
##' loop through all lines in the data frame
for(l in 1:nrow(df)) { # l = 47
  for(Prec in (c("hPrec", "vPrec"))) { # Prec = "hPrec"
    x <- df[l, names(df)[grepl(Prec, names(df))]]
    ##'
    ##' names of columns with data entries
    n <- names(colSums(is.na(x))[colSums(is.na(x)) == 0])
    ##'
    ##' deleting all columns with NA entries
    x <- x[, colSums(is.na(x)) < nrow(x)]
    ##'
    ##' adding column entries for PKK processed measurements with only one base
    if(length(x) == 1 && Prec == "hPrec") {
      df[l, "dBase"] <- df[l, paste0("DistTo", sub(Prec, "", n))]
      df[l, "nBase"] <- length(x)
    }
    if(length(x) == 1) df[l, Prec] <- x
    ##'
    ##' adding column entries for PKK processed measurements with several bases
    PREC.s = 0
    DistTo.s = 0
    DistTo.inv.s = 0
    if(length(x) > 1) {
      for(j in names(x)) {
        ##'
        ##' inverse distance to the base
        DistTo <- as.numeric(df[l, paste0("DistTo", sub(Prec, "", j))])
        DistTo.s <- DistTo.s + DistTo
        DistTo.inv <- 1 / DistTo
        DistTo.inv.s <- DistTo.inv.s + DistTo.inv
        ##'
        ##' precision weighted by the inverse distance
        PREC <- DistTo.inv * as.numeric(df[l, j])
        PREC.s <- PREC.s + PREC
      }
      ##'
      ##' weighted average of precision
      df[l, Prec] <- PREC.s / DistTo.inv.s
      df[l, "dBase"] <- DistTo.s / length(x)
      df[l, "nBase"] <- length(x)
    }
  }
} 
##'
##' clean work space
rm(list= setdiff(ls(),
                 c("FILE", "fileIDs", "fileID.n",
                   "df", "df.final", "base.dir", "export.dir", "tmp",
                   "hTol", "vTol", "OutName", "LogFile",
                   "StartTime", "CodeExecutionDuration")))
##'
##' defining the "reduced" data frame
df.r <- df
df.r[, c("deltaX", "deltaY", "deltaZ", "Qual", "Gr")] <- NA
df.r <- df.r[FALSE, ]
##'
##' doing a first search for replicated GNSS measurements
##' i.e., search radius is 5 times of the defined tolerance 
for(r in 1:nrow(df)) { # r = 14
  lbound <- df[r, "Elevation"] - (5 * vTol)
  ubound <- df[r, "Elevation"] + (5 * vTol)
  df.s <- df[df$Elevation >= lbound & df$Elevation <= ubound, ]
  lbound <- df[r, "Easting"] - (5 * hTol)
  ubound <- df[r, "Easting"] + (5 * hTol)  
  df.s <- df.s[df.s$Easting >= lbound & df.s$Easting <= ubound, ]
  lbound <- df[r, "Northing"] - (5 * hTol)
  ubound <- df[r, "Northing"] + (5 * hTol)  
  df.s <- df.s[df.s$Northing >= lbound & df.s$Northing <= ubound, ]
  ##'
  ##' calculating the median for coordinates within the search radius
  if(nrow(df.s) == 1) { # only one measurement found
    df.s[,c("deltaX", "deltaY", "deltaZ")] <- NA
    df.s$Qual <- 1
    df.s$Gr <- df.s$GrID
    df.r <- rbind(df.r,df.s)
  } else { # several measurement found
    df.s <- df.s[order(df.s$Time), ]
    df.m <- df.s[1, ]
    df.m[1, ] <- NA
    df.m$Northing <- median(df.s$Northing)
    df.m$Easting <- median(df.s$Easting)
    df.m$Elevation <- median(df.s$Elevation)
    Group <- paste(sort(unique(df.s$GrID)), collapse = ";")
    ##'
    ##' finding replicated GNSS measurements within the tolerance, Quality 1
    t.lX <- df.m$Easting - hTol
    t.hX <- df.m$Easting + hTol    
    t.lY <- df.m$Northing - hTol
    t.hY <- df.m$Northing + hTol
    t.lZ <- df.m$Elevation - vTol
    t.hZ <- df.m$Elevation + vTol
    df.t <- df.s[df.s$Elevation >= t.lZ & df.s$Elevation <= t.hZ, ]
    df.t <- df.t[df.t$Easting >= t.lX & df.t$Easting <= t.hX, ]
    df.t <- df.t[df.t$Northing >= t.lY & df.t$Northing <= t.hY, ]
    df.t <- df.t[order(df.t$Date, df.t$ID), ]
    ##'
    ##' creating the new data frame
    if(nrow(df.t) > 1) { # several measurements within tolerance found
      df.m <- df.t[1, ]
      df.m[1, ] <- NA
      ##'
      ##' concatenate unique content for each variable
      VARs <- setdiff(names(df.m), c("Latitude", "Longitude", "HeightAE",
                                     "Easting", "Northing", "Elevation",
                                     "hPrec", "vPrec", "dBase",
                                     "Gr", "Qual",
                                     "deltaX", "deltaY", "deltaZ"))
      for(VAR in VARs) {
        if(VAR %in% names(df.t)) {
          df.m[, VAR] <- paste(unique(df.t[, VAR]), collapse = ";")
        }  
      }
      ##'
      ##' calculate the mean for coordinate variables
      VARs <- intersect(names(df.m), c("Latitude", "Longitude", "HeightAE",
                                       "Easting", "Northing", "Elevation",
                                       "hPrec", "vPrec", "dBase")) 
      for(VAR in VARs) {
        if(VAR %in% names(df.t)) {
          df.m[, VAR] <- mean(as.numeric(df.t[, VAR]))
        }  
      }
      rm(VARs, VAR)
      ##'
      ##' calculate differences for x,y,z coordinates
      df.m$deltaX <- max(df.t$Easting) - min(df.t$Easting)
      df.m$deltaY <- max(df.t$Northing) - min(df.t$Northing)
      df.m$deltaZ <- max(df.t$Elevation) - min(df.t$Elevation)
      ##'
      ##' measurements within the GPS tolerance, i.e. Quality 1
      df.m$Gr <- Group  
      df.m$Qual <- 1
      ##'
      ##' measurements outside GPS tolerance, i.e. Quality 5
      if(nrow(df.t) ==! nrow(df.s)) { # i.e. measurements that were not merged
        df.l <- df.s[!df.s$GrID%in%df.t$GrID, ]
        df.l$deltaX <- max(df.s$Easting) - min(df.s$Easting)
        df.l$deltaY <- max(df.s$Northing) - min(df.s$Northing)
        df.l$deltaZ <- max(df.s$Elevation) - min(df.s$Elevation)
        df.l$Qual <- 5
        df.l$Gr <- Group
        df.r <- rbind(df.r, df.m, df.l) # add Quality 1 and Quality 5 data
      } else {
        df.r <- rbind(df.r, df.m) # only Quality 1 data to add
      }
    } else if (nrow(df.t) == 1){ # only one measurement within tolerance
      df.t[ , c("deltaX", "deltaY", "deltaZ")] <- NA
      ##'
      ##'  measurement within the GPS tolerance, i.e. Quality 1
      df.t$Qual <- 1
      df.t$Gr <- Group
      ##'
      ##'  measurements outside GPS tolerance, i.e. Quality 5
      df.l <- df.s[!df.s$GrID%in%df.t$GrID,]
      df.l$deltaX <- max(df.s$Easting) - min(df.s$Easting)
      df.l$deltaY <- max(df.s$Northing) - min(df.s$Northing)
      df.l$deltaZ <- max(df.s$Elevation) - min(df.s$Elevation)
      df.l$Qual <- 5
      df.l$Gr <- Group
      df.r <- rbind(df.r, df.t, df.l)
    } else { # no measurement within tolerance
      df.s$deltaX <- max(df.s$Easting) - min(df.s$Easting)
      df.s$deltaY <- max(df.s$Northing) - min(df.s$Northing)
      df.s$deltaZ <- max(df.s$Elevation) - min(df.s$Elevation)
      ##'
      ##' all measurements outside GPS tolerance, i.e. Quality 5
      df.s$Qual <- 5
      df.s$Gr <- Group
      df.r <- rbind(df.r, df.s)
    }  
  }
}
df.r <- unique(df.r)
df.r <- df.r[order(df.r$Date, df.r$GCP, df.r$Time), ]
row.names(df.r) <- c(1:nrow(df.r))
##'
##' clean work space
rm(list= setdiff(ls(),
                 c("FILE", "fileIDs", "fileID.n",
                   "df", "df.final", "df.r",
                   "base.dir", "export.dir", "tmp",
                   "hTol", "vTol",
                   "OutName", "LogFile",
                   "StartTime", "CodeExecutionDuration")))
##'
##' Extracting number of Group Elements, i.e. measurements
a <- strsplit(as.character(df.r[, "Gr"]), ";")
df.r$GrE <- NA
for(i in 1:length(a)){
  df.r[i, "GrE"] <- length(a[[i]])
}
rm(a, i)
##'
##' Calculating the summed error of repeated measurements
df.r$deltaXYZ <- df.r$deltaX + df.r$deltaY + df.r$deltaZ
##'
##' Eliminating identical coordinate entries
df.r.bu <- df.r
df.r <- df.r[FALSE, ] 
for(r in 1:nrow(df.r.bu)) { # r=2
  lbound <- df.r.bu[r,"Elevation"] - 0.002
  ubound <- df.r.bu[r,"Elevation"] + 0.002
  df.s <- df.r.bu[df.r.bu$Elevation >= lbound & df.r.bu$Elevation <= ubound, ]
  lbound <- df.r.bu[r,"Easting"] - 0.001
  ubound <- df.r.bu[r,"Easting"] + 0.001  
  df.s <- df.s[df.s$Easting >= lbound & df.s$Easting <= ubound, ]
  lbound <- df.r.bu[r,"Northing"] - 0.001
  ubound <- df.r.bu[r,"Northing"] + 0.001  
  df.s <- df.s[df.s$Northing >= lbound & df.s$Northing <= ubound, ]
  if(nrow(df.s)==1){
    df.r <- rbind(df.r, df.s)
  }else{
    df.s <- df.s[order(df.s$Qual, -df.s$GrE, df.s$Date),]
    df.r <- rbind(df.r, df.s[1, ])
  }
}
df.r <- unique(df.r)
rm(r, lbound, ubound, df.s)
##'
##' Finding single measurements now contained in the averages 
GrIDs_Rows <- strsplit(as.character(df.r[,"GrID"]), ";")
GrIDs <- sort(as.numeric(unlist(GrIDs_Rows)))
GrIDs <- as.character(unique(GrIDs[duplicated(GrIDs)]))
##'
ToDel <- NULL
for(GrID in GrIDs){
  a <- df.r[grepl(paste0("\\b",GrID,"\\b"), GrIDs_Rows), ]
  a <- a[order(a$Qual, -a$GrE, a$deltaXYZ), ]
  if(length(row.names(a) > 1)){
  ToDel <- c(ToDel, row.names(a)[-1])
  }
}
##'
##' Deleting the identified single measurements 
ToDel <- unique(ToDel)
for(i in ToDel) df.r <- df.r[!row.names(df.r) == i, ]
##'
##' clean work space
rm(list= setdiff(ls(),
                 c("FILE", "fileIDs", "fileID.n",
                   "df", "df.final", "df.r", "df.r.bu",
                   "base.dir", "export.dir", "tmp",
                   "hTol", "vTol",
                   "OutName", "LogFile",
                   "StartTime", "CodeExecutionDuration")))
##'
##' Identifying measurements that have been grouped but not merged
df.Q1 <- df.r[df.r$Qual == 1, ] 
df.Q5 <- df.r[df.r$Qual > 1, ]
##'
Gr_Rows <- strsplit(as.character(df.Q5[, "Gr"]), ";")
Grs <- sort(as.numeric(unlist(Gr_Rows)))
Grs <- as.character(unique(Grs[duplicated(Grs)])) 
##'
a.r <- df.Q5[FALSE, ]
for(Gr in Grs){# Gr="1591"
  a <- unique(df.Q5[grepl(paste0("\\b", Gr, "\\b"), Gr_Rows), ])
  if(length(unique(a$GrE)) == 1 && nrow(a) == unique(a$GrE)){
    ##'
    ##' finding replicated GNSS measurements within the tolerance
    for(r in 1:nrow(a)){ # r=1
      lbound <- a[r, "Elevation"] - (2 * vTol)
      ubound <- a[r, "Elevation"] + (2 * vTol)
      a.s <- a[a$Elevation>=lbound & a$Elevation<=ubound, ]
      lbound <- a[r, "Easting"] - (2 * hTol)
      ubound <- a[r, "Easting"] + (2 * hTol)  
      a.s <- a.s[a.s$Easting>=lbound & a.s$Easting<=ubound, ]
      lbound <- a[r, "Northing"] - (2 * hTol)
      ubound <- a[r, "Northing"] + (2 * hTol)  
      a.s <- a.s[a.s$Northing >= lbound & a.s$Northing <= ubound, ]
      if(nrow(a.s) > 1){
        a.m <- a[1, ]
        a.m[1, ] <- NA
        ##'
        ##' concatenate unique content for each variable
        VARs <- setdiff(names(a.m), c("Latitude", "Longitude", "HeightAE",
                                      "Easting", "Northing", "Elevation",
                                      "hPrec", "vPrec", "dBase",
                                      "Gr", "Qual",
                                      "deltaX", "deltaY", "deltaZ"))
        for(VAR in VARs){
          if(VAR %in% names(a.s)){
            a.m[, VAR] <- paste(unique(a.s[, VAR]), collapse = ";")
          }  
        }
        ##'
        ##' calculate the mean for coordinate variables
        VARs <- intersect(names(a.m), c("Latitude", "Longitude", "HeightAE",
                                        "Easting", "Northing", "Elevation",
                                        "hPrec", "vPrec", "dBase")) 
        for(VAR in VARs){
          if(VAR %in% names(a.s)){
            a.m[, VAR] <- mean(as.numeric(a.s[, VAR]))
          }  
        }
        rm(VARs, VAR)
        ##'
        ##' calculate differences for x, y, z coordinates
        a.m$deltaX <- max(a.s$Easting) - min(a.s$Easting)
        a.m$deltaY <- max(a.s$Northing) - min(a.s$Northing)
        a.m$deltaZ <- max(a.s$Elevation) - min(a.s$Elevation)
        a.m$deltaXYZ   <- a.m$deltaX + a.m$deltaY + a.m$deltaZ
        ##'
        ##'measurements within 2 times the GPS tolerance, Qual=2
        a.m$Qual <- 2
        if(nrow(a.s) == nrow(a)) a.r <- rbind(a.r, a.m)
      }
      if(nrow(a.s) == 1){
        for(r in 1:nrow(a)){ # r=1
          lbound <- a[r, "Elevation"] - (4 * vTol)
          ubound <- a[r, "Elevation"] + (4 * vTol)
          a.s <- a[a$Elevation >= lbound & a$Elevation <= ubound,]
          lbound <- a[r, "Easting"] - (4 * hTol)
          ubound <- a[r, "Easting"] + (4 * hTol)  
          a.s <- a.s[a.s$Easting >= lbound & a.s$Easting <= ubound,]
          lbound <- a[r, "Northing"] - (4 * hTol)
          ubound <- a[r, "Northing"] + (4 * hTol)  
          a.s <- a.s[a.s$Northing >= lbound & a.s$Northing <= ubound,]
          if(nrow(a.s) > 1){
            a.m <- a[1, ]
            a.m[1, ] <- NA
            ##'
            ##' concatenate unique content for each variable
            VARs <- setdiff(names(a.m), c("Latitude", "Longitude", "HeightAE",
                                          "Easting", "Northing", "Elevation",
                                          "hPrec", "vPrec", "dBase",
                                          "Gr", "Qual",
                                          "deltaX", "deltaY", "deltaZ"))
            for(VAR in VARs){
              if(VAR %in% names(a.s)){
                a.m[, VAR] <- paste(unique(a.s[, VAR]), collapse = ";")
              }  
            }
            ##'
            ##' calculate the mean for coordinate variables
            VARs <- intersect(names(a.m), c("Latitude", "Longitude", "HeightAE",
                                            "Easting", "Northing", "Elevation",
                                            "hPrec", "vPrec", "dBase")) 
            for(VAR in VARs){
              if(VAR %in% names(a.s)){
                a.m[, VAR] <- mean(as.numeric(a.s[, VAR]))
              }  
            }
            rm(VARs, VAR)
            ##'
            ##' calculate differences for x, y, z coordinates
            a.m$deltaX <- max(a.s$Easting) - min(a.s$Easting)
            a.m$deltaY <- max(a.s$Northing) - min(a.s$Northing)
            a.m$deltaZ <- max(a.s$Elevation) - min(a.s$Elevation)
            a.m$deltaXYZ   <- a.m$deltaX + a.m$deltaY + a.m$deltaZ
            ##'
            ##' measurements within 4 times the GPS tolerance, Qual=4
            a.m$Qual  <- 4
            if(nrow(a.s) == nrow(a)) a.r <- rbind(a.r, a.m)
          }
        }
      }
    }
  }
}
a.r <- unique(a.r)
df.Q2Q4 <- a.r[order(a.r$Date, a.r$GCP, a.r$Time), ]
row.names(df.Q2Q4) <- c(1:nrow(df.Q2Q4))
##'
##' clean work space
rm(list= setdiff(ls(),
                 c("df","fileIDs", "fileID.n",
                   "df.final", "df.r", "df.r.bu", "FILE",
                   "base.dir", "export.dir", "tmp",
                   "hTol", "vTol",
                   "OutName", "LogFile",
                   "df.Q1", "df.Q2Q4", "df.Q5",
                   "StartTime", "CodeExecutionDuration")))
##'
##' Finding single measurements now contained in averages
GrIDs_Rows.Q5 <- strsplit(as.character(df.Q5[, "GrID"]), ";")
GrIDs.Q2Q4 <- strsplit(as.character(df.Q2Q4[, "GrID"]), ";")
GrIDs.Q2Q4 <- as.character(unique(sort(as.numeric(unlist(GrIDs.Q2Q4)))))
##'
ToDel <- NULL
for(GrID in GrIDs.Q2Q4){#GrID="8"
  a <- df.Q5[grepl(paste0("\\b", GrID, "\\b"), GrIDs_Rows.Q5), ]
  ToDel <- c(ToDel,row.names(a))
}
##'
##' Deleting the identified single measurements 
ToDel <- unique(ToDel)
for(i in ToDel) df.Q5 <- df.Q5[!row.names(df.Q5) == i, ]
##'
##' Combining the quality data frames 
df.q <- rbind(df.Q1, df.Q2Q4, df.Q5)
df.q <- df.q[order(df.q$Date, df.q$GCP, df.q$Time), ]
##'
##' sort variables in the data frames
n <- union(c("GrID", "GCP", "Date", "Qual", "deltaXYZ", "hPrec", "vPrec"),
           names(df.r))
df.r <- df.r[, n]
row.names(df.r) <- c(1:nrow(df.r))
n <- union(c("GrID", "GCP", "Date", "Qual", "deltaXYZ", "hPrec", "vPrec"),
           names(df.q))
df.q <- df.q[, n]
row.names(df.q) <- c(1:nrow(df.q))
df.o <- df
##'
##' register "NA" entries as real NA values for the entire data frame
df.q[] <- lapply(df.q, function(x) {
  is.na(levels(x)) <- levels(x) == "NA"
  x
})
df.r[] <- lapply(df.r, function(x) {
  is.na(levels(x)) <- levels(x) == "NA"
  x
})
##'
##' round accuracy measures to millimeter level
VARs <- c("deltaXYZ", "hPrec", "vPrec", "deltaX", "deltaY", "deltaZ")
for(VAR in VARs) {
  df.q[, VAR] <- round(as.numeric(df.q[, VAR]), digits = 3)
  df.r[, VAR] <- round(as.numeric(df.r[, VAR]), digits = 3) 
}
##'
##' clean work space
rm(list= setdiff(ls(),
                 c("FILE","fileIDs", "fileID.n",
                   "df.o", "df.r", "df.q",
                   "base.dir", "export.dir", "tmp",
                   "hTol", "vTol",
                   "OutName", "LogFile",
                   "StartTime", "CodeExecutionDuration")))
##'
##' List of all group IDs
GrIDs <- strsplit(as.character(df.o[, "GrID"]), ";")
GrIDs <- unique(sort(as.numeric(unlist(GrIDs))))
##'
##' Finding measurements that are assigned to two groups (duplicated: d)
GrsID_Q <- unlist(strsplit(as.character(df.q[, "GrID"]), ";"))
GrsID_Q <- sort(as.numeric(GrsID_Q[!is.na(GrsID_Q)]))
GrsID_Q.d <- unique(GrsID_Q[duplicated(GrsID_Q)])
if(length(GrsID_Q.d) > 0) {
  df.d <- df.r[0 ,]
  for(GrsID in GrsID_Q.d) { # GrsID = 2
    l <- df.q[grepl(paste0("^", GrsID, ";"), df.q[, "GrID"]), ]
    df.d <- rbind(l, df.d)
    l <- df.q[grepl(paste0(";", GrsID, ";"), df.q[, "GrID"]), ]
    df.d <- rbind(l, df.d)
    l <- df.q[grepl(paste0(";", GrsID, "$"), df.q[, "GrID"]), ]
    df.d <- rbind(l, df.d)
  }
  df.d <- unique(df.d)
}
##'
##' Finding single measurements now discarded (lost: l)
Grs_Q <- unlist(strsplit(as.character(df.q[, "Gr"]), ";")) # length(Grs_Q)
Grs_Q <- sort(as.numeric(Grs_Q[!is.na(Grs_Q)]))
GrIDs_Q <- unlist(strsplit(as.character(df.q[, "GrID"]), ";")) # length(GrIDs_Q)
GrIDs_Q <- sort(as.numeric(GrIDs_Q[!is.na(Grs_Q)]))
GrIDs_Q.l <- setdiff(GrIDs, unique(GrIDs_Q))
df.l <- df.o[df.o$GrID %in% GrIDs_Q.l, ]
if(length(GrIDs_Q.l) > 0) {
  df.l$Qual <- 5
  df.l[, c("deltaXYZ", "deltaX", "deltaY", "deltaZ", "Gr", "GrE")] <- NA
  df.l.names <- intersect(names(df.q), names(df.l))
  df.l <- df.l[, df.l.names]
  ##'
  ##' adding the group they have been assigned
  Grs <- strsplit(as.character(df.q[,"Gr"]),";")
  for(GrID in df.l$GrID) {
    l <- grepl(paste0("\\b", GrID, "\\b"), Grs)
    df.l[df.l$GrID == GrID, c("Gr", "GrE")] <- unique(df.q[l, c("Gr", "GrE")])
  }
  df.l <- df.l[order(df.l$Gr, df.l$Date, df.l$GCP, df.l$Time), ]
}
##'
##' clean work space
rm(list= setdiff(ls(),
                 c("FILE", "fileIDs", "fileID.n", 
                   "df.o", "df.r", "df.q", "df.l", "df.d",
                   "base.dir", "export.dir", "tmp",
                   "hTol", "vTol",
                   "OutName", "LogFile",
                   "StartTime", "CodeExecutionDuration")))
##'
##' start log entry
setwd(tmp)
cat(paste0("-> original number of measurements: ", nrow(df.o), "\n",
           "-> new number of measurements: ", nrow(df.q), "\n\n"
           ),
    ##'
    ##' log entry for Q1 points
    paste0("-> points within the defined tolerance (Q1): ",
           nrow(df.q[df.q$Qual == 1, ]), "\n"
           ),
    if(nrow(df.q[df.q$Qual == 1, ]) > 0) {
      paste0("- Q1, max deltaX: ",
             max(as.numeric(df.q[df.q$Qual == 1, ]$deltaX), na.rm = T), "\n",
             "- Q1, max deltaY: ",
             max(as.numeric(df.q[df.q$Qual == 1, ]$deltaY), na.rm = T), "\n",
             "- Q1, max deltaZ: ",
             max(as.numeric(df.q[df.q$Qual == 1, ]$deltaZ), na.rm = T), "\n",
             "\n"
             )
      },
    ##'
    ##' log entry for Q2 points
    paste0("-> points within 2x the defined tolerance (Q2): ",
           nrow(df.q[df.q$Qual == 2, ]), "\n"
           ),
    if(nrow(df.q[df.q$Qual == 2, ]) > 0) {
      paste0("- Q2, max deltaX: ",
             max(as.numeric(df.q[df.q$Qual == 2, ]$deltaX), na.rm = T),"\n",
             "- Q2, max deltaY: ",
             max(as.numeric(df.q[df.q$Qual == 2, ]$deltaY), na.rm = T),"\n",
             "- Q2, max deltaZ: ",
             max(as.numeric(df.q[df.q$Qual == 2, ]$deltaZ), na.rm = T), "\n"
             )
      }, "\n",
    ##'
    ##' log entry for Q4 points
    paste0("-> points within 4x the defined tolerance (Q4): ",
           nrow(df.q[df.q$Qual == 4, ]), "\n"
           ),
    if(nrow(df.q[df.q$Qual == 4, ]) > 0) {
      paste0("- Q4, max deltaX: ",
             max(as.numeric(df.q[df.q$Qual == 4, ]$deltaX), na.rm = T), "\n",
             "- Q4, max deltaY: ",
             max(as.numeric(df.q[df.q$Qual == 4, ]$deltaY), na.rm = T), "\n",
             "- Q4, max deltaZ: ",
             max(as.numeric(df.q[df.q$Qual == 4, ]$deltaZ), na.rm = T), "\n"
             )
      }, "\n",
    ##'
    ##' log entry for Q5 points
    paste0("-> points within 5x the defined tolerance (not merged, Q5): ",
           nrow(df.q[df.q$Qual == 5, ]), "\n"
           ),
    if(nrow(df.q[df.q$Qual == 5, ]) > 0) {
      paste0("- Q5, max deltaX: ",
             max(as.numeric(df.q[df.q$Qual == 5, ]$deltaX), na.rm = T),"\n",
             "- Q5, max deltaY: ",
             max(as.numeric(df.q[df.q$Qual == 5, ]$deltaY), na.rm = T),"\n",
             "- Q5, max deltaZ: ",
             max(as.numeric(df.q[df.q$Qual == 5, ]$deltaZ), na.rm = T),"\n"
             )
    }, "\n",
    append = TRUE, sep = "", file = LogFile)
setwd(base.dir)
##'
##' log entry for points that occur in two measurement groups
setwd(tmp) 
if(exists("df.d")){
  cat("-> points that occur in two measurement groups:\n",
      append = TRUE, file = LogFile)
  for(r in 1:length(df.d)) {
    cat(if(!is.na(df.d[r, "GCP"])){
      paste0("   ", df.d[r, "GCP"], "; ", df.d[r, "GrID"], "; ",
             df.d[r, "Date"], "\n",
             "- Group <", df.d[r, "Gr"], ">\n"
             )
    }, append = TRUE, file = LogFile)
  }; rm(r)
}
setwd(base.dir)
##'
##'  log entry for points that have been discarded
setwd(tmp)  
if(exists("df.l")){
  cat("-> points that have been discarded:\n\n",
      append = TRUE, file = LogFile)
  for(r in 1:length(df.l)) {
    cat(if(!is.na(df.l[r, "GCP"])){
      paste0("   ", df.l[r, "GCP"], "; ", df.l[r, "GrID"], "; ",
             df.l[r, "Date"], "\n",
             "-> Group:\n<", df.l[r, "Gr"], ">\n\n"
             )
      }, append = TRUE, file = LogFile)
  }; rm(r)
}
setwd(base.dir)
##' end log entry 
##'
##' write data to tab delimited text file and save data in R data format
if(exists("FILE")){
  OUTID <- paste0(strsplit(basename(FILE), split="\\.")[[1]][-2], "_merged.txt")
  setwd(export.dir)
  write.table(df.q, file = OUTID, row.names = FALSE, quote = FALSE, sep = "\t")
  if(exists("df.q") && exists("df.d") && exists("df.l")){
    save(df.q, df.l, df.d, file = paste0(sub(".txt", "", OUTID), ".rda"))
  }
  if(exists("df.q") && !exists("df.d") && exists("df.l")){
    save(df.q, df.l, file = paste0(sub(".txt", "", OUTID), ".rda"))
  }
  if(exists("df.q") && exists("df.d") && !exists("df.l")){
    save(df.q, df.d, file = paste0(sub(".txt", "", OUTID), ".rda"))
  }
  if(exists("df.q") && !exists("df.d") && !exists("df.l")){
    save(df.q, file = paste0(sub(".txt", "", OUTID), ".rda"))
  }
} 
##' start log entry
setwd(tmp)
cat(paste0("\n-> finished processing i.e., ",
           "replicated measurements replaced by their mean.\n",
           "   ", Sys.time(), "\n",
           if(!is.null(warnings())){
             paste0(" -> warnings: ", names(warnings()), "\n")
           }
       ),
   append = TRUE, file = LogFile)
setwd(base.dir)
##' end log entry 
### END OF DATA PROCESSING #####################################################

