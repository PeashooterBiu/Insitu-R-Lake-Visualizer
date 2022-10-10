#insitu_combine.R
#author: Paul Zhu    GitHub: PeashooterBiu
#last edited: 7/25/2022


#this script combines multiple insitu files into a single one
#the script trims the data, only retaining downcast data
#the output file has the date column in year-month-day format



library(stringr)
library(dplyr)
library(rstudioapi)
library(janitor)
library(lubridate)
library(writexl)
library(readxl)
library(ggplot2)
library(utils)

# a Boolean to determine whether the outfile will be exported
OutBoolean <- TRUE

# Select folder with data files to be processed 
datapath <- selectDirectory(caption = "Select Data Files Directory")

# file output (this can be customized)
outpath <- paste(selectDirectory(caption = "Select Output Directory"), "/", sep = "")

# Get list of files
filenames <- dir(datapath)

# Get site and year from the first file name
filename1 = filenames[1]
year <- as.character(strsplit(strsplit(filename1, "_")[[1]][2], "-")[[1]][1])
site <- as.character(strsplit(strsplit(filename1, "_")[[1]][4], "-")[[1]])

# Create output file
basefile <- paste(site,year,sep="_")
outfile <- ""
outfilename <- paste(outpath,paste(basefile,"_InSitu.xlsx",sep = ""),sep = "")

# loop through file names
for (filename in filenames){
  # check if all the file names have the same sites and years
  year_ <- as.character(strsplit(strsplit(filename, "_")[[1]][2], "-")[[1]][1])
  site_ <- as.character(strsplit(strsplit(filename, "_")[[1]][4], "-")[[1]])
  if (year_ != year | site_ != site ) {
    print("ERROR: Sites or Years Not Consistent. Please Check Files")
    OutBoolean <- FALSE
    winDialog("ok","ERROR: Sites or Years Not Consistent. Please Check Files")
    }
  
  # use concatenation to create the directory of each file
  fullpath <- paste(datapath,filename,sep = "/")
  
  # Read in the data, skipping the header
  dat <- as.data.frame(read_excel(fullpath, skip = 19))

  # The serial numbers (specific to each probe) are part of the column names.
  # To combine files from both probes, we need to rename them.
  Z_m <- dat[,grepl( "Depth" , names( dat ) ) ]
  Lat <- dat[,grepl( "Latitude" , names( dat ) ) ]
  Lon <- dat[,grepl( "Latitude" , names( dat ) ) ]
  Tc <-  dat[,grepl( "Temperature" , names( dat ) )]
  T_C <- Tc[,grepl("754", names(Tc))]
  DO_ppm <-  dat[,grepl( "RDO.C" , names( dat ) ) ]
  DO_sat <-  dat[,grepl( "RDO.S" , names( dat ) ) ]
  Turb_NTU <- dat[,grepl( "Turbidity" , names( dat ) ) ]
  #TSS_ppm <- dat[,grepl( "Suspended" , names( dat ) ) ]
  AcCond_uScm <- dat[,grepl( "Actual" , names( dat ) ) ]
  SpCond_uScm <- dat[,grepl( "Specific" , names( dat ) ) ]
  Sal_psu <- dat[,grepl( "Salinity" , names( dat ) ) ]
  TDS_ppt <- dat[,grepl( "Dissolved" , names( dat ) ) ]
  Dens_gcm3 <- dat[,grepl( "Density" , names( dat ) ) ]
  Chla_RFU <- dat[,grepl( "Fluorescence" , names( dat ) ) ]
  #Chla_ugL <- dat[,grepl( "a.Conc" , names( dat ) ) ]
  
  # Obtain year_day from file name
  year_day <- as.character(strsplit(filename, "_")[[1]][2])
  
  # Add a Year Day column for compatibility across platforms (######)
  Date <- rep(year_day, each = length(Z_m))
  
  # Recombine into new data frames
  dat1 <- as.data.frame(cbind(Date,Z_m,Lat,Lon,T_C,DO_ppm,DO_sat,Turb_NTU,AcCond_uScm,
                SpCond_uScm,Sal_psu,TDS_ppt,Dens_gcm3,Chla_RFU))
  
  # Trim data so only downcast is used
  maxdepi <-which.max(Z_m)
  dat1 <- dat1[2:maxdepi,]
  # Bind the data frames to create out file
  outfile <- (as.data.frame(rbind(outfile, dat1)))  #remove the first empty line using index
  } 

outfile1 <- outfile[-1,]

# write the dataframe as .xlsx file only if OutBoolean is TRUE
rownames(outfile1) <- NULL
if (OutBoolean){
  write_xlsx(outfile1, path = outfilename)}


