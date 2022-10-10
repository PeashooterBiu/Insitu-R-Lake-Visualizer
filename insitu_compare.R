#insitu_compare.R
#author: Paul Zhu    GitHub: PeashooterBiu
#last edited: 7/5/2022
#compare chosen variable against depth in multiple insitu files



library(stringr)
library(dplyr)
library(rstudioapi)
library(janitor)
library(lubridate)
library(writexl)
library(readxl)
library(ggplot2)
library(utils)
library(ggplot2)
library(hrbrthemes)
library(svDialogs)

# users choose the variable plotted against depth
input_number <- ( dlgInput("Enter a number to choose a variable to be plotted against depth:
                          1. T_C   2. DO_ppm   3.DO_sat   4.Turb_NTU   5.AcCond_uScm
                          6. SpCond_uScm    7.Sal_psu   8.TDS_ppt   9.Dens_gcm3   
                          10.Chla_RFU", Sys.info()["user"])$res )

column_number <- as.numeric(input_number)

# Select folder with data files to be processed 
datapath <- selectDirectory(caption = "Select Data Files Directory")

# Get list of files
filenames <- dir(datapath)

# Get site and year from the first file name
filename1 = filenames[1]
year <- as.character(strsplit(strsplit(filename1, "_")[[1]][2], "-")[[1]][1])
site <- as.character(strsplit(strsplit(filename1, "_")[[1]][4], "-")[[1]])
year_day0 <- as.character(strsplit(filename1, "_")[[1]][2])

# outfile is an empty string
outfile <- ""


# loop through file names
for (filename in filenames){
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
  Turb_NTU <- rep(0, each = length(Z_m))
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
  outfile <- (as.data.frame(rbind(outfile, dat1)))}


outfile <- outfile[-1,]

# create lists of column names
column_names <- c("Temperature (C)","RDO (mg/L)", "RDO (%sat)", "Turbidity (NTU)", "Actual Conductivity", "Specific Conductivity", "Salinity (PSU)", "Total Dissolved Solid (TSU)", "Density (g/cm3)", "Chlorophyll-a Fluorescence (RFU)")
column_names_2 <- c("Temperature","RDO Concentration", "RDO Saturation", "Turbidity", "Actual Conductivity", "Specific Conductivity", "Salinity", "Total Dissolved Solid", "Density", "Chlorophyll-a Fluorescence")


# get x axes vectors used for plots
x_axis <- as.numeric((outfile[column_number + 4])[[1]])
y_axis <- as.numeric(outfile$Z_m)
date <- (outfile$Date)

# title string
title_str = paste(site, paste(year_day0, year_day, sep = " to "), column_names_2[column_number], "Comparison", sep = "  ")

# change line colors
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "")

# plot for trend
plot1 <- ggplot(outfile, mapping = aes(x = x_axis, y = y_axis, color = date)) + 
      geom_point(size = 1.5) +
      labs ( #to label each axis
          x = column_names[column_number],
          y = "Depth (m)",
          title = title_str) +
      guides(colour = guide_legend(override.aes = list(size=8)))
      
plot1 + scale_y_reverse() + theme_bw() + xlim(c(0,max(x_axis))) + scale_colour_manual(values = c("#000000", "#56B4E9", "#009E73", "#FF0000", "#0072B2", "#8B4513", "#CC79A7", "#00FA9A", "#00FFFF", "#FFFF00", "#808080", "#808000", "#228B22", "#E69F00"))

