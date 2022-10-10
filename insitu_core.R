#insitu_core.R
#author: Paul Zhu    GitHub: PeashooterBiu
#last edited: 6/20/2022
#plot the chosen variable against depth in a single insitu file



library(ggplot2)
library(rLakeAnalyzer)
library(hrbrthemes)
library(svDialogs)
library(readxl)


# users choose the variable plotted against depth
input_number <- ( dlgInput("Enter a number to choose a variable to be plotted against depth:
                          1. T_C   2. DO_ppm   3.DO_sat   4.Turb_NTU   5.AcCond_uScm
                          6. SpCond_uScm    7.Sal_psu   8.TDS_ppt   9.Dens_gcm3   
                          10.Chla_RFU", Sys.info()["user"])$res )

column_number <- as.numeric(input_number)
if (column_number > 10) {print("Invalid Number. Only Enter Integer 1-10")
                         break}
  

# users choose file directory interactively
file_path <- file.choose(new = FALSE)
filename <- basename(file_path)


# get the site name from file name
sitename <- strsplit(strsplit(filename, "_")[[1]][4], "-")[[1]]


# get date from file name
string <- strsplit(strsplit(filename, "_")[[1]][2], "-")[[1]]
date <- as.character(paste(string[1], string[2], string[3], sep =''))


# Read in the data and clean up the column names
dat <- as.data.frame(read_excel(file_path, skip = 19))


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


#finding the index where max in depth occurs
max_depth_index <- which.max(Z_m)


#assign different number for upcast and downcast data to the "group column"
part <- rep("downcast", length(Z_m))
part[1:max_depth_index] <- "downcast"                  #downcast 
part[(max_depth_index+1):length(Z_m)] <- "upcast"      #upcast


# Recombine all the data into new data frames
dat_raw <- as.data.frame(cbind(T_C,DO_ppm,DO_sat,Turb_NTU,AcCond_uScm,
              SpCond_uScm,Sal_psu,TDS_ppt,Dens_gcm3,Chla_RFU,Lat,Lon,Z_m))


# retain only the downcast data; remove all the upcast data
dat_downcast <- as.data.frame(dat_raw[2:max_depth_index,])
dat_uppcast  <- as.data.frame(dat_raw[max_depth_index:length(Z_m),])


# Depth columns
depth1 <- dat_downcast$Z_m
depth2 <- dat_raw$Z_m


# create lists of column names
column_names <- c("Temperature (C)","RDO (mg/L)", "RDO (%sat)", "Turbidity (NTU)", "Actual Conductivity", "Specific Conductivity", "Salinity (PSU)", "Total Dissolved Solid (TSU)", "Density (g/cm3)", "Chlorophyll-a Fluorescence (RFU)")
column_names_2 <- c("Temperature","RDO Concentration", "RDO Saturation", "Turbidity", "Actual Conductivity", "Specific Conductivity", "Salinity", "Total Dissolved Solid", "Density", "Chlorophyll-a Fluorescence")


# get x axes vectors used for plots
x_axis_1 <- unlist(dat_downcast[column_number])
x_axis_2 <- unlist(dat_raw[column_number])
  

# plot for trend
plot1 <- ggplot(dat_downcast, mapping = aes(x = x_axis_1, y = depth1, color = x_axis_1))+
  geom_point(show.legend = FALSE) + 
  labs ( #to label each axis
    x = column_names[column_number],
    y = "Depth (m)",
    title = paste(sitename, column_names_2[column_number], date, sep = "  ")
  )
plot1 + scale_y_reverse() + xlim(c(0,max(x_axis_1))) + theme_bw()


readline("press enter to continue")


# plot for checking the matching between upcast and downcast
plot2 <- ggplot(dat_raw, mapping = aes(x = x_axis_2, y = depth2))+
      geom_point(aes(col=part), size = 1) + 
      labs ( #to label each axis
           x = column_names[column_number],
           y = "Depth (m)",
           title = paste(sitename, column_names_2[column_number], date, "Up/downcast Check", sep = "  ")
           )
plot2 + scale_y_reverse() + xlim(c(0,max(x_axis_1))) + scale_colour_brewer(palette = "Set1") +
      theme_bw() + labs(color = "parts") #labs change legend title
        