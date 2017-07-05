#' Tools to downscale ERA data basing on climate station measurements
#'
#' Put description here
#'
#' \tabular{ll}{ Package: \tab Luminescence\cr Type: \tab Package\cr Version:
#' \tab 0.1 \cr Date: \tab 2017-07-05 \cr License: \tab GPL-3\cr }
#'
#' @name Climate-reanalysis-downscaling
#' @docType package
#' @author \bold{Full list of authors and contributors} (alphabetic order)
#'
#' \tabular{ll}{
#' Lars Gerlitz \tab GFZ Potsdam, Germany \cr
#' David Loibl \tab Humboldt University, Germany \cr
#' Anika Pinzer \tab Uppsala University, Sweden \cr
#' }
#'
#' \bold{Acknowledgement}
#'
#' GFZ Potsdam; Geo.X / MORSANAT project
#'
#' @references to be prepared
#'
NULL


# Read configuration from file (full path + filename - EDIT MANUALLY)
source("/home/loibldav/Git/downscaling/maidan-config.R")

# Load packages
library(sp)
library(raster)
library(ncdf4)

setwd(station_PATH)

SpatialPoints(cbind(x_coordinate,y_coordinate)) 

##############################################################################################################################
# Functions
##############################################################################################################################

# Preprocessing of ERA data
preprocess_ERA <- function(ERA_PATH, ERA_FILE, x_coordinate, y_coordinate, column_title) {
  
  # Read ERA data
  ERA_data<-brick(paste(ERA_PATH,ERA_FILE,sep=""))
  
  # Extract ERA data for station location
  ERA_data_ext <- extract(ERA_data,SpatialPoints(cbind(x_coordinate,y_coordinate)))
  rm(ERA_data)
  
  # Extract date
  y <- as.numeric(substr(colnames(ERA_data_ext),2,5))
  m <- as.numeric(substr(colnames(ERA_data_ext),7,8))
  d <- as.numeric(substr(colnames(ERA_data_ext),10,11))
  h <- as.numeric(substr(colnames(ERA_data_ext),13,14))
  # Correct ERA time offset during summer month (0 > 1, 6 > 7, 12 > 13, 18 > 19)
  h <- round(h/6)*6
  
  # Construct new table for input data of Spatial Point
  result <- data.frame(y, m, d, h, ERA_data_ext[1,])
  names(result)[5] <- column_title                     
  return(result)
}


# Data normalization
normalize_data <- function(input_data) {
  data_norm <- input_data
  for(c in 8:13) 
    data_norm[,c] = ((input_data[,c] - mean(input_data[,c])) / sd(input_data[,c]))
  
  return(data_norm)
}


# Normalize Hindcast data
normalize_HC <- function(input_data_H, input_data) {
  data_norm_HC <- input_data_H
  data_norm_HC <- data_norm_HC[,-(1:5)]
  input_data<-input_data[,-(1:7)]
  
  for(c in 1:ncol(input_data)) 
    data_norm_HC[,c]=((data_norm_HC[,c] - mean(input_data[,c])) / sd(input_data[,c]))
  
  return(data_norm_HC)
}


# Crossvalidation
crossvalidate <- function(input_data) {
  data_cv <- input_data
  cv_results <- vector()
  
  for(i in 1:nrow(data_cv)) {
    data_cv <- input_data[-i,]
    rg_cv <- lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=data_cv)
    results <- predict(rg_cv, newdata=input_data[i,])
    cv_results <- append(cv_results, results)
  }
  
  # Add cv model results to table
  data_cv <- cbind(input_data, cv_results)
  colnames(data_cv)[ncol(data_cv)] <- "cv"
  
  return(data_cv)
}


# Plotting functions
plot_Tdiff <- function(data_norm, regr, graphtitle, color) {
  date<- ISOdate(data_norm$y, data_norm$m, data_norm$d, data_norm$h)
  plot(date, data_norm$Tdiff, pch=16, cex=0.5, main=graphtitle, xlab="date", ylab="Tdiff [K]", ylim=c(-20,30))
  points(date, regr$fitted.values, col=color, pch=16, cex=0.5)  
}


plot_CV <- function(input_data, rg, cv_results, cor, RSS, graphtitle, color) {
  date<- ISOdate(input_data$y, input_data$m, input_data$d, input_data$h)
  plot(date, cv_results, pch=16, cex=0.5, main=graphtitle, xlab="Date", ylab="Tdiff [K]", col=color, ylim=c(-20,30))
  points(date, input_data$Tdiff, pch=16, cex=0.5)
  legend("topright", legend=c(paste("cor =",cor), paste("RSS =" ,RSS)), bty='n', horiz=FALSE, cex=0.7)
  legend("bottomleft", legend =c(paste("C: GPH= ", round(rg$coefficients[1], digits=2)), 
                                 paste("TCWV= ", round(rg$coefficients[2], digits=2)), 
                                 paste("UCW= ", round(rg$coefficients[3], digits=2)), 
                                 paste("VCW= ", round(rg$coefficients[4], digits=2)),
                                 paste("MSLP= ", round(rg$coefficients[5], digits=2)), 
                                 paste("TCC= ", round(rg$coefficients[6], digits=2))), horiz=TRUE, bty='n', cex=0.7)
}


plot_HC <- function(input_data, norm_input_data, station_data, graphtitle, legendtext, color1, color2, color3) {
  date <- ISOdate(input_data$y, input_data$m, input_data$d, input_data$h)
  plot(date, input_data$T_ERA, pch=16, cex=0.5, main=graphtitle, xlab="date", ylab="T [K]", col=color1, ylim=c(-20,40))
  points(date, station_data, pch=16, cex=0.5, col=color2)
  date <- ISOdate(norm_input_data$y, norm_input_data$m, norm_input_data$d, norm_input_data$h)
  points(date, norm_input_data$T_station, pch=16, cex=0.5, col=color3)
  legend("topright", legend=c(legendtext), 
         lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c(color1, color2, color3),bty='n', horiz=FALSE, cex=0.7)
}



##############################################################################################################################
# Process station data
# Data available at: http://sdss.caiag.kg/sdss/
##############################################################################################################################

temperature <- read.table(station_data_file, sep=",", dec=".", header=T)  

# Convert date to numeric format & create new data frame
year <- as.numeric(substr(temperature[,1],1,4))
month <- as.numeric(substr(temperature[,1],6,7))
day <- as.numeric(substr(temperature[,1],9,10))
hour <- as.numeric(substr(temperature[,1],12,13))
temp_obs <- data.frame(y=year, m=month, d=day, h=hour, T_station=temperature$Wert)

# Extract relevant times of day (0, 6, 12, 18) -> 0=0-1am, 6=6-7am, 12=12-1pm, 18=6-7pm 
temp_obs <- subset(temp_obs, h==0|h==6|h==12|h==18)

# Calculat mean values for respective time steps
temp_obs <- aggregate(temp_obs, by=list(temp_obs$y, temp_obs$m, temp_obs$d, temp_obs$h), FUN=mean, na.rm=T)

# Define date 
date <- ISOdate(temp_obs$y, temp_obs$m, temp_obs$d, temp_obs$h)

# Order table by date
temp_obs <- temp_obs[order(date),]

# Delete unnecessary groups
temp_obs <- temp_obs[,-(1:4)]



##############################################################################################################################
# Process ERA Interim data
##############################################################################################################################

ERA_T_ext <- preprocess_ERA(ERA_PATH, ERA_T2m_file, x_coordinate, y_coordinate, "T_ERA")
ERA_GPH_ext <- preprocess_ERA(ERA_PATH, ERA_GPH_file, x_coordinate, y_coordinate, "GPH")
ERA_TCWV_ext <- preprocess_ERA(ERA_PATH, ERA_TCWV_file, x_coordinate, y_coordinate, "TCWV")
ERA_UCW_ext <- preprocess_ERA(ERA_PATH, ERA_UCW_file, x_coordinate, y_coordinate, "UCW")
ERA_VCW_ext <- preprocess_ERA(ERA_PATH, ERA_VCW_file, x_coordinate, y_coordinate, "VCW")
ERA_MSLP_ext <- preprocess_ERA(ERA_PATH, ERA_MSLP_file, x_coordinate, y_coordinate, "MSLP")
ERA_TCC_ext <- preprocess_ERA(ERA_PATH, ERA_TCC_file, x_coordinate, y_coordinate, "TCC")

# Merge tables
data_all<-merge(ERA_T_ext, temp_obs, by=c("y", "m", "d", "h"), sort=F)
data_all<-merge(data_all, ERA_GPH_ext, by=c("y", "m", "d", "h"), sort=F)
data_all<-merge(data_all, ERA_TCWV_ext, by=c("y", "m", "d", "h"), sort=F)
data_all<-merge(data_all, ERA_UCW_ext, by=c("y", "m", "d", "h"), sort=F)
data_all<-merge(data_all, ERA_VCW_ext, by=c("y", "m", "d", "h"), sort=F)
data_all<-merge(data_all, ERA_MSLP_ext, by=c("y", "m", "d", "h"), sort=F)
data_all<-merge(data_all, ERA_TCC_ext, by=c("y", "m", "d", "h"), sort=F)

# Calculate difference T_ERA/T_station
data_all$Tdiff <- data_all$T_ERA - data_all$T_station

# Order table
data_all <- cbind(y=data_all$y, m=data_all$m, d=data_all$d, h=data_all$h, T_station=data_all$T_station, T_ERA=data_all$T_ERA, Tdiff=data_all$Tdiff, GPH=data_all$GPH, TCWV=data_all$TCWV, UCW=data_all$UCW, VCW=data_all$VCW, MSLP=data_all$MSLP, TCC=data_all$TCC, VIWV=data_all$VIWV)

write.table(data_all, "downscaling_Temp_data_all.txt", col.names=T, row.names=F, sep="\t", quote=F)

# Create 4 new tables according to 4 timeslots
df_data_all <- as.data.frame(data_all)

data_all_0<-subset(df_data_all, h==0)
data_all_6<-subset(df_data_all, h==6)
data_all_12<-subset(df_data_all, h==12)
data_all_18<-subset(df_data_all, h==18)



##############################################################################################################################
# Calculate correlation, normalization and regression
##############################################################################################################################

# Correlation
cor(data_all)
write.table(cor(data_all), "correlation_data_all.txt", col.names=T, row.names=F, sep="\t", quote=F)


# Normalize data
data_all_norm_0 <- normalize_data(data_all_0)
data_all_norm_6 <- normalize_data(data_all_6)
data_all_norm_12 <- normalize_data(data_all_12)
data_all_norm_18 <- normalize_data(data_all_18)
df_data_all_norm <- normalize_data(df_data_all)

# Calculate Regression
rg0 <- lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=data_all_norm_0)
plot_Tdiff(data_all_norm_0, rg0, paste(name_station, "- RG 0h", time_difference), "red")

rg6 <- lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=data_all_norm_6)
plot_Tdiff(data_all_norm_6, rg6, paste(name_station, "- RG 6h", time_difference), "purple")

rg12 <- lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=data_all_norm_12)
plot_Tdiff(data_all_norm_12, rg12, paste(name_station, "- RG 12h", time_difference), "green")

rg18 <- lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=data_all_norm_18)
plot_Tdiff(data_all_norm_18, rg18, paste(name_station, "- RG 18h", time_difference), "blue")

rg <- lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=df_data_all_norm)
plot_Tdiff(df_data_all_norm, rg, paste(name_station, "- RG data_all", time_difference), "steelblue")



##############################################################################################################################
# Validation
##############################################################################################################################

#Crossvalidation 
data_all_norm_0_cv <- crossvalidate(data_all_norm_0)
data_all_norm_6_cv <- crossvalidate(data_all_norm_6)
data_all_norm_12_cv <- crossvalidate(data_all_norm_12)
data_all_norm_18_cv <- crossvalidate(data_all_norm_18)

# Correlation 
cor_0h <- cor(data_all_norm_0_cv$Tdiff, data_all_norm_0_cv$cv)
cor_6h <- cor(data_all_norm_6_cv$Tdiff, data_all_norm_6_cv$cv)
cor_12h <- cor(data_all_norm_12_cv$Tdiff, data_all_norm_12_cv$cv)
cor_18h <- cor(data_all_norm_18_cv$Tdiff, data_all_norm_18_cv$cv)

#Residual sum of squares
RSS_0h <- sum((data_all_norm_0_cv$Tdiff-data_all_norm_0_cv$cv)^2)
RSS_0h <- sqrt(RSS_0h/(nrow(data_all_norm_0_cv)))

RSS_6h <- sum((data_all_norm_6_cv$Tdiff-data_all_norm_6_cv$cv)^2)
RSS_6h <- sqrt(RSS_6h/(nrow(data_all_norm_6_cv)))

RSS_12h <- sum((data_all_norm_12_cv$Tdiff-data_all_norm_12_cv$cv)^2)
RSS_12h <- sqrt(RSS_12h/(nrow(data_all_norm_12_cv)))

RSS_18h <- sum((data_all_norm_18_cv$Tdiff-data_all_norm_18_cv$cv)^2)
RSS_18h <- sqrt(RSS_18h/(nrow(data_all_norm_18_cv)))


# Create plots
plot_CV(data_all_norm_0, rg0, data_all_norm_0_cv$cv, cor_0h, RSS_0h, paste(name_station, "- CV 0h", time_difference), "violetred3")
plot_CV(data_all_norm_6, rg6, data_all_norm_6_cv$cv, cor_6h, RSS_6h, paste(name_station, "- CV 6h", time_difference), "darkcyan")
plot_CV(data_all_norm_12, rg12, data_all_norm_12_cv$cv, cor_12h, RSS_12h, paste(name_station, "- CV 12h", time_difference), "olivedrab3")
plot_CV(data_all_norm_18, rg18, data_all_norm_18_cv$cv, cor_18h, RSS_18h, paste(name_station, "- CV 18h", time_difference), "darkorange2")


# Append cv model results for all time steps to table
data_all_norm_all_cv<- rbind(data_all_norm_0_cv, data_all_norm_6_cv, data_all_norm_12_cv, data_all_norm_18_cv)
data_all_norm_all_cv<- data_all_norm_all_cv[order(ISOdate(data_all_norm_all_cv$y, data_all_norm_all_cv$m, data_all_norm_all_cv$d, data_all_norm_all_cv$h)),]



##############################################################################################################################
# Hindcast
##############################################################################################################################

data_all_H<-merge(ERA_T_ext, ERA_GPH_ext, by=c("y", "m", "d", "h"), sort=F)
data_all_H<-merge(data_all_H, ERA_TCWV_ext, by=c("y", "m", "d", "h"), sort=F)
data_all_H<-merge(data_all_H, ERA_UCW_ext, by=c("y", "m", "d", "h"), sort=F)
data_all_H<-merge(data_all_H, ERA_VCW_ext, by=c("y", "m", "d", "h"), sort=F)
data_all_H<-merge(data_all_H, ERA_MSLP_ext, by=c("y", "m", "d", "h"), sort=F)
data_all_H<-merge(data_all_H, ERA_TCC_ext, by=c("y", "m", "d", "h"), sort=F)

head(data_all_H)


# Create 4 new tables according to 4 timeslots, all_variables
data_all_H_0h<-subset(data_all_H, h==0)
data_all_H_6h<-subset(data_all_H, h==6)
data_all_H_12h<-subset(data_all_H, h==12)
data_all_H_18h<-subset(data_all_H, h==18)

###tail(data_all_H_18h)

# Normalize hindcast data
data_all_norm_H_0h <- normalize_HC(data_all_H_0h, data_all_0)
data_all_norm_H_6h <- normalize_HC(data_all_H_6h, data_all_6)
data_all_norm_H_12h <- normalize_HC(data_all_H_12h, data_all_12)
data_all_norm_H_18h <- normalize_HC(data_all_H_18h, data_all_18)

###data_all_norm_H_0h_date<- data_all_norm_H_0h[,1:4]
###data_all_norm_H_6h_date<- data_all_norm_H_6h[,1:4]
###data_all_norm_H_12h_date<- data_all_norm_H_12h[,1:4]
###data_all_norm_H_18h_date<- data_all_norm_H_18h[,1:4]


# Predict past temperatures on basis of ERA-Interim temperatures
results_0h_H <- predict(rg0, newdata=data_all_norm_H_0h) # only predictors
results_6h_H <- predict(rg6, newdata=data_all_norm_H_6h) # only predictors
results_12h_H <- predict(rg12, newdata=data_all_norm_H_12h) # only predictors
results_18h_H <- predict(rg18, newdata=data_all_norm_H_18h) # only predictors

# Add temperature difference to T_ERA in order to get T_Station_H_18h
T_Station_H_0h <- (data_all_H_0h$T_ERA - results_0h_H)
T_Station_H_6h <- (data_all_H_6h$T_ERA - results_6h_H)
T_Station_H_12h <- (data_all_H_12h$T_ERA - results_12h_H)
T_Station_H_18h <- (data_all_H_18h$T_ERA - results_18h_H)

###head(data_all_H_0h)
###head(T_Station_H_0h)
###head(results_0h_H)

### table: date (date+T_Station)
data.frame(data_all_norm_H_0h_date, T_Station_H_0h)
data.frame(data_all_norm_H_6h_date, T_Station_H_6h)
data.frame(data_all_norm_H_12h_date, T_Station_H_12h)
data.frame(data_all_norm_H_18h_date, T_Station_H_18h)

# Create hindcast plots
plot_HC(data_all_H_0h, data_all_norm_0_cv, T_Station_H_0h, paste(name_station, "- Temperatures Hindcast 0h", time_difference), c(paste("T_ERA"), paste("T_Station_H"), paste("T_Station")), "black", "darkolivegreen", "brown") 
plot_HC(data_all_H_6h, data_all_norm_6_cv, T_Station_H_6h, paste(name_station, "- Temperatures Hindcast 6h", time_difference), c(paste("T_ERA"), paste("T_Station_H"), paste("T_Station")), "black", "darkolivegreen", "brown") 
plot_HC(data_all_H_12h, data_all_norm_12_cv, T_Station_H_12h, paste(name_station, "- Temperatures Hindcast 12h", time_difference), c(paste("T_ERA"), paste("T_Station_H"), paste("T_Station")), "black", "darkolivegreen", "brown") 
plot_HC(data_all_H_18h, data_all_norm_18_cv, T_Station_H_18h, paste(name_station, "- Temperatures Hindcast 18h", time_difference), c(paste("T_ERA"), paste("T_Station_H"), paste("T_Station")), "black", "darkolivegreen", "brown") 


#table including temperatures of hindcast for all time steps
H_data_all<- rbind(T_Station_H_0h, T_Station_H_6h, T_Station_H_12h, T_Station_H_18h)
date<-ISOdate(data_all_norm_H_0h$y, data_all_norm_H_0h$m, data_all_norm_H_0h$d, data_all_norm_H_0h$h)
H_data_all<- H_data_all[order(date),]

write.table(H_data_all, "H_downscaling_Temp_data_all.txt", col.names=T, row.names=F, sep="\t", quote=F)
