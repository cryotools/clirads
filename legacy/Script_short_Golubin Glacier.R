#############################################################################################################################
#########################    Regional Research Network "Water in Central Asia" (CAWa)      ##################################
#############################################################################################################################


setwd("Z:/PRAKT/Anika_Pinzner/Stationsdaten/Golubin glacier")

#Defining the coordinates of the respective field station
x_coordinate <- 74.29    #Golubin Glacier
y_coordinate <- 42.28

SpatialPoints(cbind(x_coordinate,y_coordinate)) 
name_station <- "Golubin Glacier"


#Required Packages
install.packages("sp")
library(rgdal)
library(raster)
library(ncdf4)




##############################################################################################################################
# Working with Station Data 
##############################################################################################################################

#Data available at: http://sdss.caiag.kg/sdss/

temperature <- read.table("HM_data_2013-09-26", sep=",", dec=".", header=T)  

#Converting date to numeric format & Creating new data frame
year<-as.numeric(substr(temperature[,1],1,4))
month<-as.numeric(substr(temperature[,1],6,7))
day<-as.numeric(substr(temperature[,1],9,10))
hour<-as.numeric(substr(temperature[,1],12,13))
temp_obs<-data.frame(y=year, m=month, d=day, h=hour, T_station=temperature$Wert)

#Extracting relevant times of day (0, 6, 12, 18) -> 0=0-1am, 6=6-7am, 12=12-1pm, 18=6-7pm 
temp_obs<-subset(temp_obs, h==0|h==6|h==12|h==18)

#calculating the mean values for the respective time steps
temp_obs<-aggregate(temp_obs, by=list(temp_obs$y, temp_obs$m, temp_obs$d, temp_obs$h), FUN=mean, na.rm=T)
#Defining the date 
date<-ISOdate(temp_obs$y, temp_obs$m, temp_obs$d, temp_obs$h)
#Ordering the table by date
temp_obs<-temp_obs[order(date),]
#Deleting unnecessary groups
temp_obs<-temp_obs[,-(1:4)]



##############################################################################################################################
# Working with ERA Interim Data
##############################################################################################################################


########################################
##### ERA Interim - 2m Temperature #####
########################################

#reading ERA data into R
ERA_T<-brick("Z:/PRAKT/Anika_Pinzner/ERA Interim/2mTemperature_grib2netcdf-atls19-a562cefde8a29a7288fa0b8b7f9413f7-QxQ5UT.nc")

#Extracting 2mT (= ERA_T) for station data (for Spatial Point) 
ERA_T_ext<-extract(ERA_T,SpatialPoints(cbind(x_coordinate,y_coordinate)))
rm(ERA_T)

#Using the function substr() and as.numeric() -> extract date
y<-as.numeric(substr(colnames(ERA_T_ext),2,5))
m<-as.numeric(substr(colnames(ERA_T_ext),7,8))
d<-as.numeric(substr(colnames(ERA_T_ext),10,11))
h<-as.numeric(substr(colnames(ERA_T_ext),13,14))
#As Interim data based on 1, 7, 13, 19 in summer:
h<-round(h/6)*6

#Constructing new table with '2mTemperature' of Spatial Point
ERA_T_ext<-data.frame(y, m, d, h, T_ERA = ERA_T_ext[1,])



#############################################
##### ERA Interim - Geopotential height #####
#############################################

ERA_GPH<-brick("Z:/PRAKT/Anika_Pinzner/ERA Interim/Geopotentialheight_grib2netcdf-atls19-a562cefde8a29a7288fa0b8b7f9413f7-f40UOF.nc")

#Extracting GPH for station data (Spatial point)
ERA_GPH_ext<-extract(ERA_GPH,SpatialPoints(cbind(x_coordinate,y_coordinate)))
rm(ERA_GPH)

#Using the function substr() and as.numeric() -> extract date
y<-as.numeric(substr(colnames(ERA_GPH_ext),2,5))
m<-as.numeric(substr(colnames(ERA_GPH_ext),7,8))
d<-as.numeric(substr(colnames(ERA_GPH_ext),10,11))
h<-as.numeric(substr(colnames(ERA_GPH_ext),13,14))
h<-round(h/6)*6

#Constructing new table w/ 'GPH' of Spatial Point 
ERA_GPH_ext<-data.frame(y, m, d, h, GPH=ERA_GPH_ext[1,])



###################################################
##### ERA Interim - Total column water vapour #####  
###################################################

ERA_TCWV<-brick("Z:/PRAKT/Anika_Pinzner/ERA Interim/TCWV_grib2netcdf-atls04-a562cefde8a29a7288fa0b8b7f9413f7-i6r9so.nc")

#Extracting TCWV for station data (Spatial Point)
ERA_TCWV_ext<-extract(ERA_TCWV, SpatialPoints(cbind(x_coordinate,y_coordinate)))
rm(ERA_TCWV)

#Using the function substr() and as.numeric() -> extract date
y<-as.numeric(substr(colnames(ERA_TCWV_ext),2,5))
m<-as.numeric(substr(colnames(ERA_TCWV_ext),7,8))
d<-as.numeric(substr(colnames(ERA_TCWV_ext),10,11))
h<-as.numeric(substr(colnames(ERA_TCWV_ext),13,14))
h<-round(h/6)*6

#Constructing new table w/ 'TCWV' of Spatial Point
ERA_TCWV_ext<-data.frame(y, m, d, h, TCWV = ERA_TCWV_ext[1,])



###################################################
######## ERA Interim - U-Component of wind ########
###################################################


ERA_UCW<-brick("Z:/PRAKT/Anika_Pinzner/ERA Interim/UCW_grib2netcdf-atls17-a562cefde8a29a7288fa0b8b7f9413f7-BusyaH.nc")

#Extracting UCW for station data (Spatial Point)
ERA_UCW_ext<-extract(ERA_UCW,SpatialPoints(cbind(x_coordinate,y_coordinate)))
rm(ERA_UCW)

#Using the function substr() and as.numeric() -> extract date
y<-as.numeric(substr(colnames(ERA_UCW_ext),2,5))
m<-as.numeric(substr(colnames(ERA_UCW_ext),7,8))
d<-as.numeric(substr(colnames(ERA_UCW_ext),10,11))
h<-as.numeric(substr(colnames(ERA_UCW_ext),13,14))
h<-round(h/6)*6

#Constructing new table w/ 'UCW' of Spatial Point
ERA_UCW_ext<-data.frame(y, m, d, h, UCW = ERA_UCW_ext[1,])



###################################################
######## ERA Interim - V-Component of wind ######## 
###################################################


ERA_VCW<-brick("Z:/PRAKT/Anika_Pinzner/ERA Interim/VCW_grib2netcdf-atls06-a562cefde8a29a7288fa0b8b7f9413f7-AWDI2T.nc")

#Extracting VCW for station data (Spatial Point)
ERA_VCW_ext<-extract(ERA_VCW,SpatialPoints(cbind(x_coordinate,y_coordinate)))
rm(ERA_VCW)

#Using the function substr() and as.numeric() -> extract date
y<-as.numeric(substr(colnames(ERA_VCW_ext),2,5))
m<-as.numeric(substr(colnames(ERA_VCW_ext),7,8))
d<-as.numeric(substr(colnames(ERA_VCW_ext),10,11))
h<-as.numeric(substr(colnames(ERA_VCW_ext),13,14))
h<-round(h/6)*6

#Constructing new table w/ 'VCW' of Spatial Point
ERA_VCW_ext<-data.frame(y, m, d, h, VCW = ERA_VCW_ext[1,])



###################################################
###### ERA Interim - Mean Sea Level Pressure ######   
###################################################

ERA_MSLP<-brick("Z:/PRAKT/Anika_Pinzner/ERA Interim/MSLP_grib2netcdf-atls15-a562cefde8a29a7288fa0b8b7f9413f7-W2M6nq.nc")

#Extracting MSLP for station data (Spatial Point)
ERA_MSLP_ext<-extract(ERA_MSLP,SpatialPoints(cbind(x_coordinate,y_coordinate)))
rm(ERA_MSLP)

#Using the function substr() and as.numeric() -> extracting date
y<-as.numeric(substr(colnames(ERA_MSLP_ext),2,5))
m<-as.numeric(substr(colnames(ERA_MSLP_ext),7,8))
d<-as.numeric(substr(colnames(ERA_MSLP_ext),10,11))
h<-as.numeric(substr(colnames(ERA_MSLP_ext),13,14))
h<-round(h/6)*6

#Constructing new table w/ 'MSLP' of Spatial Point
ERA_MSLP_ext<-data.frame(y, m, d, h, MSLP = ERA_MSLP_ext[1,])



###################################################
######### ERA Interim - Total Cloud Cover #########       
###################################################


ERA_TCC<-brick("Z:/PRAKT/Anika_Pinzner/ERA Interim/TCC_grib2netcdf-atls12-a562cefde8a29a7288fa0b8b7f9413f7-A0rdO6.nc")

#Extracting TCC for station data (Spatial Point)
ERA_TCC_ext<-extract(ERA_TCC,SpatialPoints(cbind(x_coordinate,y_coordinate)))
rm(ERA_TCC)

#Using the function substr() and as.numeric() -> extracting date
y<-as.numeric(substr(colnames(ERA_TCC_ext),2,5))
m<-as.numeric(substr(colnames(ERA_TCC_ext),7,8))
d<-as.numeric(substr(colnames(ERA_TCC_ext),10,11))
h<-as.numeric(substr(colnames(ERA_TCC_ext),13,14))
h<-round(h/6)*6

#Constructing the new UCW_golubin table
ERA_TCC_ext<-data.frame(y, m, d, h, TCC = ERA_TCC_ext[1,])





################################################################################################################################

#Merging tables
data_all<-merge(ERA_T_ext, temp_obs, by=c("y", "m", "d", "h"), sort=F)
data_all<-merge(data_all, ERA_GPH_ext, by=c("y", "m", "d", "h"), sort=F)
data_all<-merge(data_all, ERA_TCWV_ext, by=c("y", "m", "d", "h"), sort=F)
data_all<-merge(data_all, ERA_UCW_ext, by=c("y", "m", "d", "h"), sort=F)
data_all<-merge(data_all, ERA_VCW_ext, by=c("y", "m", "d", "h"), sort=F)
data_all<-merge(data_all, ERA_MSLP_ext, by=c("y", "m", "d", "h"), sort=F)
data_all<-merge(data_all, ERA_TCC_ext, by=c("y", "m", "d", "h"), sort=F)

#Difference T_ERA/T_station
data_all$Tdiff<-data_all$T_ERA-data_all$T_station

#Ordering table
data_all<-cbind(y=data_all$y, m=data_all$m, d=data_all$d, h=data_all$h, T_station=data_all$T_station, T_ERA=data_all$T_ERA, Tdiff=data_all$Tdiff, GPH=data_all$GPH, TCWV=data_all$TCWV, UCW=data_all$UCW, VCW=data_all$VCW, MSLP=data_all$MSLP, TCC=data_all$TCC, VIWV=data_all$VIWV)
write.table(data_all, "downscaling_Temp_data_all.txt", col.names=T, row.names=F, sep="\t", quote=F)

# Creating 4 new tables according to 4 timeslots
df_data_all<-as.data.frame(data_all)

data_all_0<-subset(df_data_all, h==0)
data_all_6<-subset(df_data_all, h==6)
data_all_12<-subset(df_data_all, h==12)
data_all_18<-subset(df_data_all, h==18)



########################################################################################################################

#Correlation
cor(data_all)
write.table(cor(data_all), "correlation_data_all.txt", col.names=T, row.names=F, sep="\t", quote=F)



######################################################################################################################## 

# Normalization & Regression
name_station <- "Golubin Glacier"


#Normalizing data/ Regression for 0h
data_all_norm_0<-data_all_0

for(c in 8:13) 
  data_all_norm_0[,c]=((data_all_0[,c]-mean(data_all_0[,c]))/sd(data_all_0[,c]))
rg0<-lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=data_all_norm_0)

#plot(data_all_norm_0$Tdiff)
date<- ISOdate(data_all_norm_0$y, data_all_norm_0$m, data_all_norm_0$d, data_all_norm_0$h)
plot(date, data_all_norm_0$Tdiff, main = paste(name_station, "- RG 0h"), xlab="date", ylab="Tdiff [K]", ylim=c(-20,30))
points(date, rg0$fitted.values, col="red")



#Normalizing data/ Regression for 6h
data_all_norm_6<-data_all_6

for(c in 8:13) 
  data_all_norm_6[,c]=((data_all_6[,c]-mean(data_all_6[,c]))/sd(data_all_6[,c]))
rg6<-lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=data_all_norm_6)

#plot(data_all_norm_6$Tdiff)
date<- ISOdate(data_all_norm_6$y, data_all_norm_6$m, data_all_norm_6$d, data_all_norm_6$h)
plot(date, data_all_norm_6$Tdiff, main = paste(name_station, "- RG 6h"), xlab="date", ylab="Tdiff [K]", ylim=c(-20,30))
points(date, rg6$fitted.values, col="orange")



#Normalizing data/ Regression for 12h
data_all_norm_12<-data_all_12

for(c in 8:13) 
  data_all_norm_12[,c]=((data_all_12[,c]-mean(data_all_12[,c]))/sd(data_all_12[,c]))
rg12<-lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=data_all_norm_12)

#plot(data_all_norm_12$Tdiff)
date<- ISOdate(data_all_norm_12$y, data_all_norm_12$m, data_all_norm_12$d, data_all_norm_12$h)
plot(date, data_all_norm_12$Tdiff, main = paste(name_station, "- RG 12h"), xlab="date", ylab="Tdiff [K]", ylim=c(-20,30))
points(date, rg12$fitted.values, col="gold")



#Normalizing data/ Regression for 18h
data_all_norm_18<-data_all_18

for(c in 8:13) 
  data_all_norm_18[,c]=((data_all_18[,c]-mean(data_all_18[,c]))/sd(data_all_18[,c]))
rg18<-lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=data_all_norm_18)

#plot(data_all_norm_18$Tdiff)
date<- ISOdate(data_all_norm_18$y, data_all_norm_18$m, data_all_norm_18$d, data_all_norm_18$h)
plot(date, data_all_norm_18$Tdiff, main = paste(name_station, "- RG 18h"), xlab="date", ylab="Tdiff [K]", ylim=c(-20,30))
points(date, rg18$fitted.values, col="chartreuse3")



#Normalizing data_all/ Regression for data_all
df_data_all_norm <- df_data_all

for(c in 8:13) 
  df_data_all_norm[,c]=((df_data_all[,c]-mean(df_data_all[,c]))/sd(df_data_all[,c]))

#Linear Regression
rg<-lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=df_data_all_norm)

#plot(df_data_all_norm$Tdiff)
date<- ISOdate(df_data_all_norm$y, df_data_all_norm$m, df_data_all_norm$d, df_data_all_norm$h)
plot(date, df_data_all_norm$Tdiff, main = paste(name_station, "- RG data_all"), xlab="date", ylab="Tdiff [K]", ylim=c(-20,30))
points(date, rg$fitted.values, col="steelblue") 




########################################################################################################################

#Crossvalidation 0h
data_all_norm_0_cv<-data_all_norm_0

cv_results_0h<-vector()

for(i in 1:nrow(data_all_norm_0))
  
{
  
  data_all_norm_0_cv <- data_all_norm_0[-i,]
  
  rg0_cv<-lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=data_all_norm_0_cv)
  
  results_0h<-predict(rg0_cv, newdata=data_all_norm_0[i,])
  
  cv_results_0h<-append(cv_results_0h, results_0h)
}




#Adding cv model results to table_0h
data_all_norm_0_cv <- cbind(data_all_norm_0, cv_results_0h)
colnames(data_all_norm_0_cv)[ncol(data_all_norm_0_cv)]<-"cv"

#Correlation_0h
cor_0h<-cor(data_all_norm_0_cv$Tdiff, data_all_norm_0_cv$cv)

#Residual sum of squares: RSS_0h
RSS_0h<-sum((data_all_norm_0_cv$Tdiff-data_all_norm_0_cv$cv)^2)
RSS_0h<-sqrt(RSS_0h/(nrow(data_all_norm_0_cv)))



date<- ISOdate(data_all_norm_0$y, data_all_norm_0$m, data_all_norm_0$d, data_all_norm_0$h)
plot(date, cv_results_0h, main = paste(name_station, "- CV 0h"), xlab="date", ylab="Tdiff [K]", col="violetred3", ylim=c(-20,30))
points(date, data_all_norm_0$Tdiff)
legend("topright", legend=c(paste("cor =",cor_0h), paste("RSS =" ,RSS_0h)), bty='n', horiz=FALSE, cex=0.7)
legend("bottomleft", legend =c(paste("C: GPH= ", round(rg0$coefficients[1], digits=2)), 
                               paste("TCWV= ", round(rg0$coefficients[2], digits=2)), 
                               paste("UCW= ", round(rg0$coefficients[3], digits=2)), 
                               paste("VCW= ", round(rg0$coefficients[4], digits=2)),
                               paste("MSLP= ", round(rg0$coefficients[5], digits=2)), 
                               paste("TCC= ", round(rg0$coefficients[6], digits=2))), horiz=TRUE, bty='n', cex=0.7)




#Crossvalidation 6h
data_all_norm_6_cv<-data_all_norm_6

cv_results_6h<-vector()
for(i in 1:nrow(data_all_norm_6))
  
{
  
  data_all_norm_6_cv <- data_all_norm_6[-i,]
  
  rg6_cv<-lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=data_all_norm_6_cv)
  
  results_6h<-predict(rg6_cv, newdata=data_all_norm_6[i,])
  
  cv_results_6h<-append(cv_results_6h, results_6h)
}


#Adding cv model results to table_6h
data_all_norm_6_cv <- cbind(data_all_norm_6, cv_results_6h)
colnames(data_all_norm_6_cv)[ncol(data_all_norm_6_cv)]<-"cv"

##Correlation_6h
cor_6h<-cor(data_all_norm_6_cv$Tdiff, data_all_norm_6_cv$cv)

#Residual sum of squares: RSS_6h
RSS_6h<-sum((data_all_norm_6_cv$Tdiff-data_all_norm_6_cv$cv)^2)
RSS_6h<-sqrt(RSS_6h/(nrow(data_all_norm_6_cv)))



date<- ISOdate(data_all_norm_6$y, data_all_norm_6$m, data_all_norm_6$d, data_all_norm_6$h)
plot(date, cv_results_6h, main = paste(name_station, "- CV 6h"), xlab="date", ylab="Tdiff [K]", col="darkcyan", ylim=c(-20,30))
points(date, data_all_norm_6$Tdiff)
legend("topright", legend=c(paste("cor =",cor_6h), paste("RSS =" ,RSS_6h)), bty='n', horiz=FALSE, cex=0.7)
legend("bottomleft", legend =c(paste("C: GPH= ", round(rg6$coefficients[1], digits=2)), 
                               paste("TCWV= ", round(rg6$coefficients[2], digits=2)), 
                               paste("UCW= ", round(rg6$coefficients[3], digits=2)), 
                               paste("VCW= ", round(rg6$coefficients[4], digits=2)),
                               paste("MSLP= ", round(rg6$coefficients[5], digits=2)), 
                               paste("TCC= ", round(rg6$coefficients[6], digits=2))), horiz=TRUE, bty='n', cex=0.7)




#Crossvalidation 12h
data_all_norm_12_cv<-data_all_norm_12

cv_results_12h<-vector()
for(i in 1:nrow(data_all_norm_12))
  
{
  
  data_all_norm_12_cv <- data_all_norm_12[-i,]
  
  rg12_cv<-lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=data_all_norm_12_cv)
  
  results_12h<-predict(rg12_cv, newdata=data_all_norm_12[i,])
  
  cv_results_12h<-append(cv_results_12h, results_12h)
}



#Adding cv model results to table_12h
data_all_norm_12_cv <- cbind(data_all_norm_12, cv_results_12h)
colnames(data_all_norm_12_cv)[ncol(data_all_norm_12_cv)]<-"cv"

##Correlation_12h
cor_12h<-cor(data_all_norm_12_cv$Tdiff, data_all_norm_12_cv$cv)

#Residual sum of squares: RSS_12h
RSS_12h<-sum((data_all_norm_12_cv$Tdiff-data_all_norm_12_cv$cv)^2)
RSS_12h<-sqrt(RSS_12h/(nrow(data_all_norm_12_cv)))



date<- ISOdate(data_all_norm_12$y, data_all_norm_12$m, data_all_norm_12$d, data_all_norm_12$h)
plot(date, cv_results_12h, main = paste(name_station, "- CV 12h"), xlab="date", ylab="Tdiff [K]", col="olivedrab3", ylim=c(-20,30))
points(date, data_all_norm_12$Tdiff)
legend("topright", legend=c(paste("cor =",cor_12h), paste("RSS =" ,RSS_12h)), bty='n', horiz=FALSE, cex=0.7)
legend("bottomleft", legend =c(paste("C: GPH= ", round(rg12$coefficients[1], digits=2)), 
                               paste("TCWV= ", round(rg12$coefficients[2], digits=2)), 
                               paste("UCW= ", round(rg12$coefficients[3], digits=2)), 
                               paste("VCW= ", round(rg12$coefficients[4], digits=2)),
                               paste("MSLP= ", round(rg12$coefficients[5], digits=2)), 
                               paste("TCC= ", round(rg12$coefficients[6], digits=2))), horiz=TRUE, bty='n', cex=0.7)



#Crossvalidation 18h
data_all_norm_18_cv<-data_all_norm_18

cv_results_18h<-vector()
for(i in 1:nrow(data_all_norm_18))
  
{
  
  data_all_norm_18_cv <- data_all_norm_18[-i,]
  
  rg18_cv<-lm(Tdiff~GPH+TCWV+UCW+VCW+MSLP+TCC, data=data_all_norm_18_cv)
  
  results_18h<-predict(rg18_cv, newdata=data_all_norm_18[i,])
  
  cv_results_18h<-append(cv_results_18h, results_18h)
}



#Adding cv model results to table_18h
data_all_norm_18_cv <- cbind(data_all_norm_18, cv_results_18h)
colnames(data_all_norm_18_cv)[ncol(data_all_norm_18_cv)]<-"cv"

#Correlation_18h
cor_18h<-cor(data_all_norm_18_cv$Tdiff, data_all_norm_18_cv$cv)

#Residual sum of squares: RSS_18h
RSS_18h<-sum((data_all_norm_18_cv$Tdiff-data_all_norm_18_cv$cv)^2)
RSS_18h<-sqrt(RSS_18h/(nrow(data_all_norm_18_cv)))



date<- ISOdate(data_all_norm_18$y, data_all_norm_18$m, data_all_norm_18$d, data_all_norm_18$h)
plot(date, cv_results_18h, main = paste(name_station, "- CV 18h"), xlab="date", ylab="Tdiff [K]", col="darkorange2", ylim=c(-20,30))
points(date, data_all_norm_18$Tdiff)
legend("topright", legend=c(paste("cor =",cor_18h), paste("RSS =" ,RSS_18h)), bty='n', horiz=FALSE, cex=0.7)
legend("bottomleft", legend =c(paste("C: GPH= ", round(rg18$coefficients[1], digits=2)), 
                               paste("TCWV= ", round(rg18$coefficients[2], digits=2)), 
                               paste("UCW= ", round(rg18$coefficients[3], digits=2)), 
                               paste("VCW= ", round(rg18$coefficients[4], digits=2)),
                               paste("MSLP= ", round(rg18$coefficients[5], digits=2)), 
                               paste("TCC= ", round(rg18$coefficients[6], digits=2))), horiz=TRUE, bty='n', cex=0.7)





#table including cv model results for all time steps
data_all_norm_all_cv<- rbind(data_all_norm_0_cv, data_all_norm_6_cv, data_all_norm_12_cv, data_all_norm_18_cv)
data_all_norm_all_cv<- data_all_norm_all_cv[order(ISOdate(data_all_norm_all_cv$y, 
                                                          data_all_norm_all_cv$m, data_all_norm_all_cv$d, data_all_norm_all_cv$h)),]








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


#Creating 4 new tables according to 4 timeslots, all_variables
data_all_H_0h<-subset(data_all_H, h==0)
data_all_H_6h<-subset(data_all_H, h==6)
data_all_H_12h<-subset(data_all_H, h==12)
data_all_H_18h<-subset(data_all_H, h==18)

tail(data_all_H_18h)



######## 0h ########
#Normalizing the data
data_all_norm_H_0h<-data_all_H_0h

data_all_norm_H_0h_date<- data_all_norm_H_0h[,1:4]
data_all_norm_H_0h<- data_all_norm_H_0h[,-(1:5)]
data_all_0<-data_all_0[,-(1:7)]

for(c in 1:ncol(data_all_0)) 
  data_all_norm_H_0h[,c]=((data_all_norm_H_0h[,c]-mean(data_all_0[,c]))/sd(data_all_0[,c]))


#Predicting past temperatures on basis of ERA-Interim temperatures
results_0h_H<-predict(rg0, newdata=data_all_norm_H_0h) # only predictors

#Adding Temperature difference to T_ERA in order to get T_Station_H_18h
T_Station_H_0h<-(data_all_H_0h$T_ERA - results_0h_H)

head(data_all_H_0h)
head(T_Station_H_0h)
head(results_0h_H)

### table: date (date+T_Station)
data.frame(data_all_norm_H_0h_date, T_Station_H_0h)


date<- ISOdate(data_all_H_0h$y, data_all_H_0h$m, data_all_H_0h$d, data_all_H_0h$h)
plot(date, data_all_H_0h$T_ERA, main = paste(name_station, "- Temperatures Hindcast_0h", time_difference), xlab="date", ylab="T [K]", col="black", ylim=c(-20,40))
points(date, T_Station_H_0h, col="aquamarine4")
date<- ISOdate(data_all_norm_0_cv$y, data_all_norm_0_cv$m, data_all_norm_0_cv$d, data_all_norm_0_cv$h)
points(date, data_all_norm_0_cv$T_station, col="brown")
legend("topright", legend=c(paste("T_ERA"), paste("T_Station_H"), paste("T_Station")), 
       lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("black","aquamarine4","brown"),bty='n', horiz=FALSE, cex=0.7)




######## 6h ########

#Normalizing the data
data_all_norm_H_6h<-data_all_H_6h

data_all_norm_H_6h_date<- data_all_norm_H_6h[,1:4]
data_all_norm_H_6h<- data_all_norm_H_6h[,-(1:5)]
data_all_6<-data_all_6[,-(1:7)]

for(c in 1:ncol(data_all_6)) 
  data_all_norm_H_6h[,c]=((data_all_norm_H_6h[,c]-mean(data_all_6[,c]))/sd(data_all_6[,c]))


#Predicting past temperatures on basis of ERA-Interim temperatures
results_6h_H<-predict(rg6, newdata=data_all_norm_H_6h) # only predictors

#Adding Temperature difference to T_ERA in order to get T_Station_H_18h
T_Station_H_6h<-(data_all_H_6h$T_ERA - results_6h_H)

head(data_all_H_6h)
head(T_Station_H_6h)
head(results_6h_H)

### table: date (date+T_Station)
data.frame(data_all_norm_H_6h_date, T_Station_H_6h)


date<- ISOdate(data_all_H_6h$y, data_all_H_6h$m, data_all_H_6h$d, data_all_H_6h$h)
plot(date, data_all_H_6h$T_ERA, main = paste(name_station, "- Temperatures Hindcast_6h", time_difference), xlab="date", ylab="T [K]", col="black", ylim=c(-20,40))
points(date, T_Station_H_6h, col="darkcyan")
date<- ISOdate(data_all_norm_6_cv$y, data_all_norm_6_cv$m, data_all_norm_6_cv$d, data_all_norm_6_cv$h)
points(date, data_all_norm_6_cv$T_station, col="brown")
legend("topright", legend=c(paste("T_ERA"), paste("T_Station_H"), paste("T_Station")), 
       lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("black","darkcyan","brown"),bty='n', horiz=FALSE, cex=0.7)




######## 12h ########

#Normalizing the data
data_all_norm_H_12h<-data_all_H_12h

data_all_norm_H_12h_date<- data_all_norm_H_12h[,1:4]
data_all_norm_H_12h<- data_all_norm_H_12h[,-(1:5)]
data_all_12<-data_all_12[,-(1:7)]

for(c in 1:ncol(data_all_12)) 
  data_all_norm_H_12h[,c]=((data_all_norm_H_12h[,c]-mean(data_all_12[,c]))/sd(data_all_12[,c]))


#Predicting past temperatures on basis of ERA-Interim temperatures
results_12h_H<-predict(rg12, newdata=data_all_norm_H_12h) # only predictors

#Adding Temperature difference to T_ERA in order to get T_Station_H_18h
T_Station_H_12h<-(data_all_H_12h$T_ERA - results_12h_H)

head(data_all_H_12h)
head(T_Station_H_12h)
head(results_12h_H)

### table: date (date+T_Station)
data.frame(data_all_norm_H_12h_date, T_Station_H_12h)


date<- ISOdate(data_all_H_12h$y, data_all_H_12h$m, data_all_H_12h$d, data_all_H_12h$h)
plot(date, data_all_H_12h$T_ERA, main = paste(name_station, "- Temperatures Hindcast_12h", time_difference), xlab="date", ylab="T [K]", col="black", ylim=c(-20,40))
points(date, T_Station_H_12h, col="deepskyblue4")
date<- ISOdate(data_all_norm_12_cv$y, data_all_norm_12_cv$m, data_all_norm_12_cv$d, data_all_norm_12_cv$h)
points(date, data_all_norm_12_cv$T_station, col="brown")
legend("topright", legend=c(paste("T_ERA"), paste("T_Station_H"), paste("T_Station")), 
       lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("black","deepskyblue4","brown"),bty='n', horiz=FALSE, cex=0.7)





######## 18h ########

#Normalizing the data
data_all_norm_H_18h<-data_all_H_18h

data_all_norm_H_18h_date<- data_all_norm_H_18h[,1:4]
data_all_norm_H_18h<- data_all_norm_H_18h[,-(1:5)]
data_all_18<-data_all_18[,-(1:7)]

for(c in 1:ncol(data_all_18)) 
  data_all_norm_H_18h[,c]=((data_all_norm_H_18h[,c]-mean(data_all_18[,c]))/sd(data_all_18[,c]))


#Predicting past temperatures on basis of ERA-Interim temperatures
results_18h_H<-predict(rg18, newdata=data_all_norm_H_18h) # only predictors

#Adding Temperature difference to T_ERA in order to get T_Station_H_18h
T_Station_H_18h<-(data_all_H_18h$T_ERA - results_18h_H)

head(data_all_H_18h)
head(T_Station_H_18h)
head(results_18h_H)

### table: date (date+T_Station)
data.frame(data_all_norm_H_18h_date, T_Station_H_18h)


date<- ISOdate(data_all_H_18h$y, data_all_H_18h$m, data_all_H_18h$d, data_all_H_18h$h)
plot(date, data_all_H_18h$T_ERA, main = paste(name_station, "- Temperatures Hindcast_18h", time_difference), xlab="date", ylab="T [K]", col="black", ylim=c(-20,40))
points(date, T_Station_H_18h, col="dodgerblue4")
date<- ISOdate(data_all_norm_18_cv$y, data_all_norm_18_cv$m, data_all_norm_18_cv$d, data_all_norm_18_cv$h)
points(date, data_all_norm_18_cv$T_station, col="brown")
legend("topright", legend=c(paste("T_ERA"), paste("T_Station_H"), paste("T_Station")), 
       lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("black","dodgerblue4","brown"),bty='n', horiz=FALSE, cex=0.7)




#table including temperatures of hindcast for all time steps
H_data_all<- rbind(T_Station_H_0h, T_Station_H_6h, T_Station_H_12h, T_Station_H_18h)
date<-ISOdate(data_all_norm_H_0h$y, data_all_norm_H_0h$m, data_all_norm_H_0h$d, data_all_norm_H_0h$h)
H_data_all<- H_data_all[order(date),]

write.table(H_data_all, "H_downscaling_Temp_data_all_GG.txt", col.names=T, row.names=F, sep="\t", quote=F)
