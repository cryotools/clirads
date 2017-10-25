########################################################################################################################
#Plotting

name_station <- "Golubin Glacier"

#Plots for Tdiff/0,6,12,18
date<- ISOdate(data_all_0$y, data_all_0$m, data_all_0$d, data_all_0$h)
plot(date, data_all_0$Tdiff, main = paste(name_station, "- Temperature difference, 0h"), ylab = "T-difference [K]")

date<- ISOdate(data_all_6$y, data_all_6$m, data_all_6$d, data_all_6$h)
plot(date, data_all_6$Tdiff, main = paste(name_station, "- Temperature difference, 6h"), ylab = "T-difference [K]")

date<- ISOdate(data_all_12$y, data_all_12$m, data_all_12$d, data_all_12$h)
plot(date, data_all_12$Tdiff, main = paste(name_station, "- Temperature difference, 12h"), ylab = "T-difference [K]")

date<- ISOdate(data_all_18$y, data_all_18$m, data_all_18$d, data_all_18$h)
plot(date, data_all_18$Tdiff, main = paste(name_station, "- Temperature difference, 18h"), ylab = "T-difference [K]")








########################################################################################################################
#Plots for Tdiff/0,6,12,18 - Seasonal

#Plots for Tdiff/Winter

#Tdiff 0h Winter
data_all_dec<-subset(data_all_0, m==12)
data_all_jan<-subset(data_all_0, m==1)
data_all_feb<-subset(data_all_0, m==2)

data_all_W<-rbind(data_all_dec, data_all_jan, data_all_feb)

date<- ISOdate(data_all_W[,1], data_all_W[,2], data_all_W[,3], data_all_W[,4])
plot(date, data_all_W$Tdiff, main = paste(name_station, " - Winter months -Temperature difference, 0h"), ylab = "T-difference [K]")

#Tdiff 6h Winter
data_all_dec<-subset(data_all_6, m==12)
data_all_jan<-subset(data_all_6, m==1)
data_all_feb<-subset(data_all_6, m==2)

data_all_W<-rbind(data_all_dec, data_all_jan, data_all_feb)

date<- ISOdate(data_all_W[,1], data_all_W[,2], data_all_W[,3], data_all_W[,4])
plot(date, data_all_W$Tdiff, main = paste(name_station, "- Winter months -Temperature difference, 6h"), ylab = "T-difference [K]")

#Tdiff 12h Winter
data_all_dec<-subset(data_all_12, m==12)
data_all_jan<-subset(data_all_12, m==1)
data_all_feb<-subset(data_all_12, m==2)

data_all_W<-rbind(data_all_dec, data_all_jan, data_all_feb)

date<- ISOdate(data_all_W[,1], data_all_W[,2], data_all_W[,3], data_all_W[,4])
plot(date, data_all_W$Tdiff, main = paste(name_station, "- Winter months -Temperature difference, 12h"), ylab = "T-difference [K]")

#Tdiff 18h Winter
data_all_dec<-subset(data_all_18, m==12)
data_all_jan<-subset(data_all_18, m==1)
data_all_feb<-subset(data_all_18, m==2)

data_all_W<-rbind(data_all_dec, data_all_jan, data_all_feb)

date<- ISOdate(data_all_W[,1], data_all_W[,2], data_all_W[,3], data_all_W[,4])
plot(date, data_all_W$Tdiff, main = paste(name_station, "- Winter months -Temperature difference, 18h"), ylab = "T-difference [K]")


#Plots for Tdiff/Summer

#Tdiff 0h Summer
data_all_jun<-subset(data_all_0, m==6)
data_all_jul<-subset(data_all_0, m==7)
data_all_aug<-subset(data_all_0, m==8)

data_all_S<-rbind(data_all_jun, data_all_jul, data_all_aug)

date<- ISOdate(data_all_S[,1], data_all_S[,2], data_all_S[,3], data_all_S[,4])
plot(date, data_all_S$Tdiff, main = paste(name_station, "- Summer months -Temperature difference, 0h"), ylab = "T-difference [K]")


#Tdiff 6h Summer
data_all_jun<-subset(data_all_6, m==6)
data_all_jul<-subset(data_all_6, m==7)
data_all_aug<-subset(data_all_6, m==8)

data_all_S<-rbind(data_all_jun, data_all_jul, data_all_aug)

date<- ISOdate(data_all_S[,1], data_all_S[,2], data_all_S[,3], data_all_S[,4])
plot(date, data_all_S$Tdiff, main = paste(name_station, "- Summer months -Temperature difference, 6h"), ylab = "T-difference [K]")


#Tdiff 12h Summer
data_all_jun<-subset(data_all_12, m==6)
data_all_jul<-subset(data_all_12, m==7)
data_all_aug<-subset(data_all_12, m==8)

data_all_S<-rbind(data_all_jun, data_all_jul, data_all_aug)

date<- ISOdate(data_all_S[,1], data_all_S[,2], data_all_S[,3], data_all_S[,4])
plot(date, data_all_S$Tdiff, main = paste(name_station, "- Summer months -Temperature difference, 12h"), ylab = "T-difference [K]")


#Tdiff 18h Summer
data_all_jun<-subset(data_all_18, m==6)
data_all_jul<-subset(data_all_18, m==7)
data_all_aug<-subset(data_all_18, m==8)

data_all_S<-rbind(data_all_jun, data_all_jul, data_all_aug)

date<- ISOdate(data_all_S[,1], data_all_S[,2], data_all_S[,3], data_all_S[,4])
plot(date, data_all_S$Tdiff, main = paste(name_station, "- Summer months -Temperature difference, 18h"), ylab = "T-difference [K]")





########################################################################################################################
#Plots for all Variables/0,6,12,18

#GPH
date<- ISOdate(data_all_0$y, data_all_0$m, data_all_0$d, data_all_0$h)
plot(date, data_all_0$GPH, main = paste(name_station, "- Geopotential Height, 0h"), ylab = "GPH [m**2/s**2]")

date<- ISOdate(data_all_6$y, data_all_6$m, data_all_6$d, data_all_6$h)
plot(date, data_all_6$GPH, main = paste(name_station, "- Geopotential Height, 6h"), ylab = "GPH [m**2/s**2]")

date<- ISOdate(data_all_12$y, data_all_12$m, data_all_12$d, data_all_12$h)
plot(date, data_all_12$GPH, main = paste(name_station, "- Geopotential Height, 12h"), ylab = "GPH [m**2/s**2]")

date<- ISOdate(data_all_18$y, data_all_18$m, data_all_18$d, data_all_18$h)
plot(date, data_all_18$GPH, main = paste(name_station, "- Geopotential Height, 18h"), ylab = "GPH [m**2/s**2]")


#TCWV
date<- ISOdate(data_all_0$y, data_all_0$m, data_all_0$d, data_all_0$h)
plot(date, data_all_0$TCWV, main = paste(name_station, "- Total Column Water Vapour, 0h"), ylab = "TCWV [kg/m^2]")

date<- ISOdate(data_all_6$y, data_all_6$m, data_all_6$d, data_all_6$h)
plot(date, data_all_6$TCWV, main = paste(name_station, "- Total Column Water Vapour, 6h"), ylab = "TCWV [kg/m^2]")

date<- ISOdate(data_all_12$y, data_all_12$m, data_all_12$d, data_all_12$h)
plot(date, data_all_12$TCWV, main = paste(name_station, "- Total Column Water Vapour, 12h"), ylab = "TCWV [kg/m^2]")

date<- ISOdate(data_all_18$y, data_all_18$m, data_all_18$d, data_all_18$h)
plot(date, data_all_18$TCWV, main = paste(name_station, "- Total Column Water Vapour, 18h"), ylab = "TCWV [kg/m^2]")


#UCW
date<- ISOdate(data_all_0$y, data_all_0$m, data_all_0$d, data_all_0$h)
plot(date, data_all_0$UCW, main = paste(name_station, "- U Component of Wind, 0h"), ylab = "UCW [m/s]")

date<- ISOdate(data_all_6$y, data_all_6$m, data_all_6$d, data_all_6$h)
plot(date, data_all_6$UCW, main = paste(name_station, "- U Component of Wind, 6h"), ylab = "UCW [m/s]")

date<- ISOdate(data_all_12$y, data_all_12$m, data_all_12$d, data_all_12$h)
plot(date, data_all_12$UCW, main = paste(name_station, "- U Component of Wind, 12h"), ylab = "UCW [m/s]")

date<- ISOdate(data_all_18$y, data_all_18$m, data_all_18$d, data_all_18$h)
plot(date, data_all_18$UCW, main = paste(name_station, "- U Component of Wind, 18h"), ylab = "UCW [m/s]")


#VCW
date<- ISOdate(data_all_0$y, data_all_0$m, data_all_0$d, data_all_0$h)
plot(date, data_all_0$VCW, main = paste(name_station, "- V Component of Wind, 0h"), ylab = "VCW [m/s]")

date<- ISOdate(data_all_6$y, data_all_6$m, data_all_6$d, data_all_6$h)
plot(date, data_all_6$VCW, main = paste(name_station, "- V Component of Wind, 6h"), ylab = "VCW [m/s]")

date<- ISOdate(data_all_12$y, data_all_12$m, data_all_12$d, data_all_12$h)
plot(date, data_all_12$VCW, main = paste(name_station, "- V Component of Wind, 12h"), ylab = "VCW [m/s]")

date<- ISOdate(data_all_18$y, data_all_18$m, data_all_18$d, data_all_18$h)
plot(date, data_all_18$VCW, main = paste(name_station, "- V Component of Wind, 18h"), ylab = "VCW [m/s]")


#MSLP
date<- ISOdate(data_all_0$y, data_all_0$m, data_all_0$d, data_all_0$h)
plot(date, data_all_0$MSLP, main = paste(name_station, "- Mean Sea Level Pressure, 0h"), ylab = "MSLP [Pa]")

date<- ISOdate(data_all_6$y, data_all_6$m, data_all_6$d, data_all_6$h)
plot(date, data_all_6$MSLP, main = paste(name_station, "- Mean Sea Level Pressure, 6h"), ylab = "MSLP [Pa]")

date<- ISOdate(data_all_12$y, data_all_12$m, data_all_12$d, data_all_12$h)
plot(date, data_all_12$MSLP, main = paste(name_station, "- Mean Sea Level Pressure, 12h"), ylab = "MSLP [Pa]")

date<- ISOdate(data_all_18$y, data_all_18$m, data_all_18$d, data_all_18$h)
plot(date, data_all_18$MSLP, main = paste(name_station, "- Mean Sea Level Pressure, 18h"), ylab = "MSLP [Pa]")



#TCC
date<- ISOdate(data_all_0$y, data_all_0$m, data_all_0$d, data_all_0$h)
plot(date, data_all_0$TCC, main = paste(name_station, "- Total Cloud Cover, 0h"), ylab = "TCC [0-1]")

date<- ISOdate(data_all_6$y, data_all_6$m, data_all_6$d, data_all_6$h)
plot(date, data_all_6$TCC, main = paste(name_station, "- Total Cloud Cover, 6h"), ylab = "TCC [0-1]")

date<- ISOdate(data_all_12$y, data_all_12$m, data_all_12$d, data_all_12$h)
plot(date, data_all_12$TCC, main = paste(name_station, "- Total Cloud Cover, 12h"), ylab = "TCC [0-1]")

date<- ISOdate(data_all_18$y, data_all_18$m, data_all_18$d, data_all_18$h)
plot(date, data_all_18$TCC, main = paste(name_station, "- Total Cloud Cover, 18h"), ylab = "TCC [0-1]")



