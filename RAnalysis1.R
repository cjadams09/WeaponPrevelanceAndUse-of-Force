setwd("~/GunStatsAnalysis")
library(readr)
library(dplyr)
library(rgdal)
library(leaflet)
library(stargazer)
library(latticeExtra)
library(gtools)


#Importing Data
ArrestReport <- read.csv("NYPD_Arrests_2019.csv", as.is = TRUE)
summary(ArrestReport)
ArrestReport1 <- ArrestReport[ArrestReport$OFNS_DESC == "DANGEROUS WEAPONS" & ArrestReport$OFNS_DESC != "PL 2650101",]
ArrestReport2 <- ArrestReport[ArrestReport$OFNS_DESC == "PL 2650101",]
ArrestReport3 <- ArrestReport[ArrestReport$OFNS_DESC == "DANGEROUS WEAPONS",]
write.csv(ArrestReport,"ArrestReport.csv")

x <- length(unique(ArrestReport[ArrestReport$ARREST_PRECINCT == 22,]$ARREST_KEY))
x <- ArrestReport[ArrestReport$ARREST_PRECINCT == 22,]

FullArrRepo <- read.csv("NYPD_Arrests_Data_06-19.csv", as.is = TRUE)
FullArrRepo$year <- as.numeric(substr(FullArrRepo$ARREST_DATE,7,11))
unique(FullArrRepo$year)
write.csv(FullArrRepo,"FullArrRepo.csv")
FullArrRepo <- read.csv("FullArrRepo.csv")

summary(FullArrRepo)

CurrArrRepo <- read.csv("NYPD_Arrest_Data__Year_to_Date_.csv", as.is = TRUE)

callsmap <- read.csv("NYPD_Complaint_Map_Historic.csv", as.is = TRUE)
summary(callsmap)

sqf2017 <- read.csv("sqf-2017.csv", as.is = TRUE)
sqf2018 <- read.csv("sqf-2018.csv", as.is = TRUE)
colnames(sqf2018)[3] <- "STOP_FRISK_TIME"
sqf2019 <- read.csv("sqf-2019.csv", as.is = TRUE)
colnames(sqf2019)[1] <- colnames(sqf2018)[1]

format(sqf3_mod, scientific=FALSE)
sqf2017_mod <- sqf2017[c(1:8,18,23:26,31:73,79:83),]
sqf2018_mod <- sqf2018[c(1:8,18,23:26,31:73,79:83),]
sqf2019_mod <- sqf2019[c(1:8,17,22:25,30,31,34:74,79:83),]

sqf3 <- rbind(sqf2017_mod,sqf2018_mod)
sqf3 <- smartbind(sqf3,sqf2019_mod)

sqf3_mod <- sqf3[c(1,4,7,18,19,23,31:66,73)]
colnames(sqf3_mod)[2] <- "Year"
colnames(sqf3_mod)[1] <- "ID"

##Precinct Demographics
precinctpop <- read.csv("nyc_precincts_pop.csv", as.is = TRUE)
precpoptot <- precinctpop %>% select(1,73:74,77:82)
precpoptot <- as.data.frame(precpoptot, stringsAsFactors = FALSE)
colnames(precpoptot) <- c("Precinct", "Total Pop", "Hisp Pop", "White Pop", "Black Pop", "Native Pop", "Asian Pop", "Pacific Islander Pop","Other Pop")

##Shootings
precinctshootings <- read.csv("NYPD_Shooting_Incident_Data__Historic_.csv", as.is = TRUE)

##Police Aggression
y <- read.csv("PrecinctComplaints.csv", as.is = TRUE)
summary(y)

z <- read.csv("UseOfForceFlat.csv", as.is = TRUE)

a <- read.csv("InjuryReports - Officer.csv", as.is = TRUE)
b <- read.csv("InjuryReports - Subject.csv", as.is = TRUE)

forcereport2019 <- read.csv("forcereport2019.csv")
forcereport2018 <- read.csv("forcereport2018.csv")
forcereport2017 <- read.csv("forcereport2017.csv")

fullforcereport <- rbind(forcereport2017,forcereport2018,forcereport2019)
fullforcereport <- fullforcereport[!is.na(fullforcereport[,2]),2:4]
write.csv(fullforcereport,"fullforcereport.csv")
fullforcereport <- read.csv("fullforcereport.csv")

ArrestReport <- read.csv("ArrestReport.csv", as.is = TRUE)

ArrestReport <- ArrestReport %>% filter(
  is.na(ArrestReport$Latitude)==FALSE &
    is.na(ArrestReport$Longitude)==FALSE)
ArrestReport <- ArrestReport[ArrestReport$ARREST_BORO == "K",]
arrwhite <-ArrestReport[ArrestReport$PERP_RACE == "WHITE",]
arrhisp <- ArrestReport[ArrestReport$PERP_RACE == "BLACK HISPANIC" | ArrestReport$PERP_RACE == "WHITE HISPANIC",]
arrblack <- ArrestReport[ArrestReport$PERP_RACE == "BLACK",]
arrnative <- ArrestReport[ArrestReport$PERP_RACE == "AMERICAN INDIAN/ALASKAN NATIVE",]
arrasian <- ArrestReport[ArrestReport$PERP_RACE == "ASIAN / PACIFIC ISLANDER",]
arrunknown <- ArrestReport[ArrestReport$PERP_RACE == "UNKNOWN",]


ArrestReport1 <- ArrestReport1 %>% filter(
  is.na(ArrestReport1$Latitude)==FALSE &
    is.na(ArrestReport1$Longitude)==FALSE)

ArrestReport2 <- ArrestReport2 %>% filter(
  is.na(ArrestReport2$Latitude)==FALSE &
    is.na(ArrestReport2$Longitude)==FALSE)
# Plot
leaflet() %>% 
  addTiles() %>% # add the background map
  addCircles(lng = arrwhite$Longitude, lat = arrwhite$Latitude,radius = 1, color = "#00F") %>%
  addCircles(lng = arrblack$Longitude, lat = arrblack$Latitude,radius = 1, color = "#0F0", opacity = 25) %>%
  addCircles(lng = arrhisp$Longitude, lat = arrhisp$Latitude,radius = 1, color = "#F00", opacity = 25) %>%
  addCircles(lng = arrasian$Longitude, lat = arrasian$Latitude,radius = 1, color = "#FFF", opacity = 25)

table(ArrestReport3$ARREST_BORO == 'K', ArrestReport3$OFNS_DESC)

table(ArrestReport$ARREST_PRECINCT ,ArrestReport$OFNS_DESC == "DANGEROUS WEAPONS")

sort(unique(ArrestReport$LAW_CODE))
x<- length(unique(ArrestReport[ArrestReport$LAW_CODE == "PL 26503",]$ARREST_KEY))
#by Precinct
races <- sort(unique(ArrestReport$PERP_RACE))
precinct <- sort(unique(ArrestReport$ARREST_PRECINCT))
precinct_mat <- matrix(NA,length(precinct),15)
for (i in 1:length(precinct)) {
  prec_temp <- precinct[i]
  total_arrest <- length(unique(ArrestReport[ArrestReport$ARREST_PRECINCT==prec_temp,]$ARREST_KEY))
  total_weapons <- length(unique(ArrestReport[ArrestReport$ARREST_PRECINCT==prec_temp & ArrestReport$OFNS_DESC == "DANGEROUS WEAPONS",]$ARREST_KEY))
  total_FelAss <- length(unique(ArrestReport[ArrestReport$ARREST_PRECINCT==prec_temp & ArrestReport$OFNS_DESC == "FELONY ASSAULT",]$ARREST_KEY))
  knownfirearms <- length(unique(ArrestReport[ArrestReport$ARREST_PRECINCT==prec_temp & (ArrestReport$LAW_CODE == "PL 265031B") ,]$ARREST_KEY))
  
  #race
  arrests_AmI_AlNat <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[1] & ArrestReport$ARREST_PRECINCT==prec_temp,]$ARREST_KEY))
  arrests_As_PaIs <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[2] & ArrestReport$ARREST_PRECINCT==prec_temp,]$ARREST_KEY))
  arrests_Bla <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[3] & ArrestReport$ARREST_PRECINCT==prec_temp,]$ARREST_KEY))
  arrests_Bla_His <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[4] & ArrestReport$ARREST_PRECINCT==prec_temp,]$ARREST_KEY))
  arrests_Unk <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[5] & ArrestReport$ARREST_PRECINCT==prec_temp,]$ARREST_KEY))
  arrests_Whi <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[6] & ArrestReport$ARREST_PRECINCT==prec_temp,]$ARREST_KEY))
  arrests_Whi_His <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[7] & ArrestReport$ARREST_PRECINCT==prec_temp,]$ARREST_KEY))
  
  precinct_mat[i,1] <- prec_temp
  precinct_mat[i,2] <- total_arrest
  precinct_mat[i,3] <- total_weapons
  precinct_mat[i,4] <- total_weapons/total_arrest
  precinct_mat[i,5] <- knownfirearms
  precinct_mat[i,6] <- knownfirearms/total_arrest
  precinct_mat[i,7] <- total_FelAss
  precinct_mat[i,8] <- total_FelAss/total_arrest
  precinct_mat[i,9] <- arrests_AmI_AlNat
  precinct_mat[i,10] <- arrests_As_PaIs
  precinct_mat[i,11] <- arrests_Bla
  precinct_mat[i,12] <- arrests_Bla_His
  precinct_mat[i,13] <- arrests_Whi
  precinct_mat[i,14] <- arrests_Whi_His
  precinct_mat[i,15] <- arrests_Unk
  
  
  print(i)
}
colnames(precinct_mat) <- c("Precinct", "Total Arrests", "Weapons Arrests", "Weapons Rate",
                            "Known Firearms","Firearms Rate","Felony Arrests",
                            "Felony Rate","American Indian/Alaskan Native", "Asian/Pacific Islander",
                            "Black", "Black Hispanic", "White", "White Hispanic", "Unknown" )
hist(precinct_mat[,4])
precinct_mat <- as.data.frame(precinct_mat, stringsAsFactors = FALSE)
hist(precinct_mat$`Total Arrests`)
hist(precinct_mat$Precinct)

#race Rate
precinct_mat$`Native Rate` <- precinct_mat$`American Indian/Alaskan Native`/precinct_mat$`Total Arrests`
precinct_mat$`Asian/Islander Rate` <- precinct_mat$`Asian/Pacific Islander`/precinct_mat$`Total Arrests`
precinct_mat$`Black Rate` <- precinct_mat$`Black`/precinct_mat$`Total Arrests`
precinct_mat$`Black Hispanic Rate` <- precinct_mat$`Black Hispanic`/precinct_mat$`Total Arrests`
precinct_mat$`White Rate` <- precinct_mat$`White`/precinct_mat$`Total Arrests`
precinct_mat$`White Hispanic Rate` <- precinct_mat$`White Hispanic`/precinct_mat$`Total Arrests`
precinct_mat$`Unknown Rate` <- precinct_mat$`Unknown`/precinct_mat$`Total Arrests`

write.csv(precinct_mat, "precinct_mat.csv")
precinct_mat <- read.csv("precinct_mat.csv")

fullData <- merge(precinct_mat,y, by.precinct_mat = c("Precinct"), all.precinct_mat = FALSE)
fullData <- merge(fullData,z, by.fullData = c("Precinct"), all.fullData = FALSE)
fullData <- merge(fullData,a, by.fullData = c("Precinct"), all.fullData = TRUE)
fullData <- merge(fullData,b, by.fullData = c("Precinct"), all.fullData = TRUE)
fullData <- merge(fullData,precpoptot, by.fullData = c("Precinct"), all.fullData = TRUE)

fullData[is.na(fullData)] <- 0

fullData$`Hisp Ratio` <- fullData$`Hisp Pop`/fullData$`Total Pop`
fullData$`White Ratio` <- fullData$`White Pop`/fullData$`Total Pop`
fullData$`Black Ratio` <- fullData$`Black Pop`/fullData$`Total Pop`
fullData$`Native Ratio` <- fullData$`Native Pop`/fullData$`Total Pop`
fullData$`Asian Ratio` <- fullData$`Asian Pop`/fullData$`Total Pop`
fullData$`Pacific Islander Ratio` <- fullData$`Pacific Islander Pop`/fullData$`Total Pop`
fullData$`Other Ratio` <- fullData$`Other Pop`/fullData$`Total Pop`


fullData$ComplaintRate <- fullData$Complaints/fullData$Total.Arrests
fullData$OfficerInjuryRate <- fullData$Total.Officer/fullData$Total.Arrests
fullData$ForceRate <- fullData$Total/fullData$Total.Arrests
fullData$SubjectInjuryRate <- fullData$Total.Subject/fullData$Total.Arrests

write.csv(fullData, "fullData.csv")
fullData <- read.csv("fullData.csv")

View(fullData[c(1,2,4,6,8,38,39,40,41)])

      #### Graphs with Race Ratio and Weapons
treat <- ifelse(fullData$`White Ratio` < .5,1,0)
mean(treat)
treat_prec <- fullData$Precinct[treat == 1]

fullData$treat <- ifelse(fullData$Precinct %in% treat_prec,1,0)

hist(fullData$Weapons.Rate)
hist(fullData$White.Ratio)
plot(fullData[fullData$treat == 0,]$`Weapons Rate`,fullData[fullData$treat == 0,]$ForceRate,
     main = "Weapons Rate on Force Rate", sub = "Red = White Ratio < 50%, Bla = White Ratio > 50%", xlab = "Weapons Rate", ylab = "Force Rate")
lines(fullData[fullData$treat == 1,]$`Weapons Rate`,fullData[fullData$treat == 1,]$ForceRate,
      col ="red", type = "p")

lmx <- lm(fullData[fullData$treat == 0,]$ForceRate~fullData[fullData$treat == 0,]$`Weapons Rate`)
lmx2 <- lm(fullData[fullData$treat == 1,]$ForceRate~fullData[fullData$treat == 1,]$`Weapons Rate`)
abline(a = lmx$coefficients[1],b = lmx$coefficients[2])
abline(a = lmx2$coefficients[1],b = lmx2$coefficients[2], col = "red")





lmx <- lm(fullData$Weapons.Rate~fullData$Firearms.Rate)
plot(fullData$Firearms.Rate, fullData$Weapons.Rate, main = "Weapons Rate and Known Firearms Arrest Rate",
     xlab = "Firearms Rate", ylab = "Weapons Rate",)
abline(a = lmx$coefficients[1],b = lmx$coefficients[2])
summary(lmx)

lmx <- lm(fullData$ForceRate~fullData$`Felony Rate`)
lmx2 <- lm(fullData$ForceRate~fullData$`Weapons Rate`)
lmy <- lm(fullData$ForceRate~fullData$`Weapons Rate` + fullData$`Felony Rate`)
summary(lmy)
lmy2 <- lm(fullData$ComplaintRate~fullData$`Weapons Rate` + fullData$`Felony Rate`)
summary(lmy2)
lmy3 <- lm(fullData$OfficerInjuryRate~fullData$`Weapons Rate` + fullData$`Felony Rate`)
summary(lmy3)
lmy4 <- lm(fullData$SubjectInjuryRate~fullData$`Weapons Rate` + fullData$`Felony Rate`)
summary(lmy4)
plot(fullData$`Felony Rate`,fullData$ForceRate, main = "Felonies and Use of Force", xlab= "Felony Rate", ylab = "Use of Force")
abline(a = lmx$coefficients[1],b = lmx$coefficients[2])

#Regression
lm1 <- lm(fullData$Complaints~fullData$`Weapons Rate`)
lm2 <- lm(fullData$Total.Subject~fullData$`Weapons Rate`)
lm3 <- lm(fullData$Total.Officer~fullData$`Weapons Rate`)
lm4 <- lm(fullData$Total~fullData$`Weapons Rate`)



plot(fullData$`Weapons Rate`,fullData$Total)
plot(fullData$`Weapons Rate`,fullData$Total.Officer)
plot(fullData$Weapons.Rate,fullData$Complaints,main = ' Weapons and Police Agression',
        sub = 'red = sub.Inj, bla = Compl., blu = Force.use, gre = off.Inj',
     xlab = 'Weapons Rate', ylab = 'Police Behavior')
abline(a = lm1$coefficients[1],b = lm1$coefficients[2])
lines(fullData$`Weapons Rate`,fullData$Total.Subject,type = "p",col = "red")
abline(a = lm2$coefficients[1],b = lm2$coefficients[2], col = "red")
lines(fullData$`Weapons Rate`,fullData$Total,type = "p",col = "blue")
abline(a = lm4$coefficients[1],b = lm4$coefficients[2], col = "blue")
lines(fullData$`Weapons Rate`,fullData$Total.Officer,type = "p",col = "green")
abline(a = lm3$coefficients[1],b = lm3$coefficients[2], col = "green")

##Race Ratio on Stuff
plot(fullData$`Black Ratio`, fullData$ForceRate)
plot(fullData$`Black Ratio`, fullData$`Weapons Rate`)
lmx <- lm(fullData$ForceRate ~ fullData$`Black Ratio` + fullData$`Weapons Rate`)
summary(lmx)
plot(fullData$`Black Ratio`, fullData$`Black Rate`)
lmx <- lm(fullData$`Black Rate` ~ fullData$`Black Ratio`)
summary(lmx)


#Rate Regression
lm5 <- lm(fullData$ComplaintRate~fullData$Weapons.Rate)
lm6 <- lm(fullData$SubjectInjuryRate~fullData$Weapons.Rate)
lm7 <- lm(fullData$OfficerInjuryRate~fullData$Weapons.Rate)
lm8 <- lm(fullData$ForceRate~fullData$Weapons.Rate)

summary(lm5)
summary(lm6)
summary(lm7)
summary(lm8)

plot(fullData$Weapons.Rate,fullData$ComplaintRate,
     xlab = 'Weapons Arrest Rate', ylab = 'Measures for Police Agression')
abline(a = lm5$coefficients[1],b = lm5$coefficients[2])
lines(fullData$Weapons.Rate,fullData$SubjectInjuryRate,type = "p",col = "red", pch = 0)
abline(a = lm6$coefficients[1],b = lm6$coefficients[2], col = "red")
lines(fullData$Weapons.Rate,fullData$ForceRate,type = "p",col = "blue", pch = 2,)
abline(a = lm8$coefficients[1],b = lm8$coefficients[2], col = "blue")
lines(fullData$Weapons.Rate,fullData$OfficerInjuryRate,type = "p",col = "green", pch = 9)
abline(a = lm7$coefficients[1],b = lm7$coefficients[2], col = "green")


#Other correlations
plot(fullData$ForceRate,fullData$OfficerInjuryRate, main = 'Use of Force and Injury', 
        xlab = 'Rate of Force', ylab = 'Injury Rate', sub = 'red = subjects, bla = officers')
lines(fullData$ForceRate,fullData$SubjectInjuryRate, type = "p",col = "red")

plot(fullData$SubjectInjuryRate,fullData$ComplaintRate, main = 'Injury and Complaints', 
     xlab = 'Injury Rate', ylab = 'Complaint Rate')

plot(fullData$ForceRate,fullData$ComplaintRate, main = 'Force and Complaints', 
     xlab = 'Rate of Force', ylab = 'Complaint Rate')


lm9 <- lm(fullData$ForceRate~fullData$`Total Arrests`)
plot(fullData$`Total Arrests`,fullData$ForceRate, main = 'Force Rate and Arrests', 
     xlab = 'Arrests', ylab = 'Force Rate')
abline(a = lm9$coefficients[1],b = lm9$coefficients[2])

lm10 <- lm(fullData$ForceRate~fullData$`Black Rate`)
plot(fullData$`Black Rate`,fullData$ForceRate, main = 'Black Arrests and Use of Force',
     xlab = 'Percentage of Black Arrests', ylab = 'Force Rate')
abline(a = lm10$coefficients[1],b = lm10$coefficients[2])

lm11 <- lm(fullData$`Weapons Rate`~fullData$`Black Rate`)
lm12 <- lm(fullData$`Weapons Rate`~fullData$`White Rate`)
plot(fullData$`Black Rate`,fullData$`Weapons Rate`, main = 'Black Arrests and Weapons Arrest',
     xlab = 'Percentage of Black Arrests', ylab = 'Weapons Rate')
abline(a = lm11$coefficients[1],b = lm11$coefficients[2])



# guns,police, and black arrests
lm13 <- lm(fullData$ForceRate ~ fullData$`Weapons Rate` + fullData$`Black Rate`)
summary(lm13)
summary(lm10)
summary(lm8)

#by Crime
races <- sort(unique(ArrestReport$PERP_RACE))
for (i in 1:length(races)){
  race <- races[i]
  print(race)
}
crime <- sort(unique(ArrestReport$LAW_CODE))
crime_mat <- matrix(NA,length(crime),8)
for (i in 1:length(crime)) {
  crime_temp <- crime[i]
  arrests_AmI_AlNat <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[1] & ArrestReport$LAW_CODE==crime_temp,]$ARREST_KEY))
  arrests_As_PaIs <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[2] & ArrestReport$LAW_CODE==crime_temp,]$ARREST_KEY))
  arrests_Bla <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[3] & ArrestReport$LAW_CODE==crime_temp,]$ARREST_KEY))
  arrests_Bla_His <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[4] & ArrestReport$LAW_CODE==crime_temp,]$ARREST_KEY))
  arrests_Unk <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[5] & ArrestReport$LAW_CODE==crime_temp,]$ARREST_KEY))
  arrests_Whi <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[6] & ArrestReport$LAW_CODE==crime_temp,]$ARREST_KEY))
  arrests_Whi_His <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[7] & ArrestReport$LAW_CODE==crime_temp,]$ARREST_KEY))
  total_arrests_by_crime <- length(unique(ArrestReport[ArrestReport$LAW_CODE==crime_temp,]$ARREST_KEY))
  crime_mat[i,1] <- total_arrests_by_crime
  crime_mat[i,2] <- arrests_AmI_AlNat
  crime_mat[i,3] <- arrests_As_PaIs
  crime_mat[i,4] <- arrests_Bla
  crime_mat[i,5] <- arrests_Bla_His
  crime_mat[i,6] <- arrests_Whi
  crime_mat[i,7] <- arrests_Whi_His
  crime_mat[i,8] <- arrests_Unk
  print(i)
}
rownames(crime_mat) <- crime
colnames(crime_mat) <- c("Total Arrests", "American Indian/Alaskan Native", "Asian/Pacific Islander",
                         "Black", "Black Hispanic", "White", "White Hispanic", "Unknown")

crime_mat <- as.data.frame(crime_mat, stringsAsFactors = FALSE)


crime_mat$`Native Rate` <- crime_mat$`American Indian/Alaskan Native`/crime_mat$`Total Arrests`
crime_mat$`Asian/Islander Rate` <- crime_mat$`Asian/Pacific Islander`/crime_mat$`Total Arrests`
crime_mat$`Black Rate` <- crime_mat$`Black`/crime_mat$`Total Arrests`
crime_mat$`Black Hispanic Rate` <- crime_mat$`Black Hispanic`/crime_mat$`Total Arrests`
crime_mat$`White Rate` <- crime_mat$`White`/crime_mat$`Total Arrests`
crime_mat$`White Hispanic Rate` <- crime_mat$`White Hispanic`/crime_mat$`Total Arrests`
crime_mat$`Unknown Rate` <- crime_mat$`Unknown`/crime_mat$`Total Arrests`

xxxx <- ArrestReport[!duplicated(ArrestReport$LAW_CODE),]
ArrDisc <- xxxx[,c(4,6,7,8)]
ArrDisc <- ArrDisc[order(ArrDisc$LAW_CODE),]
crime_mat$`PD Description` <- ArrDisc[,1]
crime_mat$`Offense Description` <- ArrDisc[,3]
crime_mat$`Law Code` <- ArrDisc[,4]
crime_mat$`Crime Rate` <- crime_mat$`Total Arrests`/sum(crime_mat$`Total Arrests`)

crime_mat$pd

View(crime_mat[,c(17,16,1,18,9,10,11,12,13,14,15,2,3,4,5,6,7,8)])
write.csv(crime_mat, "crime_mat.csv")
crime_mat <- read.csv("crime_mat.csv")
ArrestReport <- read.csv("ArrestReport.csv")

weaponcrime_mat <- crime_mat[crime_mat$Offense.Description == "DANGEROUS WEAPONS",]
weaponcrime_mat$Crime.Type.Rate <- weaponcrime_mat$Total.Arrests/sum(weaponcrime_mat$Total.Arrests)
write.csv(weaponcrime_mat,"weaponcrime.csv")
weaponcrime <- read.csv("weaponcrime.csv")

# by Offense Discription
crimetype <- sort(unique(ArrestReport$OFNS_DESC))
crimetype_mat <- matrix(NA,length(crimetype),8)
for (i in 1:length(crimetype)) {
  crimetype_temp <- crimetype[i]
  arrests_AmI_AlNat <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[1] & ArrestReport$OFNS_DESC==crimetype_temp,]$ARREST_KEY))
  arrests_As_PaIs <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[2] & ArrestReport$OFNS_DESC==crimetype_temp,]$ARREST_KEY))
  arrests_Bla <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[3] & ArrestReport$OFNS_DESC==crimetype_temp,]$ARREST_KEY))
  arrests_Bla_His <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[4] & ArrestReport$OFNS_DESC==crimetype_temp,]$ARREST_KEY))
  arrests_Unk <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[5] & ArrestReport$OFNS_DESC==crimetype_temp,]$ARREST_KEY))
  arrests_Whi <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[6] & ArrestReport$OFNS_DESC==crimetype_temp,]$ARREST_KEY))
  arrests_Whi_His <- length(unique(ArrestReport[ArrestReport$PERP_RACE==races[7] & ArrestReport$OFNS_DESC==crimetype_temp,]$ARREST_KEY))
  total_arrests_by_crimetype <- length(unique(ArrestReport[ArrestReport$OFNS_DESC==crimetype_temp,]$ARREST_KEY))
  crimetype_mat[i,1] <- total_arrests_by_crimetype
  crimetype_mat[i,2] <- arrests_AmI_AlNat
  crimetype_mat[i,3] <- arrests_As_PaIs
  crimetype_mat[i,4] <- arrests_Bla
  crimetype_mat[i,5] <- arrests_Bla_His
  crimetype_mat[i,6] <- arrests_Whi
  crimetype_mat[i,7] <- arrests_Whi_His
  crimetype_mat[i,8] <- arrests_Unk
  print(i)
}
rownames(crimetype_mat) <- crimetype
colnames(crimetype_mat) <- c("Total Arrests", "American Indian/Alaskan Native", "Asian/Pacific Islander",
                         "Black", "Black Hispanic", "White", "White Hispanic", "Unknown")

crimetype_mat <- as.data.frame(crimetype_mat, stringsAsFactors = FALSE)

crimetype_mat$`Native Rate` <- crimetype_mat$`American Indian/Alaskan Native`/crimetype_mat$`Total Arrests`
crimetype_mat$`Asian/Islander Rate` <- crimetype_mat$`Asian/Pacific Islander`/crimetype_mat$`Total Arrests`
crimetype_mat$`Black Rate` <- crimetype_mat$`Black`/crimetype_mat$`Total Arrests`
crimetype_mat$`Black Hispanic Rate` <- crimetype_mat$`Black Hispanic`/crimetype_mat$`Total Arrests`
crimetype_mat$`White Rate` <- crimetype_mat$`White`/crimetype_mat$`Total Arrests`
crimetype_mat$`White Hispanic Rate` <- crimetype_mat$`White Hispanic`/crimetype_mat$`Total Arrests`
crimetype_mat$`Unknown Rate` <- crimetype_mat$`Unknown`/crimetype_mat$`Total Arrests`
crimetype_mat$`Crime Rate` <- crimetype_mat$`Total Arrests`/sum(crimetype_mat$`Total Arrests`)
write.csv(crimetype, "crimetype.csv")
crimetype <- read.csv("crimetype.csv")
View(crimetype_mat[c(1,16,9,10,11,12,13,14,15)])

#Playing with Full Data set
FullArrRepo <- read.csv("FullArrRepo.csv", as.is = TRUE)

FullArrRepo$year <- as.numeric(substr(FullArrRepo$ARREST_DATE,7,11))
unique(FullArrRepo$year)

races <- sort(unique(FullArrRepo$PERP_RACE))
years <- sort(unique(FullArrRepo$year))
fullprecinct <- sort(unique(FullArrRepo$ARREST_PRECINCT))
fullprecinct_mat <- matrix(NA,length(fullprecinct)*length(years),18)
k <- 1
for(i in 1:length(years)){
  year_temp <- years[i]
  for (j in 1:length(fullprecinct)) {
    prec_temp <- fullprecinct[j]
    total_arrest <- length(unique(FullArrRepo[FullArrRepo$ARREST_PRECINCT==prec_temp &
                                                FullArrRepo$year == year_temp,]$ARREST_KEY))
    total_weapons <- length(unique(FullArrRepo[FullArrRepo$ARREST_PRECINCT==prec_temp & FullArrRepo$OFNS_DESC == "DANGEROUS WEAPONS" &
                                                 FullArrRepo$year == year_temp,]$ARREST_KEY))
    total_FelAss <- length(unique(FullArrRepo[FullArrRepo$ARREST_PRECINCT==prec_temp & FullArrRepo$OFNS_DESC == "FELONY ASSAULT" &
                                                FullArrRepo$year == year_temp,]$ARREST_KEY))
    
    #race
    arrests_AmI_AlNat <- length(unique(FullArrRepo[FullArrRepo$PERP_RACE==races[1] &
                                                     FullArrRepo$ARREST_PRECINCT==prec_temp &
                                                     FullArrRepo$year == year_temp,]$ARREST_KEY))
    arrests_As_PaIs <- length(unique(FullArrRepo[FullArrRepo$PERP_RACE==races[2] &
                                                   FullArrRepo$ARREST_PRECINCT==prec_temp &
                                                   FullArrRepo$year == year_temp,]$ARREST_KEY))
    arrests_Bla <- length(unique(FullArrRepo[FullArrRepo$PERP_RACE==races[3] &
                                               FullArrRepo$ARREST_PRECINCT==prec_temp &
                                               FullArrRepo$year == year_temp,]$ARREST_KEY))
    arrests_Bla_His <- length(unique(FullArrRepo[FullArrRepo$PERP_RACE==races[4] &
                                                   FullArrRepo$ARREST_PRECINCT==prec_temp &
                                                   FullArrRepo$year == year_temp,]$ARREST_KEY))
    arrests_Unk <- length(unique(FullArrRepo[FullArrRepo$PERP_RACE==races[5] &
                                               FullArrRepo$ARREST_PRECINCT==prec_temp &
                                               FullArrRepo$year == year_temp,]$ARREST_KEY))
    arrests_Whi <- length(unique(FullArrRepo[FullArrRepo$PERP_RACE==races[6] &
                                               FullArrRepo$ARREST_PRECINCT==prec_temp &
                                               FullArrRepo$year == year_temp,]$ARREST_KEY))
    arrests_Whi_His <- length(unique(FullArrRepo[FullArrRepo$PERP_RACE==races[7] &
                                               FullArrRepo$ARREST_PRECINCT==prec_temp &
                                                 FullArrRepo$year == year_temp,]$ARREST_KEY))
    total_pot <- length(unique(FullArrRepo[FullArrRepo$year == year_temp & FullArrRepo$ARREST_PRECINCT==prec_temp &
                                             (FullArrRepo$PD_DESC == "MARIJUANA, POSSESSION 4 & 5" |
                                                FullArrRepo$PD_DESC == "MARIJUANA, SALE 3 & 4"),]$ARREST_KEY))
    total_drugs <- length(unique(FullArrRepo[FullArrRepo$year == year_temp & FullArrRepo$ARREST_PRECINCT==prec_temp &
                                               (FullArrRepo$OFNS_DESC == "DANGEROUS DRUGS"),]$ARREST_KEY))
    
    
    fullprecinct_mat[k,1] <- year_temp
    fullprecinct_mat[k,2] <- prec_temp
    fullprecinct_mat[k,3] <- total_arrest
    fullprecinct_mat[k,4] <- total_weapons
    fullprecinct_mat[k,5] <- total_weapons/total_arrest
    fullprecinct_mat[k,6] <- total_FelAss
    fullprecinct_mat[k,7] <- total_FelAss/total_arrest
    fullprecinct_mat[k,8] <- arrests_AmI_AlNat
    fullprecinct_mat[k,9] <- arrests_As_PaIs
    fullprecinct_mat[k,10] <- arrests_Bla
    fullprecinct_mat[k,11] <- arrests_Bla_His
    fullprecinct_mat[k,12] <- arrests_Whi
    fullprecinct_mat[k,13] <- arrests_Whi_His
    fullprecinct_mat[k,14] <- arrests_Unk
    fullprecinct_mat[k,15] <- total_pot
    fullprecinct_mat[k,16] <- total_pot/total_arrest
    fullprecinct_mat[k,17] <- total_drugs
    fullprecinct_mat[k,18] <- total_drugs/total_arrest
    
    k <- k+1
    print(k)
    print(i)
  }
}
fullprecinct_mat <- as.data.frame(fullprecinct_mat, stringsAsFactors = FALSE)
colnames(fullprecinct_mat) <- c("Year", "Precinct", "Total Arrests", "Weapons Arrests", "Weapons Rate","Felony Assault","Felony Rate","American Indian/Alaskan Native", "Asian/Pacific Islander",
                            "Black", "Black Hispanic", "White", "White Hispanic", "Unknown", "Total Pot", "Pot Rate", "Total Drugs", "Drug Rate" )

#race Rate
fullprecinct_mat$`Native Rate` <- fullprecinct_mat$`American Indian/Alaskan Native`/fullprecinct_mat$`Total Arrests`
fullprecinct_mat$`Asian/Islander Rate` <- fullprecinct_mat$`Asian/Pacific Islander`/fullprecinct_mat$`Total Arrests`
fullprecinct_mat$`Black Rate` <- fullprecinct_mat$`Black`/fullprecinct_mat$`Total Arrests`
fullprecinct_mat$`Black Hispanic Rate` <- fullprecinct_mat$`Black Hispanic`/fullprecinct_mat$`Total Arrests`
fullprecinct_mat$`White Rate` <- fullprecinct_mat$`White`/fullprecinct_mat$`Total Arrests`
fullprecinct_mat$`White Hispanic Rate` <- fullprecinct_mat$`White Hispanic`/fullprecinct_mat$`Total Arrests`
fullprecinct_mat$`Unknown Rate` <- fullprecinct_mat$`Unknown`/fullprecinct_mat$`Total Arrests`
fullprecinct_mat <- merge.data.frame(fullprecinct_mat,precpoptot, by.fullprecinct_mat = "Precinct")
fullprecinct_mat$`Hisp Ratio` <- fullprecinct_mat$`Hisp Pop`/fullprecinct_mat$`Total Pop`
fullprecinct_mat$`White Ratio` <- fullprecinct_mat$`White Pop`/fullprecinct_mat$`Total Pop`
fullprecinct_mat$`Black Ratio` <- fullprecinct_mat$`Black Pop`/fullprecinct_mat$`Total Pop`
fullprecinct_mat$`Native Ratio` <- fullprecinct_mat$`Native Pop`/fullprecinct_mat$`Total Pop`
fullprecinct_mat$`Asian Ratio` <- fullprecinct_mat$`Asian Pop`/fullprecinct_mat$`Total Pop`
fullprecinct_mat$`Pacific Islander Ratio` <- fullprecinct_mat$`Pacific Islander Pop`/fullprecinct_mat$`Total Pop`
fullprecinct_mat$`Other Ratio` <- fullprecinct_mat$`Other Pop`/fullprecinct_mat$`Total Pop`
fullprecinct_mat$`ArrestRate` <- fullprecinct_mat$`Total Arrests`/fullprecinct_mat$`Total Pop`

write.csv(fullprecinct_mat, "fullprecinct_mat.csv")
fullprecinct_mat <- read.csv("fullprecinct_mat.csv")

plot(fullprecinct_mat[fullprecinct_mat$Precinct != 22 & fullprecinct_mat$Year == 2010,]$Pot.Rate,fullprecinct_mat[fullprecinct_mat$Precinct != 22 & fullprecinct_mat$Year == 2010,]$Arrest.Rate)


g <- length(unique(FullArrRepo[FullArrRepo$OFNS_DESC == 2651900]$ARREST_KEY))

lmx <- lm(fullprecinct_mat$`Felony Rate` ~ fullprecinct_mat$`Weapons Rate`)
lmx2 <- lm(fullprecinct_mat$`Felony Rate` ~ fullprecinct_mat$`Weapons Rate` -1)
summary(lmx)
summary(lmx2)

plot(fullprecinct_mat$`Weapons Rate`,fullprecinct_mat$`Felony Rate`, main = "Weapons and Felonies", xlab = "Weapons Rate", ylab = "Felony Rate")
abline(a = lmx$coefficients[1],b = lmx$coefficients[2])
abline(a = 0, b = lmx2$coefficients[1], col="red")


FullArrRepo <- read.csv("FullArrRepo.csv")
# by Year
years <- sort(unique(FullArrRepo$year))
crimes <- sort(unique(FullArrRepo$PD_DESC))
yearnguns <- matrix(NA,length(years),6)
i <- 1
for (i in 1:length(years)){
  year_temp <- years[i]
  total_weapons <- length(unique(FullArrRepo[FullArrRepo$OFNS_DESC == "DANGEROUS WEAPONS" &
                        FullArrRepo$year == year_temp,]$ARREST_KEY))
  total_arrest <- length(unique(FullArrRepo[FullArrRepo$year == year_temp,]$ARREST_KEY))
  total_pot <- length(unique(FullArrRepo[FullArrRepo$year == year_temp & 
                                           (FullArrRepo$PD_DESC == "MARIJUANA, POSSESSION 4 & 5" |
                                              FullArrRepo$PD_DESC == "MARIJUANA, SALE 3 & 4"),]$ARREST_KEY))
  
  
  yearnguns[i,1] <- year_temp
  yearnguns[i,2] <- total_weapons
  yearnguns[i,3] <- total_arrest
  yearnguns[i,4] <- total_weapons/total_arrest
  yearnguns[i,5] <- total_pot
  yearnguns[i,6] <- total_pot/total_arrest
  
  print(i)
}
colnames(yearnguns) <- c("Year", "Total Weapons", "Total Arrests", "Weapons Rate", "Pot Arrests", "Pot Rate")
yearnguns <- as.data.frame(yearnguns, stringsAsFactors = FALSE)

write.csv(yearnguns,"yearnguns.csv")
yearnguns <- read.csv("yearnguns.csv")

plot(yearnguns$Year,yearnguns$Weapons.Rate,xlim = c(2006,2019), type = 'l', ylim = c(0.01,.045), main = "Weapons Rate Per Year", xlab = "Year", ylab = "Weapons Rate")
plot(yearnguns$Year,yearnguns$Total.Weapons,xlim = c(2006,2019), type = 'l', main = "Weapons Arrests Per Year", xlab = "Year", ylab = "Weapons Arrests")
plot(yearnguns$Year,yearnguns$Total.Arrests,xlim = c(2006,2019), type = 'l',
     main = "Total Arrests Per Year", xlab = "Year", ylab = "Total Arrests",)
plot(yearnguns$Year,yearnguns$Pot.Arrests,xlim = c(2006,2019), type = 'l',
     main = "Total Arrests Per Year", xlab = "Year", ylab = "Total Arrests",)
lines(yearnguns$Year,yearnguns$Total.Arrests/10, type = "l",col = "red")

plot(yearnguns$Year,yearnguns$Pot.Rate,xlim = c(2006,2019), type = 'l', main = "Pot Rate Per Year", xlab = "Year", ylab = "Pot Rate")

p_plot <- xyplot(yearnguns$Pot.Arrests ~ yearnguns$Year, type = "l", col = "red")

t_plot <- xyplot(yearnguns$Total.Arrests ~ yearnguns$Year, type = "l", col = "blue")

doubleYScale(p_plot,t_plot, add.ylab2 = TRUE, use.style = FALSE)

##Historic Shootings

precinctshootings$year <- as.numeric(substr(precinctshootings$OCCUR_DATE,7,11))
unique(precinctshootings$year)

races <- sort(unique(precinctshootings$PERP_RACE))
years <- sort(unique(precinctshootings$year))
fullprecshoot <- sort(unique(precinctshootings$PRECINCT))
fullprecshoot_mat <- matrix(NA,length(fullprecshoot)*length(years),2)
k <- 1
for(i in 1:length(years)){
  year_temp <- years[i]
  for (j in 1:length(fullprecshoot)) {
    prec_temp <- fullprecshoot[j]
    total_shootings <- length(unique(precinctshootings[precinctshootings$PRECINCT == prec_temp & precinctshootings$year == year_temp,]$INCIDENT_KEY))
        
    fullprecshoot_mat[j,1] <- prec_temp
    fullprecshoot_mat[j,2] <- total_shootings

    
    print(j)
    }
  print(i)
}
fullprecshoot_mat <- as.data.frame(fullprecshoot_mat, stringsAsFactors = FALSE)
colnames(fullprecshoot_mat) <- c("Precinct","Shootings")

fullprecinct_mat <- merge.data.frame(fullprecinct_mat,fullprecshoot_mat,by.fullprecinct_mat = "Precinct")
fullprecinct_mat$`Shoot Per Pop` <- fullprecinct_mat$Shootings/fullprecinct_mat$`Total Pop`

fullprecinct_mat[is.na(fullprecinct_mat$Shootings)] <- 0
write.csv(fullprecinct_mat, "fullprecinct_mat.csv")
fullprecinct_mat <- read.csv("fullprecinct_mat.csv")

lmx <- lm(fullprecinct_mat$`Shoot Per Pop`~fullprecinct_mat$`Weapons Rate`)
summary(lmx)
plot(fullprecinct_mat$`Weapons Rate`,fullprecinct_mat$`Shoot Per Pop`)

#2020
CurrArrRepo$year <- as.numeric(substr(CurrArrRepo$ARREST_DATE,7,11))
CurrArrRepo$month <- as.numeric(substr(CurrArrRepo$ARREST_DATE,1,2))
unique(CurrArrRepo$year)
unique(CurrArrRepo$month)

races <- sort(unique(CurrArrRepo$PERP_RACE))
years <- sort(unique(CurrArrRepo$year))
months <- sort(unique(CurrArrRepo$month))
currprecinct <- sort(unique(CurrArrRepo$ARREST_PRECINCT))
currprecinct_mat <- matrix(NA,length(currprecinct)*length(months),15)
k <- 1
for(i in 1:length(months)){
  month_temp <- months[i]
  for (j in 1:length(currprecinct)) {
    prec_temp <- currprecinct[j]
    total_arrest <- length(unique(CurrArrRepo[CurrArrRepo$ARREST_PRECINCT==prec_temp &
                                                CurrArrRepo$month == month_temp,]$ARREST_KEY))
    total_weapons <- length(unique(CurrArrRepo[CurrArrRepo$ARREST_PRECINCT==prec_temp & CurrArrRepo$OFNS_DESC == "DANGEROUS WEAPONS" &
                                                 CurrArrRepo$month == month_temp,]$ARREST_KEY))
    total_FelAss <- length(unique(CurrArrRepo[CurrArrRepo$ARREST_PRECINCT==prec_temp & CurrArrRepo$OFNS_DESC == "FELONY ASSAULT" &
                                                CurrArrRepo$month == month_temp,]$ARREST_KEY))
    
    #race
    arrests_AmI_AlNat <- length(unique(CurrArrRepo[CurrArrRepo$PERP_RACE==races[1] &
                                                     CurrArrRepo$ARREST_PRECINCT==prec_temp &
                                                     CurrArrRepo$month == month_temp,]$ARREST_KEY))
    arrests_As_PaIs <- length(unique(CurrArrRepo[CurrArrRepo$PERP_RACE==races[2] &
                                                   CurrArrRepo$ARREST_PRECINCT==prec_temp &
                                                   CurrArrRepo$month == month_temp,]$ARREST_KEY))
    arrests_Bla <- length(unique(CurrArrRepo[CurrArrRepo$PERP_RACE==races[3] &
                                               CurrArrRepo$ARREST_PRECINCT==prec_temp &
                                               CurrArrRepo$month == month_temp,]$ARREST_KEY))
    arrests_Bla_His <- length(unique(CurrArrRepo[CurrArrRepo$PERP_RACE==races[4] &
                                                   CurrArrRepo$ARREST_PRECINCT==prec_temp &
                                                   CurrArrRepo$month == month_temp,]$ARREST_KEY))
    arrests_Unk <- length(unique(CurrArrRepo[CurrArrRepo$PERP_RACE==races[5] &
                                               CurrArrRepo$ARREST_PRECINCT==prec_temp &
                                               CurrArrRepo$month == month_temp,]$ARREST_KEY))
    arrests_Whi <- length(unique(CurrArrRepo[CurrArrRepo$PERP_RACE==races[6] &
                                               CurrArrRepo$ARREST_PRECINCT==prec_temp &
                                               CurrArrRepo$month == month_temp,]$ARREST_KEY))
    arrests_Whi_His <- length(unique(CurrArrRepo[CurrArrRepo$PERP_RACE==races[7] &
                                                   CurrArrRepo$ARREST_PRECINCT==prec_temp &
                                                   CurrArrRepo$month == month_temp,]$ARREST_KEY))
    
    currprecinct_mat[k,1] <- year_temp
    currprecinct_mat[k,2] <- month_temp
    currprecinct_mat[k,3] <- prec_temp
    currprecinct_mat[k,4] <- total_arrest
    currprecinct_mat[k,5] <- total_weapons
    currprecinct_mat[k,6] <- total_weapons/total_arrest
    currprecinct_mat[k,7] <- total_FelAss
    currprecinct_mat[k,8] <- total_FelAss/total_arrest
    currprecinct_mat[k,9] <- arrests_AmI_AlNat
    currprecinct_mat[k,10] <- arrests_As_PaIs
    currprecinct_mat[k,11] <- arrests_Bla
    currprecinct_mat[k,12] <- arrests_Bla_His
    currprecinct_mat[k,13] <- arrests_Whi
    currprecinct_mat[k,14] <- arrests_Whi_His
    currprecinct_mat[k,15] <- arrests_Unk
    
    
    k <- k+1
    print(j)
    print(i)
  }
}
currprecinct_mat <- as.data.frame(currprecinct_mat, stringsAsFactors = FALSE)
colnames(currprecinct_mat) <- c("Year","Month", "Precinct", "Total Arrests", "Weapons Arrests", "Weapons Rate","Felony Assault","Felony Rate","American Indian/Alaskan Native", "Asian/Pacific Islander",
                                "Black", "Black Hispanic", "White", "White Hispanic", "Unknown" )

#race Rate
currprecinct_mat$`Native Rate` <- currprecinct_mat$`American Indian/Alaskan Native`/currprecinct_mat$`Total Arrests`
currprecinct_mat$`Asian/Islander Rate` <- currprecinct_mat$`Asian/Pacific Islander`/currprecinct_mat$`Total Arrests`
currprecinct_mat$`Black Rate` <- currprecinct_mat$`Black`/currprecinct_mat$`Total Arrests`
currprecinct_mat$`Black Hispanic Rate` <- currprecinct_mat$`Black Hispanic`/currprecinct_mat$`Total Arrests`
currprecinct_mat$`White Rate` <- currprecinct_mat$`White`/currprecinct_mat$`Total Arrests`
currprecinct_mat$`White Hispanic Rate` <- currprecinct_mat$`White Hispanic`/currprecinct_mat$`Total Arrests`
currprecinct_mat$`Unknown Rate` <- currprecinct_mat$`Unknown`/currprecinct_mat$`Total Arrests`

write.csv(currprecinct_mat, "currprecinct_mat.csv")
currprecinct_mat <- read.csv("currprecinct_mat.csv")

treat <- ifelse(currprecinct_mat[currprecinct_mat$Month == 1,]$`Weapons Rate` > .04,1,0)
mean(treat)
treat_prec <- currprecinct_mat[currprecinct_mat$Month==1,]$Precinct[treat == 1]

currprecinct_mat$treat <- ifelse(currprecinct_mat$Precinct %in% treat_prec,1,0)

hist(currprecinct_mat$`Weapons Rate`)
plot(currprecinct_mat[currprecinct_mat$treat == 0,]$Month,currprecinct_mat[currprecinct_mat$treat == 0,]$`Felony Assault`)
lines(currprecinct_mat[currprecinct_mat$treat == 1,]$Month,currprecinct_mat[currprecinct_mat$treat == 1,]$`Felony Assault`, col ="red", type = "p")

lmx <- lm(currprecinct_mat[currprecinct_mat$treat == 0,]$`Felony Assault`~currprecinct_mat[currprecinct_mat$treat == 0,]$Month)
lmx2 <- lm(currprecinct_mat[currprecinct_mat$treat == 1,]$`Felony Assault`~currprecinct_mat[currprecinct_mat$treat == 1,]$Month)
abline(a = lmx$coefficients[1],b = lmx$coefficients[2])
abline(a = lmx2$coefficients[1],b = lmx2$coefficients[2], col = "red")


### more force report years
setwd("~/GunStatsAnalysis")

fullprecinct_mat <- read.csv("fullprecinct_mat.csv") 
fullprecinct3_mat <- merge(fullforcereport,fullprecinct_mat, by = c("Year", "Precinct"), all = FALSE)
fullprecinct3_mat$`Force Rate` <- fullprecinct3_mat$Force.incidents/fullprecinct3_mat$Total.Arrests
fullprecinct3_mat$ArrestRate <- fullprecinct3_mat$Total.Arrests/fullprecinct3_mat$Total.Pop

write.csv(fullprecinct3_mat,"fullprecinct3_mat.csv")
fullprecinct3_mat <- read.csv("fullprecinct3_mat.csv")

length(unique(fullprecinct3_mat[fullprecinct3_mat$White.Ratio > .5,]$Precinct))

plot(fullprecinct3_mat$Pot.Rate,fullprecinct3_mat$Force.Rate)

plot(fullprecinct3_mat$Weapons.Rate,fullprecinct3_mat$Force.Rate, 
     main = "Weapons and Force 3 years", xlab = "Weapons Rate", ylab = "Force Rate")
legend("bottomleft", c("Weapons Rate"), col = 1)
lmwf <- lm(Force.Rate ~ Weapons.Rate, data = fullprecinct3_mat)
summary(lmwf)
abline(a = lmwf$coefficients[1],b = lmwf$coefficients[2])
lmwf2 <- lm(Force.Rate ~ Weapons.Rate + as.factor(Year), data = fullprecinct3_mat)
summary(lmwf2)
lmwf3 <- lm(Force.Rate ~ Weapons.Rate + as.factor(Year) + as.factor(Precinct), data = fullprecinct3_mat)
summary(lmwf3)

lmbf <- lm(Force.Rate ~ Black.Rate, data = fullprecinct3_mat)
summary(lmbf)

lmbw <- lm(Weapons.Rate ~ Black.Rate, data = fullprecinct3_mat)
summary(lmbw)

lmbwf <- lm(Force.Rate ~ Black.Rate + Weapons.Rate + as.factor(Year), data = fullprecinct3_mat)
summary(lmbwf)



plot(fullprecinct3_mat[fullprecinct3_mat$Precinct != 14 & fullprecinct3_mat$Precinct != 22,]$ArrestRate, fullprecinct3_mat[fullprecinct3_mat$Precinct != 14 & fullprecinct3_mat$Precinct != 22,]$Force.Rate, main = "Arrest Rate and Force Rate", xlab = "Arrest Rate", ylab = "Force Rate")
lmx <- lm(Force.Rate ~ ArrestRate, data = fullprecinct3_mat[fullprecinct3_mat$Precinct != 14 & fullprecinct3_mat$Precinct != 22,])
summary(lmx)
abline(a = lmx$coefficients[1],b = lmx$coefficients[2])

fullprecinct_mat$ArrestRate <- fullprecinct_mat$Total.Arrests/fullprecinct_mat$Total.Pop
plot(fullprecinct_mat[fullprecinct_mat$Year == 2010,]$White.Ratio, 
     fullprecinct_mat[fullprecinct_mat$Year == 2010,]$ArrestRate,
     main = "White Ratio and Total Rate of Arrests", xlab = "white Population Ratio", ylab = "Arrest Rate")
lmx <- lm(ArrestRate ~ White.Ratio, data = fullprecinct_mat)
summary(lmx)
abline(a = lmx$coefficients[1],b = lmx$coefficients[2])

plot(fullprecinct_mat[fullprecinct_mat$Year == 2010,]$White.Ratio, 
     fullprecinct_mat[fullprecinct_mat$Year == 2010,]$Weapons.Rate,
     main = "White Ratio and Weapon Rate of Arrests", xlab = "white Population Ratio", ylab = "Arrest Rate")
lmx <- lm(Weapons.Rate ~ White.Ratio, data = fullprecinct_mat)
summary(lmx)
abline(a = lmx$coefficients[1],b = lmx$coefficients[2])


       
stargazer( lmbw, lmbf,lmwf, lmbwf, type = "text")

plot(fullprecinct3_mat$White.Ratio, fullprecinct3_mat$Total.Pop)
plot(fullprecinct_mat$Black.Ratio,fullprecinct_mat$Total.Pop)

treat <- ifelse(fullprecinct3_mat$White.Ratio < .5,1,0)
mean(treat)
treat_prec <- fullprecinct3_mat$Precinct[treat == 1]

fullprecinct3_mat$treat <- ifelse(fullprecinct3_mat$Precinct %in% treat_prec,1,0)

plot(fullprecinct3_mat[fullprecinct3_mat$treat == 0,]$Weapons.Rate,fullprecinct3_mat[fullprecinct3_mat$treat == 0,]$Force.Rate,
     main = "Weapons Rate on Force Rate", sub = "Red = White Ratio < 50%, Bla = White Ratio > 50%", xlab = "Weapons Rate", ylab = "Force Rate")
lines(fullprecinct3_mat[fullprecinct3_mat$treat == 1,]$Weapons.Rate,fullprecinct3_mat[fullprecinct3_mat$treat == 1,]$Force.Rate,
      col ="red", type = "p")

lmx <- lm(fullprecinct3_mat[fullprecinct3_mat$treat == 0,]$Force.Rate~fullprecinct3_mat[fullprecinct3_mat$treat == 0,]$Weapons.Rate)
lmx2 <- lm(fullprecinct3_mat[fullprecinct3_mat$treat == 1,]$Force.Rate~fullprecinct3_mat[fullprecinct3_mat$treat == 1,]$Weapons.Rate)
abline(a = lmx$coefficients[1],b = lmx$coefficients[2])
abline(a = lmx2$coefficients[1],b = lmx2$coefficients[2], col = "red")

summary(lmx)
summary(lmx2)

plot(fullprecinct3_mat$Black.Rate,fullprecinct3_mat$Weapons.Rate)

lmx <- lm(fullprecinct3_mat$Weapons.Rate ~ fullprecinct3_mat$Black.Rate)
summary(lmx)
lmx <- lm(fullprecinct3_mat$Weapons.Rate ~ fullprecinct3_mat$Black.Rate + fullprecinct3_mat$ArrestRate)
summary(lmx)
plot(fullprecinct3_mat$ArrestRate,fullprecinct3_mat$Weapons.Rate)
plot(fullprecinct3_mat$Black.Rate,fullprecinct3_mat$ArrestRate)
hist(fullprecinct_mat[fullprecinct_mat$Year == 2010,]$Total.Pop)
s <- sum(fullprecinct_mat[fullprecinct_mat$Year == 2010,]$Total.Pop)

######## With Drugs
lmx <- lm(fullprecinct3_mat$Force.Rate ~ fullprecinct3_mat$Weapons.Rate + fullprecinct3_mat$Drug.Rate)
summary(lmx)

lmx <- lm(fullprecinct3_mat$Force.Rate ~ fullprecinct3_mat$Weapons.Rate + fullprecinct3_mat$Drug.Rate +
            fullprecinct3_mat$Black.Rate  + fullprecinct3_mat$ArrestRate)
summary(lmx)


lmx <- lm(fullprecinct3_mat$Force.incidents ~ fullprecinct3_mat$Weapons.Arrests + fullprecinct3_mat$Total.Drugs +
            fullprecinct3_mat$Black)
summary(lmx)

plot(fullprecinct3_mat$Weapons.Arrests,fullprecinct3_mat$Force.incidents)
plot(fullprecinct3_mat$Weapons.Arrests,fullprecinct3_mat$Total.Drugs)
plot(fullprecinct3_mat$Weapons.Rate,fullprecinct3_mat$ArrestRate)
lmx <- lm(fullprecinct3_mat[fullprecinct3_mat$Precinct != 14,]$ArrestRate ~ fullprecinct3_mat[fullprecinct3_mat$Precinct != 14,]$Weapons.Rate)
summary(lmx)
lmx <- lm(fullprecinct3_mat[fullprecinct3_mat$Precinct != 14,]$ArrestRate ~ fullprecinct3_mat[fullprecinct3_mat$Precinct != 14,]$Felony.Rate)
summary(lmx)
lmx <- lm(fullprecinct3_mat[fullprecinct3_mat$Precinct != 14,]$ArrestRate ~ fullprecinct3_mat[fullprecinct3_mat$Precinct != 14,]$Felony.Rate +
            fullprecinct3_mat[fullprecinct3_mat$Precinct != 14,]$Weapons.Rate)
summary(lmx)
lmx <- lm(fullprecinct3_mat[fullprecinct3_mat$Precinct != 14,]$Felony.Rate ~
            fullprecinct3_mat[fullprecinct3_mat$Precinct != 14,]$Weapons.Rate)
summary(lmx)

lmx <- lm(fullprecinct3_mat$Force.incidents/fullprecinct3_mat$Total.Pop ~ fullprecinct3_mat$Weapons.Arrests/fullprecinct3_mat$Total.Pop + 
            fullprecinct3_mat$Total.Drugs/fullprecinct3_mat$Total.Pop + fullprecinct3_mat$ArrestRate +
            fullprecinct3_mat$Black/fullprecinct3_mat$Total.Pop)
summary(lmx)

##########STOP Question and Frisk############

colnames(sqf3_mod)[1] <- "ID"
precincts <- sort(unique(sqf3_mod$STOP_LOCATION_PRECINCT))
sqf_mat <- matrix(sqf3_mod,3*length(precincts),3)
years <- c(2017,2018,2019)
j <- 1
i <- 1
for(i in 1:length(years)){
  year_temp <- years[i]
  for (j in 1:length(precincts)) {
    prec_temp <- precincts[j]
    total <- length(unique(sqf3_mod[sqf3_mod$STOP_LOCATION_PRECINCT == prec_temp & sqf3_mod$Year == year_temp,]$ID))
    
    sqf_mat[j,1] <- year_temp
    sqf_mat[j,2] <- prec_temp
    sqf_mat[j,3] <- total
    
    
    print(j)
  }
  j <- 1
  print(i)
}
sqf_mat <- as.data.frame(sqf_mat, stringsAsFactors = FALSE)

