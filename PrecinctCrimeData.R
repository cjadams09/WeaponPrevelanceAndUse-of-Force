setwd("~/GunStatsAnalysis")
library(readr)
library(dplyr)
library(rgdal)
library(leaflet)
library(stargazer)
library(latticeExtra)

FullArrRepo <- read.csv("NYPD_Arrests_Data_06-19.csv", as.is = TRUE)
fullprecinct_mat <- read.csv("fullprecinct_mat.csv")


FullArrRepo$year <- as.numeric(substr(FullArrRepo$ARREST_DATE,7,11))
unique(FullArrRepo$year)


precincts <- sort(unique(FullArrRepo$ARREST_PRECINCT))
races <- sort(unique(FullArrRepo$PERP_RACE))
years <- sort(unique(FullArrRepo$year))
lawcode <- sort(unique(FullArrRepo$LAW_CODE))
section <- sort(unique(FullArrRepo$OFNS_DESC))
sex <- sort(unique(FullArrRepo$PERP_SEX))
pdesc <- sort(unique(FullArrRepo$PD_DESC))
age <- sort(unique(FullArrRepo$AGE_GROUP))
date <- sort(unique(FullArrRepo$ARREST_DATE))
x <- length(unique(section))+length(unique(races))+length(unique(lawcode)) + 3


detailprecinct_mat <- matrix(NA,length(precincts),x)
k <- 1
for(i in 1:1) {
  year_temp <- 2019
  for (j in 1:length(precincts)) {
    k <- 1
    prec_temp <- detailprecinct[j]
    total_arrest <- length(unique(FullArrRepo[FullArrRepo$ARREST_PRECINCT==prec_temp &
                                                FullArrRepo$year == year_temp,]$ARREST_KEY))
    detailprecinct_mat[j,k] <- year_temp
    k <- k+1
    detailprecinct_mat[j,k] <- prec_temp
    k <- k+1
    detailprecinct_mat[j,k] <- total_arrest
    k <- k+1
    
    for (l in 1:length(races)) {
      race_temp <- races[l]
      race_arrests <- length(unique(FullArrRepo[FullArrRepo$ARREST_PRECINCT==prec_temp &
                                                 FullArrRepo$year == year_temp & 
                                                 FullArrRepo$PERP_RACE == race_temp,]$ARREST_KEY))
      detailprecinct_mat[j,k] <- race_arrests
      k <- k+1
      print(k)
    }
    l <- 1
    for (l in 1:length(lawcode)) {
      crime_temp <- lawcode[l]
      crime_arrests <- length(unique(FullArrRepo[FullArrRepo$ARREST_PRECINCT==prec_temp &
                                                  FullArrRepo$year == year_temp & 
                                                  FullArrRepo$LAW_CODE == crime_temp,]$ARREST_KEY))
      detailprecinct_mat[j,k] <- crime_arrests
      k <- k+1
      print(k)
    }
    for (l in 1:length(section)) {
      sec_temp <- section[l]
      sec_arrests <- length(unique(FullArrRepo[FullArrRepo$ARREST_PRECINCT==prec_temp &
                                                   FullArrRepo$year == year_temp & 
                                                   FullArrRepo$OFNS_DESC == sec_temp,]$ARREST_KEY))
      detailprecinct_mat[j,k] <- sec_arrests
      k <- k+1
      print(k)
    }
    
    print(j)
    print(i)
  }
}

lawcat <- sort(unique(FullArrRepo$LAW_CAT_CD))
lawcat_mat <- matrix(NA,length(precincts),2 + length(lawcat))

for(i in 1:1) {
  year_temp <- 2019
  for (j in 1:length(precincts)) {
    k <- 1
    prec_temp <- precincts[j]
    total_arrest <- length(unique(FullArrRepo[FullArrRepo$ARREST_PRECINCT==prec_temp &
                                              FullArrRepo$year == year_temp,]$ARREST_KEY))
    lawcat_mat[j,k] <- year_temp
    k <- k+1
    lawcat_mat[j,k] <- prec_temp
    k <- k+1
    
    
    for (l in 1:length(lawcat)) {
      cat_temp <- lawcat[l]
      cat_arrests <- length(unique(FullArrRepo[FullArrRepo$ARREST_PRECINCT==prec_temp &
                                                FullArrRepo$year == year_temp & 
                                                FullArrRepo$LAW_CAT_CD == cat_temp,]$ARREST_KEY))
      lawcat_mat[j,k] <- cat_arrests
      k <- k+1
      print(k)
    }
    
    print(j)
    print(i)
  }
}

detailprecinct_mat <- as.data.frame(detailprecinct_mat, stringsAsFactors = FALSE)
colnames(detailprecinct_mat) <- c("Year", "Precinct", "Total Arrests", races, lawcode, section)
detailprecinct2019_mat <- detailprecinct_mat
write.csv(detailprecinct_mat,"detailprecinct_mat.csv")
write.csv(detailprecinct2019_mat,"detailprecinct2019_mat.csv", as.is = TRUE)
detailprecinct2019_mat <- read.csv("detailprecinct2019_mat.csv", as.is = TRUE)
colnames(detailprecinct2019_mat) <- c("X","Year", "Precinct", "Total Arrests", races, lawcode, section)
lawcat_mat <- as.data.frame(lawcat_mat, stringsAsFactors = FALSE)

colnames(lawcat_mat) <- c("Year","Precinct",lawcat)

detailprecinct2019_mat <- merge(detailprecinct2019_mat,lawcat_mat, by = "Precinct",)
y <- read.csv("PrecinctComplaints.csv", as.is = TRUE)
summary(y)

z <- read.csv("UseOfForceFlat.csv", as.is = TRUE)

a <- read.csv("InjuryReports - Officer.csv", as.is = TRUE)
b <- read.csv("InjuryReports - Subject.csv", as.is = TRUE)
precinctpop <- read.csv("nyc_precincts_pop.csv", as.is = TRUE)
precpoptot <- precinctpop %>% select(1,73:74,77:82)
precpoptot <- as.data.frame(precpoptot, stringsAsFactors = FALSE)
colnames(precpoptot) <- c("Precinct", "Total Pop", "Hisp Pop", "White Pop", "Black Pop", "Native Pop", "Asian Pop", "Pacific Islander Pop","Other Pop")

detailprecinct2019_mat <- merge(detailprecinct2019_mat,y, by.detailprecinct2019_mat = c("Precinct"), all.detailprecinct2019_mat = TRUE)
detailprecinct2019_mat <- merge(detailprecinct2019_mat,z, by.detailprecinct2019_mat = c("Precinct"), all.detailprecinct2019_mat = FALSE)
detailprecinct2019_mat <- merge(detailprecinct2019_mat,a, by.detailprecinct2019_mat = c("Precinct"), all.detailprecinct2019_mat = TRUE)
detailprecinct2019_mat <- merge(detailprecinct2019_mat,b, by.detailprecinct2019_mat = c("Precinct"), all.detailprecinct2019_mat = TRUE)
detailprecinct2019_mat <- merge(detailprecinct2019_mat,precpoptot, by.detailprecinct2019_mat = c("Precinct"), all.detailprecinct2019_mat = TRUE)

detailprecinct2019_mat$Total.Force <- detailprecinct2019_mat$Total


weaponassault_mat <- detailprecinct2019_mat[, colnames(detailprecinct2019_mat) %in% 
                  c("Year","Precinct","DANGEROUS WEAPONS","FELONY ASSAULT","DANGEROUS DRUGS", "PL 1200502",
                    "PL 1200504", "PL 1200003", "PL 1201001", "PL 1201100", "PL 1201401"
                    , "PL 1201800","PL 1205501","PL 265031B", "PL 2650102", "Total Arrests", "Total.Force",
                    "Total.Subject","Total.Officer","Complaints","Total Pop","White Pop","Black Pop","BLACK", "WHITE")]
weaponassault_mat$weaponassault <- rowSums(weaponassault_mat[,6:13])
weaponassault_mat$Weapon.Rate <- weaponassault_mat$`DANGEROUS WEAPONS`/weaponassault_mat$`Total Arrests`
weaponassault_mat$armedfelonypercentage <- weaponassault_mat$weaponassault/weaponassault_mat$`FELONY ASSAULT`
weaponassault_mat$Felony.Rate <- weaponassault_mat$`FELONY ASSAULT`/weaponassault_mat$`Total Arrests`
weaponassault_mat$Force.Rate <- weaponassault_mat$Total.Force/weaponassault_mat$`Total Arrests`
weaponassault_mat$Firearm.Rate <- weaponassault_mat$`PL 265031B`/weaponassault_mat$`Total Arrests`
weaponassault_mat$knownfirearmspercentage <- weaponassault_mat$`PL 265031B`/weaponassault_mat$`DANGEROUS WEAPONS` 
weaponassault_mat$SubjectInjury.Rate <- weaponassault_mat$Total.Subject/weaponassault_mat$`Total Arrests`
weaponassault_mat$OfficerInjury.Rate <- weaponassault_mat$Total.Officer/weaponassault_mat$`Total Arrests`
weaponassault_mat$Complaint.Rate <- weaponassault_mat$Complaints/weaponassault_mat$`Total Arrests`
weaponassault_mat$Arrest.Rate <- weaponassault_mat$`Total Arrests`/weaponassault_mat$`Total Pop`
weaponassault_mat$White.Rate <- weaponassault_mat$WHITE/weaponassault_mat$`Total Arrests`
weaponassault_mat$Black.Rate <- weaponassault_mat$BLACK/weaponassault_mat$`Total Arrests`
weaponassault_mat$Drug.Rate <- weaponassault_mat$`DANGEROUS DRUGS`/weaponassault_mat$`Total Arrests`


summary(weaponassault_mat$knownfirearmspercentage)


summ_list <- c("Total Arrests","Arrest.Rate","Total Pop","White Pop","Black Pop","White.Rate","Black.Rate",
               "Force.Rate","SubjectInjury.Rate","OfficerInjury.Rate","Complaint.Rate",
               "Weapon.Rate","Felony.Rate","Drug.Rate")
summ_tab <- matrix(NA,length(summ_list),3)
for (i in 1:length(summ_list)) {
  var <- summ_list[i]
  summ_tab[i,1] <- var
  summ_tab[i,2] <- mean(weaponassault_mat[,colnames(weaponassault_mat)==var], 
                        na.rm = TRUE)
  summ_tab[i,3] <- sd(weaponassault_mat[,colnames(weaponassault_mat)==var], 
                        na.rm = TRUE)
}
setwd("~/GunStatsAnalysis")
colnames(summ_tab) <- c("Variable", "Mean", "Std. Dev.")
write.csv(summ_tab, "summ_tab.csv")

summ_tab <- read.csv("summ_tab.csv")

plot(weaponassault_mat$armedfelonypercentage, weaponassault_mat$Force.Rate)
plot(weaponassault_mat$armedfelonypercentage, weaponassault_mat$Weapon.Rate)

plot(weaponassault_mat$Firearm.Rate,weaponassault_mat$Weapon.Rate, main = "Weapons and Known Firearms", xlab = "Known Firearm rate", ylab = "Weapons Rate")
lmx <- lm(weaponassault_mat$Weapon.Rate~weaponassault_mat$Firearm.Rate)
summary(lmx)
abline(a = lmx$coefficients[1], b = lmx$coefficients[2])

length(unique(weaponassault_mat[weaponassault_mat$`White Pop`/weaponassault_mat$`Total Pop` < .5 & weaponassault_mat$Weapon.Rate > .04,]$Precinct))
length(unique(weaponassault_mat[weaponassault_mat$`White Pop`/weaponassault_mat$`Total Pop` > .5 & weaponassault_mat$Weapon.Rate < .04,]$Precinct))


plot(weaponassault_mat$White.Rate,weaponassault_mat$`PL 2650102`/weaponassault_mat$`Total Arrests`, xlab = "White Rate of Arrest", ylab = "Weapon Arrest Rate")
lmx <- lm(weaponassault_mat$`PL 2650102`/weaponassault_mat$`Total Arrests`~weaponassault_mat$White.Rate)
summary(lmx)
abline(a = lmx$coefficients[1], b = lmx$coefficients[2])

cor(weaponassault_mat$White.Rate,weaponassault_mat$`PL 2650102`/weaponassault_mat$`Total Arrests`)

summary(weaponassault_mat$`PL 2650102`/weaponassault_mat$`DANGEROUS WEAPONS`)

plot(weaponassault_mat$`White Pop`/weaponassault_mat$`Total Pop`,weaponassault_mat$Weapon.Rate, xlab = "White Population Ratio", ylab = "Weapon Arrest Rate")
lmx <- lm(weaponassault_mat$Weapon.Rate~weaponassault_mat$`White Pop`/weaponassault_mat$`Total Pop`)
summary(lmx)
abline(a = lmx$coefficients[1], b = lmx$coefficients[2])


lmx1 <- lm(weaponassault_mat$Weapon.Rate~weaponassault_mat$White.Rate + weaponassault_mat$Arrest.Rate)
summary(lmx1)
