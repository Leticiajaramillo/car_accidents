# general clean up
rm(list = ls())

## set working enviroment
setwd("C:/Users/letic/Documents/GMBD/projects/R")

getwd()



## Install packages 
install.packages ("psych", lib = "C:/Users/letic/Documents/GMBD/projects/R")
install.packages ("gplots", lib = "C:/Users/letic/Documents/GMBD/projects/R")
install.packages ("gmodels", lib = "C:/Users/letic/Documents/GMBD/projects/R")
install.packages ("vcd")
install.packages("lsr")
install.packages("questionr")
library (gplots)
library (psych)
library (gmodels)
library(vcd)
library(lsr)
library(questionr)
library(zoo)

#Reads data from CSV file and stores it in a dataframecalled acc_15 and acc_16

acc_15 <- read.csv ("Accidents_2015.csv", header=TRUE, sep=",", dec=".")
dim(acc_15)
head(acc_15)

acc_16 <- read.csv ("Accidents_2016.csv", header=TRUE,sep=",",dec=".")
dim(acc_16)
head(acc_16)
#QC OK

acc <- rbind(acc_15,acc_16)
dim(acc)
head(acc)
###QC OK

# Define variable as factor and define levelsand labels.
acc$Accident_Severity_f <-factor (acc$Accident_Severity, levels= c(1,2,3),
                        labels = c("Fatal", "Serious", "Slight"))
# Describe numerically
# Frequency count
t <-table (acc$Accident_Severity_f)
t
# As proportion over sample size
t1 <-prop.table(t)
t1
# As percentage over sample size
t2 <-t1*100
t2

barplot(t2,
        beside = TRUE,
        col=c("lightskyblue1", "lightskyblue2", "lightskyblue3"),
        xlab = "Accident Severity",
        main="Accident Severity Percentage")

### Convert date to quarter
yq <- as.yearqtr(as.yearmon(acc$Date, "%d/%m/%Y") + 1/12)
yq

##Convert quarter into season
acc$Season <- factor(format(yq, "%q"), levels = 1:4, 
                    labels = c("winter", "spring", "summer", "fall"))

#barplot 
t_season_f <- table ( acc$Season)
t1_season_f <- prop.table(t_season_f)*100
t1_season_f

barplot(t1_season_f,
        beside = TRUE,
        col=c("lightskyblue1", "lightskyblue2", "lightskyblue3"),
        xlab = "Season",
        main="Season Percentage")


###DESCRIBE DV (Accident severity) by the factor levels in the IV (cross tabulation of DV by IV)

describe(acc$Accident_Severity)
          n     %
Fatal     3311   1.2
Serious  41763  15.1
Slight  231603  83.7
Total   276677 100.0

barplot(t2,
        beside = TRUE,
        col=c("lightskyblue1", "lightskyblue2", "lightskyblue3"),
        xlab = "Accident Severity",
        main="Accident Severity Percentage")

describe(acc$Season)
        n     %
winter  67006  24.2
spring  66164  23.9
summer  70583  25.5
fall    72924  26.4
Total  276677 100.0

barplot(t1_season_f,
        beside = TRUE,
        col=c("lightskyblue1", "lightskyblue2", "lightskyblue3"),
        xlab = "Season",
        main="Season Percentage")

#DV aggregate distribution
TOTAL<- table(acc$Accident_Severity_f)

#Basic crosstable
ct1 <- table(acc$Accident_Severity_f,
             acc$Season)
ct1
#Merge together the Cross tab and DV dist
ct2 <- cbind(ct1, TOTAL)
ct2
# Express the table as column percentages
cp_ct2 <- prop.table (ct2,2)*100

# Add column totals (100)
addmargins (cp_ct2,1)

            winter     spring     summer       fall      TOTAL
Fatal     1.256604   1.172843   1.217007   1.143656   1.196702
Serious  14.324090  14.850976  15.668079  15.468159  15.094496
Slight   84.419306  83.976180  83.114914  83.388185  83.708801
Sum     100.000000 100.000000 100.000000 100.000000 100.000000



#### PERFOM THE NUMERIC TEST CHI2 TEST

# We apply the chisq.test() over the original crosstab containing FREQUENCIES
chisq.test(ct1)

### Pearson's Chi-squared test

###data:  ct1
###X-squared = 63.833, df = 6, p-value = 7.464e-12 (0.000)

###CONCLUSION 
### As P. Value < 0.05 we reject H0 
### H0.: Percentage of severity of accidents with fatal/serious/slight is the same in winter, spring, summer, fall
### H1.: Percentage of severity of accidents with fatal/serious/slight differes in winter, spring, summer, fall

###GRAPHIC REPRESENTATION COMBINED BARPLOT 

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(cp_ct2,
        beside = TRUE,
        col=c("lightskyblue1", "lightskyblue2", "lightskyblue3"),
        main="Accident Severity, 
        by season (percentages)")
legend("topright", inset=c(-0.3,0),legend = c("Fatal", "Serious", "Slight", " ", "Chi2: 63.83", "P.Val: 0.000"), fill=c("lightskyblue1", "lightskyblue2", "lightskyblue3", "white", "white", "white"), border="white")

##Cramer's V season  (0 no association, 1 perfect association)
cramersV(ct1)
[1] 0.01074044


###MOSAIC PLOT
mosaic(~acc$Season + acc$Accident_Severity_f, data=acc, shade=TRUE, legend=TRUE)

############################################################################

### IV 2 AREA TYPE URBAN, RURAL, UNALLOCATED

# Define variable as factor and define levelsand labels.
acc$area_type_f <-factor (acc$Urban_or_Rural_Area, levels= c(1,2,3),
                                  labels = c("Urban", "Rural", "Unallocated"))
# Describe numerically
# Frequency count
t_area <-table (acc$area_type_f)
t_area
# As proportion over sample size
t1_area <-prop.table(t_area)
t1_area
# As percentage over sample size
t2_area <-t1_area*100
t2_area

barplot(t2_area,
        beside = TRUE,
        col=c("lightskyblue1", "lightskyblue2", "lightskyblue3"),
        xlab = "Area Type",
        main="Area Type Percentage")

describe(acc$area_type_f)

              n     %
Urban       180548  65.3
Rural        96122  34.7
Unallocated      7   0.0
Total       276677 100.0

#Basic crosstable
ct1_area <- table(acc$Accident_Severity_f,
             acc$area_type_f)
ct1_area
#Merge together the Cross tab and DV dist
ct2_area <- cbind(ct1_area, TOTAL)
ct2_area
# Express the table as column percentages
cp_ct2_area <- prop.table (ct2_area,2)*100
cp_ct2_area

# Add column totals (100)
addmargins (cp_ct2_area,1)

          Urban      Rural      Unallocated   TOTAL
Fatal     0.6591045   2.206571     0.00000   1.196702
Serious  13.3083723  18.448430    28.57143  15.094496
Slight   86.0325232  79.344999    71.42857  83.708801
Sum     100.0000000 100.000000   100.00000 100.000000

#### PERFOM THE NUMERIC TEST CHI2 TEST

# We apply the chisq.test() over the original crosstab containing FREQUENCIES
chisq.test(ct1_area)


### Pearson's Chi-squared test

### data:  ct1_area
### X-squared = 2689.3, df = 4, p-value < 2.2e-16


###CONCLUSION 
### As P. Value < 0.05 we reject H0 
### H0.: Percentage of severity of accidents with fatal/serious/slight is the same in Urban, Rural, Unallocated area
### H1.: Percentage of severity of accidents with fatal/serious/slight is not the same in Urban, Rural, Unallocated area

###GRAPHIC REPRESENTATION COMBINED BARPLOT 

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
barplot(cp_ct2_area,
        beside = TRUE,
        col=c("lightskyblue1", "lightskyblue2", "lightskyblue3"),
        main="Accident Severity, 
        by Area Type (percentages)")
legend("topright", inset=c(-0.3,0),legend = c("Fatal", "Serious", "Slight", " ", "Chi2: 2689.3", "P.Val: 0.000"), fill=c("lightskyblue1", "lightskyblue2", "lightskyblue3", "white", "white", "white"), border="white")

##Cramer's V season  (0 no association, 1 perfect association)
cramersV(ct1_area)
[1] 0.0697136


###MOSAIC PLOT
mosaic(~acc$area_type_f + acc$Accident_Severity_f, data=acc, shade=TRUE, legend=TRUE)

##SQUARE PANEL OF PLOTS
par(mfrow=c(2,2))






