#necessary library load
library(readxl)
library(dplyr)

#necessary dataset load

#### ******DATA SET SUMMARY FROM PROBLEM SOURCE ******** 

#Age 	Age of the patient discharged
#Female 	A binary variable that indicates if the patient is female
#Los	Length of stay in days
#Race Race of the patient (specified numerically)
#Totchg	Hospital discharge costs
#Aprdrg	All Patient Refined Diagnosis Related Groups
# *******************************************************

dataSet <- readxl::read_xlsx("./data set/hospitalcosts.xlsx")
str(dataSet)
View(dataSet)
summary(dataSet)

#finding maximum hospital charge and patient Age group
maxTotalCharge = max(dataSet$TOTCHG)
dfAge = dplyr::filter(dataSet,TOTCHG == maxTotalCharge)
maximumHospitalChargeAge = dfAge["AGE"]

#finding maximum hospital charge and patient Refined Diagnosis Related Groups
dfDiagnosisGroup = dplyr::filter(dataSet,TOTCHG == maxTotalCharge)
dfDiagnosisGroupMax = dfDiagnosisGroup["APRDRG"]

#Total hospital charges vs Race data comparison
data <- table(dataSet$RACE,dataSet$TOTCHG)
plot(dataSet$TOTCHG,dataSet$RACE,type="l",col="red")
par(TRUE)
lines(dataSet$TOTCHG,dataSet$RACE,col="green")

#Prediction dependencies of length of stay on Age,Race, Gender
predHospitalStayData <- lm(formula = LOS ~ AGE + FEMALE + RACE, data = dataSet)
summary(predData) #Age only highly dependdable for predicting Hospital stay Target variable(because pr(>|t|)<0.05)


##Prediction dependencies check of Hospital total charges on all independent variable
predHospitalCharges <- lm(formula = TOTCHG ~ .,data = dataSet)
summary(predHospitalCharges)
# Hospital Charges not depend on Gender and Race(because pr(>|t|)>0.05), highly dependable on Age, Length of hospital stay, All Patient Refined Diagnosis Related Groups (because pr(>|t|)<0.05)
