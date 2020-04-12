# The first step is to download the dataset provided and copy it to the set path.
setwd("C:/Users/srikanth/Documents")

#The next step is to read the csv file and store it.
NIposco <- read.csv("NIPostcodes.csv",header = F)
NIposco


# In the following steps we acquire the total number of rows, structure, first and last 10 rows.
nrow(NIposco)
ncol(NIposco)


str(NIposco)


head(NIposco, n=10)
tail(NIposco, n=10)

#We need to check and change the column names according to our requirements.
colnames(NIposco) <- c("Organisation_Name","Sub-building_Name","Building_Name","Number","Primary_Thorfare",
                                  "Alt_Thorfare","Secondary_Thorfare","Locality","Townland","Town","County","PostCode",
                                  "X-Cordinates","Y-Cordinates","Primary_Key")

#The following step replaces and recode all the missing values.
NIposco[NIposco==""] <- NA
sum(is.na(NIposco))
sum(!complete.cases(NIposco))


#In this step missing values will be counted and stored in a dataframe.
Missing_Count <- sapply(NIposco, function(y) sum(length(which(is.na(y)))))
Missing_Count <- data.frame(Missing_Count)
Missing_Count

#We use the mice and VIM libraries to plot the missing values.
library(mice)
md.pattern(NIposco)
library(VIM)
missing_values <- aggr(NIposco, prop = FALSE, numbers = TRUE)

#The following step moves the Primary Key Identifier to the start of the database.
NIposco <- subset(NIposco, select = c(15,1:14))

#To create Limavady_data dataframe, the following step is used.
Limavady_data <- NIposco[which(NIposco$Locality == "LIMAVADY" | NIposco$Townland == "LIMAVADY" & NIposco$Town == "LIMAVADY"),]
Limavady_data
nrow(Limavady_data)

write.csv(Limavady_data,"Limavady.csv")
write.csv(NIposco,"CleanNIPostcodeData.csv")



#Section 2-----------------------------------------------------------------------------------------------------------

getwd()
setwd("C:/Users/srikanth/Documents/NICrimeData")
 

NICrime_data <- read.csv("ALLNICrimeData.csv")
head(NICrime_data)
nrow(NICrime_data)
NICrime_data$Crime.ID <- NULL
NICrime_data$Reported.by <- NULL
NICrime_data$Falls.within <- NULL
NICrime_data$LSOA.code <- NULL
NICrime_data$LSOA.name <- NULL
NICrime_data$Last.outcome.category <- NULL
NICrime_data$Context <- NULL

summary(NICrime_data,15)

library(plyr)
NICrime_data$Crime.type <- revalue(NICrime_data$Crime.type,c("Anti-social behaviour" = "ASBO","Bicycle theft" = "BITH",
                                                         "Burglary" = "BURG","Criminal damage and arson" = "CDAR",
                                                         "Drugs" = "DRUG","Other Theft = OTTH","Public order" = "PUBO",
                                                         "Robbery" = "ROBY", "Shoplifting" = "SHOP","Theft from the person" = "THPR",
                                                         "Vehicle crime" = "VECR", "Violence and sexual offences" = "VISO",
                                                         "Other crime" = "OTCR","Other theft" = "OTTH","Possession of weapons" = "POW"))

summary(NICrime_data$Crime.type,15)
write.csv(NICrime_data,"ALLNICrimeData.csv",row.names = F)
Final_NICrime_data <-read.csv("ALLNICrimeData.csv")
nrow(temp_data)
head(temp_data)
str(temp_data)

temp_data <- read.csv("ALLNICrimeData.csv")

attach(temp_data)
plot(temp_data$Crime.type,type='o',col = "Blue")

# Using the plot() function describe the crime frequency rate.
attach(Final_NICrime_data)
plot(Final_NICrime_data$Crime.type,ylim=c(0,200000),col = topo.colors(14),main = "Crime frequeny rate",
     xlab="Crime Type",ylab="Number of Crimes")
detach(Final_NICrime_data)

# We need to remove the On or near string from the location column.
Final_NICrime_data$Location <- sub("On or near ","",Final_NICrime_data$Location)
head(Final_NICrime_data$Location, n=10)
Final_NICrime_data$Location[Final_NICrime_data$Location == ""] <- NA

# Picking the random sample of 5000 entiries using set seed function.
secondary_Crime_data <- na.omit(Final_NICrime_data)
set.seed(100)
random_crime_sample <- secondary_Crime_data[sample(nrow(secondary_Crime_data),5000),]

