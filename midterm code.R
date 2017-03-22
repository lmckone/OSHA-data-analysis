####### Library Installation #######

require(magrittr)
require(lubridate)
require(dplyr)
require(tidyr)
require(foreign)
require(ggplot2)
require(RColorBrewer)

####### Load Data #######

# Load accident file
a1 <- read.dbf("accid.dbf")

#Load violation file
v1 <- read.dbf("viol.dbf")

#Load osha file
o1 <- read.dbf("osha.dbf")

####### Clean Data #######

####### Clean accident file #######

#check if all states are MA. If so, remove state column as it is redundant
if(sum(a1$SITESTATE=="MA")==dim(a1)[1]){a1 %<>% select(-SITESTATE)}

#set age column to NA in all rows with age equal to 0
a1$AGE[which(a1$AGE=="0")] <- NA

#set occupation code to NA in all rows with occ_code equal to 000
a1$OCC_CODE[which(a1$OCC_CODE=="000")] <- NA

#set hazsub to NA in all row with hazsub equal to 0000
#it would be interesting to know why this 0000 encoding is used as well as NA
a1$HAZSUB[which(a1$HAZSUB=="0000")] <- NA


####### Clean OSHA table #######

#check if all states are MA. If so, remove state column as it is redundant
if(sum(o1$SITESTATE=="MA")==dim(o1)[1]){o1 %<>% select(-SITESTATE)}

#Get rid of all columns with all NA
o1 <- Filter(function(x)!all(is.na(x)), o1)

####### Clean Violations Table #######

#check if all states are MA. If so, remove state column as it is redundant
if(sum(v1$SITESTATE=="MA")==dim(v1)[1]){v1 %<>% select(-SITESTATE)}

#Get rid of all tables with all NA
v1 <- Filter(function(x)!all(is.na(x)), v1)

levels(v1$EMPHASIS) <- c("egregious")
levels(v1$VIOLTYPE) <- c("other", "repeat", "serious", "unclassified", "willful")

v1 %<>% select(ACTIVITYNO, EMPHASIS, GRAVITY, VIOLTYPE, ABATE, INSTANCES, NUMEXPOSED)

v1$ABATE %<>% ymd()

####### Label Tables #######

####### Labeling Accident File #######

# Read body part labels
acclabels <- read.dbf("lookups/acc.dbf")

#join the body part names to the codes so that the data is more meaningful
bodylabels <- acclabels[(acclabels$CATEGORY=="PART-BODY"),]
bodylabels <- select(bodylabels, CODE, VALUE)

#to join, column names must be the same
colnames(bodylabels) <- c("BODYPART", "BodypartVALUE")    
a1 <- left_join(a1, bodylabels, by="BODYPART")

#join the nature of injury to the codes so that the data is more meaningful
naturelabels <- acclabels[(acclabels$CATEGORY=="NATUR-INJ"),]
naturelabels <- select(naturelabels, CODE, VALUE)

#to join, column names must be the same
colnames(naturelabels) <- c("NATURE", "NatureVALUE")    
a1 <- left_join(a1, naturelabels, by="NATURE")

#join the source of injury to the codes so that the data is more meaningful
sourcelabels <- acclabels[(acclabels$CATEGORY=="SOURC-INJ"),]
sourcelabels <- select(sourcelabels, CODE, VALUE)

#to join, column names must be the same
colnames(sourcelabels) <- c("SOURCE", "SourceVALUE")    
a1 <- left_join(a1, sourcelabels, by="SOURCE")

#join the event type to the codes so that the data is more meaningful
eventlabels <- acclabels[(acclabels$CATEGORY=="EVENT-TYP"),]
eventlabels <- select(eventlabels, CODE, VALUE)

#to join, column names must be the same
colnames(eventlabels) <- c("EVENT", "EventVALUE")    
a1 <- left_join(a1, eventlabels, by="EVENT")

#join the environmental factor to the codes so that the data is more meaningful
envlabels <- acclabels[(acclabels$CATEGORY=="ENVIR-FAC"),]
envlabels <- select(envlabels, CODE, VALUE)

#to join, column names must be the same
colnames(envlabels) <- c("ENVIRON", "EnvironmentVALUE") 
a1 <- left_join(a1, envlabels, by="ENVIRON")

#join the human factor to the codes so that the data is more meaningful
humanlabels <- acclabels[(acclabels$CATEGORY=="HUMAN-FAC"),]
humanlabels <- select(humanlabels, CODE, VALUE)

#to join, column names must be the same
colnames(humanlabels) <- c("HUMAN", "HumanVALUE") 
a1 <- left_join(a1, humanlabels, by="HUMAN")

#change task levels
levels(a1$TASK)
levels(a1$TASK) <- c("other", "regularly assigned task", "task other than reg. assigned")
#Read occupation labels
occlabels <- read.dbf("lookups/occ.dbf")

#join the occupations to the codes so that the data is more meaningful
colnames(occlabels) <- c("OCC_CODE", "OCCUPATION")
a1 <- left_join(a1, occlabels, by="OCC_CODE")


#Change the levels of degree, making them more understandable
levels(a1$DEGREE)
levels(a1$DEGREE) <- c("no injury", "fatality", "hospitalized", "non-hospitalized")

#Read hazardous substance labels
hzslabels <- read.dbf("lookups/hzs.dbf")

#join the hazardous substance names to the codes, making them more understandable
colnames(hzslabels) <- c("HAZSUB", "SUBSTANCE")
a1 <- left_join(a1, hzslabels, by="HAZSUB")

a1 %<>% select(-NATURE, -BODYPART, -SOURCE, -EVENT, -ENVIRON, -HUMAN, -HAZSUB, -OCC_CODE, -RELINSP)

names(a1) <- c("ACTIVITYNO", "NAME", "SEX", "DEGREE", "TASK", "AGE", "BODYPART", "Nature", "Source", "Event", "EnvironmentalFac", "HumanFac", "Occupation", "Substance")

####### OSHA Labels #######

#Change labels for most recent type of OSHA activity:
levels(o1$PREVCTTYP)
levels(o1$PREVCTTYP) <- c("accident", "complaint", "inspection", "referral")
#o1$PREVCTTYP[which(is.na(o1$PREVCTTYP))] <- "no activity"

#Change labels for job title of officer doing inspection
levels(o1$JOBTITLE)
levels(o1$JOBTITLE) <- c("area director", "safety officer", "health officer", "safety trainee", "health trainee", "other", "supervisor", "discrim. invest'r", "regional management")

#read in county/city codes and NAICS codes
sic <- read.dbf("lookups/sic.dbf")
scc <- read.dbf("lookups/scc.dbf")

#Create a table with all of the cities and counties in Massachusetts
sccMA <- filter(scc, STATE=="MA")

#Create a table of counties in Massachusetts
sccMACounties <- filter(sccMA, TYPE=="2")
sccMACounties %<>% select(COUNTY, NAME)

#rename column in o1 to successfully join
o1 <- rename(o1, COUNTY=SITECNTY)

#join county names to OSHA table
o1 <- left_join(o1, sccMACounties, by="COUNTY")

#rename countyname column so it is clear
o1 <- rename(o1, COUNTYNAME=NAME)

#Create a table of cities in Massachusetts
sccMACities <- filter(sccMA, TYPE=="3")
sccMACities %<>% select(CITY, NAME)

#rename column in o1 to successfully join
o1 <- rename(o1, CITY=SITECITY)

#join city names to OSHA table
o1 <- left_join(o1, sccMACities, by="CITY")

#rename column in OSHA so its meaning is clear
o1 <- rename(o1, CITYNAME=NAME)

#rename OWNERTYPE levels
levels(o1$OWNERTYPE)
levels(o1$OWNERTYPE) <- c("private", "local government", "state government", "federal government")

#transform date fields into dates
o1$OSHA1MOD <- ymd(o1$OSHA1MOD)
o1$OPENDATE <- ymd(o1$OPENDATE)
o1$CLOSEDATE <- ymd(o1$CLOSEDATE)
o1$CLOSEDATE2 <- ymd(o1$CLOSEDATE2)

#Join SIC codes so we can see the industry names
o1 <- left_join(o1, sic, by="SIC")

#Change levels of CAT_SH
levels(o1$CAT_SH) <- c("health", "safety")

o1 %<>% select(ACTIVITYNO, ESTABNAME, INDUSTRY, SITEADD, CITYNAME, COUNTYNAME, OPENDATE, CLOSEDATE)

head(o1)

####### Exploratory Analysis #######

#subset a1 so that there are no duplicate ACTIVITYNO values - these values are supposed to be unique
#but OSHA divided up some of the records so they have the same ACTIVITYNO. This is misleading
#because it distorts the number of accidents that occured at each company if we simply count
#the ACTIVITYNOs. In some cases, multiple people were injured in one accident. Since this information 
#is certainly also useful, the original table is retained in a1. But the distinct accident table
#is given below.
distincta1 <- a1[duplicated(a1$ACTIVITYNO)=="FALSE",]
distincta1[distincta1$ACTIVITYNO=="10089191",]

#Create a small table that contains the correspondence between ACTIVITYNO and the name of the establishment
estabs <- select(o1, ACTIVITYNO, ESTABNAME)

#join the accident table of distinct accidents to the key-value establishment table you made
estabaccidents <- left_join(distincta1, estabs, by="ACTIVITYNO")

#Count how many records have the same ESTABNAME - this respresents how many accidents
#occured at that establishment.
acount <- count(estabaccidents, ESTABNAME)
acount %<>% rename(numAccidents=n)

#Order the accident count table in descending order with respect to the number of accidents
acount %<>% arrange(desc(numAccidents))

#taking the head gives us the establishments with the most accidents
head(acount, 10)

#Let's take a look at the accident records for the establishment with the highest number of
#accidents
BECaccidents <- estabaccidents[estabaccidents$ESTABNAME=="BOSTON EDISON CO",]

BECaccidents <- select(BECaccidents, NAME, DEGREE, TASK, BODYPART, Nature, Source, Event, EnvironmentalFac, HumanFac)

#You may have noticed that two of the top 10 companies have very similar names
#Let's look at BOSTON EDISON CO's more verbose counterpart:
estabaccidents[estabaccidents$ESTABNAME=="BOSTON EDISON COMPANY",]

#Now that we know how the companies with the most accidents look, we should
#check out some summary statistics to see how unusual these numbers are




#Find average number of accidents so we know how unusual highest numbers are
summarise(acount, avg=mean(numAccidents))

#And the standard deviation
summarise(acount, sd=sd(numAccidents))

#Each company has occured on average 1.05 accidents with a standard deviation of 0.349
#So a company having 7 accidents is no small thing!


####### Plotting #######

#In order to plot what we want, we need to do some joins

#This allows us to look at number of accidents per establishment in relation to all the fun variables
#in the OSHA table. 

x <- left_join(estabaccidents, acount, by="ESTABNAME")
all <- left_join(x, o1, by="ACTIVITYNO")

#Some filtering will allow us to compare companies with many versus few accidents.
lower <- filter(all, all$numAccidents<=3)
upper <- filter(all, all$numAccidents>3)

#Calculate the mean and sum of accidents that occur in each county
summarise(group_by(all, COUNTYNAME), sum(numAccidents))
summarise(group_by(all, COUNTYNAME), mean(numAccidents))

colourCount = length(unique(x$Event))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

#plot of distribution of accident events
i <- ggplot(x, aes(numAccidents))
i + geom_histogram(aes(fill=Event), binwidth = 20) + scale_fill_manual(values = getPalette(colourCount)) + theme_dark() + ggtitle("Number of Accidents per Event Type") + xlab("") + ylab("Accident count")

colourCount = length(unique(all$COUNTYNAME))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

#plot of accidents by county
h <- ggplot(all, aes(numAccidents))
h + geom_histogram(aes(fill=COUNTYNAME), binwidth = 20) + scale_fill_manual(values = getPalette(colourCount)) + theme_dark() + ggtitle("Number of Workplace Accidents per MA County") + xlab("") + ylab("Accident count")

#Plot of the events that caused accidents in establishments that had <=3 accidents
colourCount = length(unique(lower$Event))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

l <- ggplot(lower, aes(numAccidents))
l + geom_histogram(aes(fill=Event), binwidth = 20) + scale_fill_manual(values = getPalette(colourCount)) + theme_dark() + ggtitle("Accident Causes in Low-Risk Establishments") + ylab("Accident Count") + xlab("")

#Plot of the events that caused accidents in establishments that had >3 accidents
colourCount = length(unique(upper$Event))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

m <- ggplot(upper, aes(numAccidents))
m + geom_histogram(aes(fill=Event), binwidth = 20) + scale_fill_manual(values = getPalette(colourCount)) + theme_dark() + ggtitle("Accident Causes in High-Risk Establishments") + ylab("Accident Count") + xlab("")


