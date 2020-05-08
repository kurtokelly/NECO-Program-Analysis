#Get Working Directory 
getwd()

#Set Working Directory
setwd("C:/Users/kmitc/Documents")

#Import 2019 NECO Account List
NECO <-read.csv("2019 NECO Account List.csv",TRUE,",")

#Make sure that our csv file is now a dataset.
class(NECO)

#Taking a closer look at the dataset and the column headers.
head(NECO)

#Calculate Summary Statistics for all Collums of Dataset
summary(NECO)

#Calculate Summary Statistics for Fare Value
summary(NECO$Fare.Value)

#Create a scatter Plot of Active Riders and Active Cards 
plot(NECO$Active.Riders, NECO$Active.Cards, 
	xlab='Active Riders', 
	ylab='Active Cards',
	main='Account Participation Summary',
	pch=20,
	col='green')

points(NECO$Active.Riders[NECO$Sponsor.SLA=='A'],
	NECO$Active.Riders[NECO$Sponsor.SLA=='A'],
	pch=20,
	col='blue')
points(NECO$Active.Riders[NECO$Sponsor.SLA=='B'],
	NECO$Active.Riders[NECO$Sponsor.SLA=='B'],
	pch=20,
	col='red')

#Create a Histogram of Active Cards compared to Accounts
hist(NECO$Active.Cards,
	main="Cards by Account",
	xlab="Active Cards", 
	ylab="Number of Accounts",
	xlim=c(0,4000),
	ylim=c(0,20))

#Create Boxplots to Show Relationships between Variables
class(NECO$Funding.Type)
table(NECO$Funding.Type)

boxplot(NECO$Active.Cards ~ NECO$Funding.Type,
	xlab="Funding Type",
	ylab="Active Cards")

boxplot(NECO$Tap.Count ~ NECO$Sponsor.Size,
	xlab="Sponsor Size",
	ylab="Tap Count",
	col='blue')

#Chi-Squared test for Funding Type and Sponsor SLA
chisq.test(table(NECO$Funding.Type))

chisq.test(table$NECO(Sponsor.SLA))

table(NECO$Sponsor.SLA, NECO$Funding.Type)
chisq.test(table(NECO$Sponsor.SLA, NECO$Funding.Type))

table(NECO$Sponsor.SLA, NECO$City)
chisq.test(table(NECO$Sponsor.SLA, NECO$Funding.Type))

table(NECO$Sponsor.SLA, NECO$Sponsor.Size)
chisq.test(table(NECO$Sponsor.SLA, NECO$Sponsor.Size))

table(NECO$Funding.Type, NECO$City)
chisq.test(table(NECO$Funding.Type, NECO$City))

table(NECO$Funding.Type, NECO$Sponsor.Size)
chisq.test(table(NECO$Funding.Type, NECO$Sponsor.Size))

table(NECO$City, NECO$Sponsor.Size)
chisq.test(table(NECO$City, NECO$Sponsor.Size))

#Create a two-sample t-test Ho: HOA Funding is equal to NonHOA funding
#(397)
mean(NECO$Funding.Type=='HOA')
sdHOA <- sd(NECO$Funding.Type=='HOA')
sdHOA

mean(NECO$Funding.Type=='NO HOA')
sdNoHOA <-sd(NECO$Funding.Type=='NO HOA')
sdNoHOA

sdHOA / sdNoHOA

t.test(x=NECO$Funding.Type=='HOA', y= NECO$Funding.Type=='NO HOA',
alternative="two.sided", conf.level=0.95, var.equal=TRUE)

#Plot relationship between Funding Type and City
boxplot(NECO$Funding.Type, NECO$City)

#Create a Linear Regression Model
NECO_regression = lm(formula= Tap.Count ~ Sponsor.SLA, data= NECO)

#Use Summary Function to call the results of the regression model
summary(NECO_regression)
 
#Create a Multiple Linear Regression Model using Tap Count   
#as the dependant variable and using the Sponsor SLA, Active Riders,
#Active Cards, City and Funding Type as the 
#independant variables. (490)

NECO_regression2 = lm(formula= Tap.Count ~ Sponsor.SLA
	+ Active.Riders + Active.Cards + City + Funding.Type,
	data= NECO)

#Use Summary function to call the results of the regression
#model titled "NECO_regression2"
summary(NECO_regression2)
