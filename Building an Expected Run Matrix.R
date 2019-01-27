#Purpose: Use openWAR to extract multigame data and observe RunsFuture (an indicator of the expected number of runs that will be scored)
library(openWAR)
library(plyr)

#If you can't download openWAR, run these following 3 lines of code. Note: openWAR requires a package called Sxslt that is INCOMPATIBLE WITH R 3.1.2
install.packages("Sxslt", repos = "http://www.omegahat.org/R", type = "source")
require(devtools)
install_github("beanumber/openWAR")

#get data works similar to scrape() in pitchRx
ds <- getData(start="2013-05-14")

#subset data for only home run instances so we can find out how many home runs occurred on that data
subset(ds, event == "Home Run", select = c("game.Id", "batterId", "description"))

#By plotting all the data, we can see the location of every ball hitting the ground on that date and to an extent,
#the state of the inning (eg. how many runs were scored)
plot(data=ds)

##Modeling: RunsFuture##
#Create an empty data set called and subset it to the bottom of the first inning, because we will use the data about who is
#on base, how many outs, etc. to simulate the number and state of runs (RunsFuture) in order to predict the future.
#RunsFuture is calculated using a model of (RunsFuture ~ startCode + startOuts + startCode*startOuts), where startCode is the
#arrangement of players on base and startOuts is the number of outs in a half-inning at the beginning of a plate appearance.

gd <- gameday()
subset(gd$ds, inning == 1 & half == "bottom", select = c("runsFuture", "runsOnPlay",  "startCode", "startOuts", "description"))

##Building a Model for Expected Runs##
#We will now apply the brief example from above to showing how to create a full model.
#Take the MLBAM2013 data set from openWAR. Please note that these data sets may be removed in the future for legal reasons
#(in case you run the code and it doesn't load)
data(MLBAM2013)
nrow(MLBAM2013)

#Take the average of the sum of startCode and startOts for each half-inning, and create a new column for runsFuture.
ddply(MLBAM2013[,c("runsFuture", "startCode", "startOuts")], ~ startCode + startOuts, mean)

#This will yield that same results whether you run an ANOVA or linear regression. Run the following
#code to verify.
coef(aov(runsFuture ~ factor(startCode) * factor(startOuts), data=MLBAM2013))
coef(lm(runsFuture ~ factor(startCode) * factor(startOuts), data=MLBAM2013))

#getRunEx will return a function for all the variables within MLBAM2013
fit.rem <-= getRunEx(MLBAM2013)
#In our simulations, we assume that the inning starts at 0-0
fit.rem(baseCode = 0, outs = 0)

#create a subset of all of the NY Mets' games
nym <- subset(MLBAM2013, (home_teamID == 121 | away_teamID == 121) & field_teamId ~=121)
#apply fit.rem to both the codes and outs to get the run expectancy
nym$re.start <- mapply(fit.rem, nym$startCode, nym$startOuts)