#This is Cindy's code to recreate the New York Yankees' payroll graph since no code was given in the R-blogger article.
#As an amateur coder, I am very proud of this fyi
library(Lahman)
library(ggplot2)
library(data.table)

#Subset Salaries data from the Lahman package for team payrolls.
#Add up individual players' salaries to get team salary
totalpayroll <- data.table(Salaries)
totalpayroll <- totalpayroll[yearID >=1985, sum(salary), by = list(yearID, teamID)]
totalpayroll[,year_team := paste(yearID, "_", teamID, sep="")]

#Make a new column of just the team and year in the SeriesPost data so we can eventually merge it with the payroll tables
#Since SeriesPost's row entries include two teams (winner and loser), we will later have to merge the payroll data separately
#to ensure that all the posts match.
SeriesPost <- data.table(SeriesPost)
SeriesPost <- SeriesPost[yearID >= 1985]
SeriesPost[,year_winner := paste(yearID, "_", teamIDwinner, sep="")]
SeriesPost[,year_loser := paste(yearID, "_", teamIDloser, sep="")]

#Set the keys to year_team and year_winner since we want to use that format to merge the graphs. 

#allow.cartesian ensures that NA entries are maintained (if a team does not make the post season, they would have salary data
#but no SeriesPost data)
setkey(totalpayroll, year_team)
setkey(SeriesPost, year_winner)
amalgamated <- SeriesPost[totalpayroll, allow.cartesian=TRUE]

#Do the same thing for the year_losers and year_team so we can match corresponding data.
setkey(SeriesPost, year_loser)
loss_amalgamated <- SeriesPost[totalpayroll, allow.cartesian=TRUE]

#Now we have two seperate tables of winners and losers' salaries and performance. Join the tables together
amalgamated <- rbind(loss_amalgamated, amalgamated, fill=TRUE)

#Set everything to a post-season loser so we can then change things into winners afterwards. This is much easier to blanket
#them all as No Post Season and then edit them into the actual entries
amalgamated <- amalgamated[,Win_or_Loss := "No Post Season"][,year_team := paste(paste(i.yearID, "_", teamID, sep=""))]
rm(loss_amalgamated, totalpayroll, SeriesPost)

#Format Win or Loss column
#Since we don't care who wins non-World Series rounds but want to identify those who made playoffs, we will first label
#all teams that made some type of round with "Post Season Loser"
amalgamated$Win_or_Loss[which(amalgamated$year_team==amalgamated$year_winner & amalgamated$round !="NA")] <- "Post Season Loser"
amalgamated$Win_or_Loss[which(amalgamated$year_team==amalgamated$year_loser & amalgamated$round != "NA")] <- "Post Season Loser"

#Isolate for World Series participants and their winner/loser status accordingly
amalgamated$Win_or_Loss[which(amalgamated$year_team==amalgamated$year_winner & amalgamated$round == "WS")] <- "World Series Champion"
amalgamated$Win_or_Loss[which(amalgamated$year_team==amalgamated$year_loser & amalgamated$round == "WS")] <- "World Series Loser"

#Format them to 5 columns so there is less excess data to deal with
amalgamated <- amalgamated[,list(year_team, Win_or_Loss, i.yearID, teamID, V1)]

#Sort the table so we can get rid of extra entries, because if a team lost in the World Series, they would show up
#as winners in the other rounds. We only want one entry for each team for each year and right now, there could be multiple 
#entries for one team in a single year. Sort first by the year_team to group all the data points together. Then sort reverse-
#alphabetically so World Series statuses appear at the top (because if a team makes the World Series, all other playoff entries
#are irrelevant).
setorder(amalgamated, year_team, -Win_or_Loss)

#Replace the amalgamated data table with only the first entry of each group by year_team so there is only one entry.
#Doing this also ensures that the World Series ranking is kept before any Playoff Loser or No Post Season status remains.
amalgamated <- amalgamated[,.SD[1],by=year_team]

#We have to isolate the Yankees data in an additional table in order to distinguish the Yankees in the graph
Yankees <- amalgamated[teamID=="NYA"]

##################

MyPlot <- ggplot(amalgamated, aes(x=i.yearID, y=V1)) +
  geom_point() +
  geom_point(data=Yankees, aes(x=i.yearID, y=V1, 
                 col=teamID, 
                 shape=Win_or_Loss), size=5) +
  geom_line(data=Yankees, aes(x=i.yearID, y=V1, 
                col=teamID), size=1.1)

#MyPlot <- ggplot(data=amalgamated, aes(x = i.yearID, y= V1, shape=Win_or_Loss, col=(teamID!="NYA")) +
#geom_point() +
#  + geom_line(aes(data=amalgamated$teamID=="NYA"))      

MyPlot <- MyPlot+
  ggtitle("MLB Payrolls: The New York Yankees vs All Other Teams") +
  xlab("Year") +
  ylab("Team Payroll (in U.S. Dollars)") +
  scale_colour_discrete(name="",
                        labels="Yankees") +
scale_shape_discrete(name="")

MyPlot

