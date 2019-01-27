library(XML)

#If the status of the game is not "In Progress", then the game is labelled "Final". Otherwise, we label whether or not
#it's the top or bottom of the inning. In our boxscore, we will also inlude details related to both teams' errors,
#hits, runs.

createBoxScore <- function(x) {
  status <- if(x$.attrs["status"] != "In Progress")
    "Final" else if(x$.attrs["top_inning"] == "Y")
      "Top" else "Bot"
  
  bs <- list(status = status,
             inning = as.numeric(x$.attrs["inning"]),
             away.team = x$.attrs["away_name_abbrev"],
             away.runs = as.numeric(x$.attrs["away_team_runs"]),
             away.hits = as.numeric(x$.attrs["away_team_hits"]),
             away.errors = as.numeric(x$.attrs["away_team_errors"]),
             home.team = x$.attrs["home_name_abbrev"],
             home.runs = as.numeric(x$.attrs["home_team_runs"]),
             home.hits = as.numeric(x$.attrs["home_team_hits"]),
             home.errors = as.numeric(x$.attrs["home_team_errors"]))
  class(bs) <- "boxscore"
  bs
}

#This dictates how the boxscore will appear
print.boxscore <- function(x, ...) {
  cat("     ", "R   ", "H  ", "E (",
      x$status, " ",
      x$inning, ")n",
      format(x$away.team, width = 3), " ",
      format(x$away.runs, width = 2), "  ",
      format(x$away.hits, width = 2), "  ",
      x$away.errors, "n",
      format(x$home.team, width = 3), " ",
      format(x$home.runs, width = 2), "  ",
      format(x$home.hits, width = 2), "  ",
      x$home.errors, "nn", sep = "")
}

#This describes the format of the boxscore data frame.
as.data.frame.boxscore <- function(x, row.names, optional, ...) {
  class(x) <- "list"
  as.data.frame(x)
}

#We create an error check for dates that we're searching, in case a future game is searched. Otherwise we can download
#the XML data from MLB, use xmlTreeParse to and prints out boxscores for games on "date".
#The xmlTreeParse function is useful in retrievinga and formatting a data set on the Internet into R, so that we can 
#create columns of data and then name them. When we use sapply() in this case, it is to identify the home and away teams
#as the labels on the MLB website are not included.

boxscore <- function(date = Sys.Date()) {
  if(date > Sys.Date())
    stop("Cannot retrieve scores from the future.")
  
  year  <- paste("year_", format(date, "%Y"), "/", sep = "")
  month <- paste("month_", format(date, "%m"), "/", sep = "")
  day   <- paste("day_", format(date, "%d"), "/", sep = "")
  
  xmlFile <-
    paste("http://gd2.mlb.com/components/game/mlb/",
          year, month, day, "miniscoreboard.xml", sep = "")
  xmlTree <- xmlTreeParse(xmlFile, useInternalNodes = TRUE)
  xp <- xpathApply(xmlTree, "//game")
  xmlList <- lapply(xp, xmlToList)
  
  bs.list <- lapply(xmlList, createBoxScore)
  names(bs.list) <-
    paste(sapply(bs.list, "[[", "away.team"),
          "@",
          sapply(bs.list, "[[", "home.team"))
  bs.list
}

######
#This will print any games happening right now:
boxscore()

# This will print boxscores for a determined date
boxscore(date = as.Date("2009-10-01"))

#This allows you to save the boxscores
bs <- boxscore()

#Convert to a data frame where each game is in a row
bs.df <- do.call(rbind, lapply(bs, as.data.frame))

#This will tell you the status of the saved games
table(bs.df$status)

#This tells you the number of innings that have been played in the saved games
sum(bs.df$inning, na.rm = TRUE)

#This describes home runs by home teams
sum(bs.df$home.runs, na.rm = TRUE)

#This describes home runs by away teams
sum(bs.df$away.runs, na.rm = TRUE)