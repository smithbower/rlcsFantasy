## ###########################################################
## ###########################################################
## updatePlayerMatchStats.r                                  #
##                                                           #
## Scrape the octane.gg API for player stats from each match,#
## allowing us to compute variance for each player. Saves the#
## output to a CSV file for later use.                       #
##                                                           #
##                                                           #
## April 9th, 2019                                           #
## Stephen Smithbower (smithy.s@gmail.com)                   #
## ###########################################################
## ###########################################################
library(httr)
library(jsonlite)
library(dplyr)

dfPlayerData <- setNames(data.frame(matrix(ncol = 14, nrow = 0)), 
                         c("Player", "GP", "Score", "ScoreSqr", "Goals", "GoalsSqr", 
                           "Assists", "AssistsSqr", "Saves", "SavesSqr", "Shots", 
                           "ShotsSqr", "AdjustedScore", "AdjustedScoreSqr"))

## Week 1
matchIDs <- c(1740101, 1740102, 1740103, 1740104, 1740105) ## NA
matchIDs <- c(matchIDs, 1750101, 1750102, 1750103, 1750104, 1750105) ## EU

## Week 2
## Week 3
## Week 4
## Week 5

for (matchID in matchIDs)
{
  print(matchID)
  
  for (team in 1:2)
  {
    if (team == 1) ## Eugh, octane why do you do this to me? Different URLs for each team.
      apiEndpoint <- "https://api.octane.gg/api/match_scoreboard_one/"
    else
      apiEndpoint <- "https://api.octane.gg/api/match_scoreboard_two/"
    
    
    mUrl <- paste(apiEndpoint, matchID, sep="")
    mUrl <- paste(mUrl, "/1", sep="")
    matchDataResp <- GET(mUrl)
    
    matchData <- fromJSON(content(matchDataResp, as = "text"))$data
    matchData <- matchData[c("Player", "Score", "Goals", "Assists", "Saves", "Shots", "MVP")]
    
    ## Empty row to just add names.
    for (i in 1:3) ## Ignore the 4th row which is the team total.
    {
      dfPlayerData[matchData$Player[i],1] <- matchData[i,1]
    }
    
    for (i in 1:5)
    {
      page <- paste("/", i, sep="")
      mUrl <- paste(apiEndpoint, matchID, sep="")
      mUrl <- paste(mUrl, page, sep="")
      gameDataResp <- GET(mUrl)
      
      gameData <- fromJSON(content(gameDataResp, as = "text"))$data
      gameData <- gameData[c("Player", "Score", "Goals", "Assists", "Saves", "Shots", "MVP")]
      
      for (j in 1:3)
      {
        pID <- which(dfPlayerData$Player == gameData$Player[j])
        
        dfPlayerData[pID, 2] <- sum(c(as.numeric(dfPlayerData[pID, 2]), 1), na.rm = TRUE)
        
        for (k in 0:4)
        {
          sumID <- (2*k) + 3
          sqrID <- (2*k) + 4
          gameID <- k + 2
          
          dfPlayerData[pID, sumID] <- sum(c(as.numeric(dfPlayerData[pID, sumID]), as.numeric(gameData[j, gameID])), na.rm = TRUE)
          dfPlayerData[pID, sqrID] <- sum(c(as.numeric(dfPlayerData[pID, sqrID]), (as.numeric(gameData[j, gameID]^2))), na.rm = TRUE)
        }
        
        ## Compute adjusted score seperately - this score is the result of clears, touches, faceoffs, etc and have
        ## the contribution of shots, goals, saves, assists, mvp, etc removed.
        ##
        ## Goals, Assits, Saves, Shots, MVP
        pts <- as.numeric(gameData[j, 2])
        ptsContrib <- (gameData[j, 3] * 100) + (gameData[j, 4] * 50) + (gameData[j, 5] * 50) + 
                      (gameData[j, 6] * 20) + (gameData[j, 7] * 100)
        adjustedPts <- pts - ptsContrib
        
        dfPlayerData[pID, "AdjustedScore"]  <- sum(c(dfPlayerData[pID, "AdjustedScore"], adjustedPts), na.rm = TRUE)
        dfPlayerData[pID, "AdjustedScoreSqr"] <- sum(c(dfPlayerData[pID, "AdjustedScoreSqr"], adjustedPts^2), na.rm = TRUE)
      }
    }
  }
}


write.csv(dfPlayerData, "season7.csv")

