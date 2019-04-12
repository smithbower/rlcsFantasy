## Params
season <- 7
numSimulations <- 1


## Load base player data from pre-season.
ogPlayerData <- read.csv("ogPlayerData.csv", TRUE)

## Load each week's stats.
weeklyPlayerData <- list(5)
for (i in 1:5)
{
  fname <- paste0("season", season, "_week", i, ".csv")
  
  if (file.exists(fname))
    weeklyPlayerData[[i]] <- read.csv(fname)
}

## Compute weighting for each week + OG data.. want season data to be weighted
## 100% by the end of the season, so each week is worth 20%. OG data is worth
## 100 - (0.2 & nWeek).
ogDataWeight <- 1.0 - (0.2 * length(weeklyPlayerData))

playerData <- data.frame("Player" = NA, "GP" = NA, "GoalsMean" = NA, "GoalsStdv" = NA, "AssistsMean" = NA, "AssitsStdv" = NA,
                         "SavesMean" = NA, "SavesStdv" = NA, "ShotsMean" = NA, "ShotsStdv" = NA, "AdjustedScoreMean" = NA,
                         "AdjustedScoreStdv" = NA)

## Compute mean and variance for each player using real data, so we can do
## simulated stats for pending weeks.
for (week in 1:length(weeklyPlayerData))
{
  data <- weeklyPlayerData[[week]]
  
  for (pID in 1:nrow(data))
  {
    if (length(which(playerData["Player"] == data[pID,]["Player"])) == 0)
    {
      playerData[nrow(playerData) + 1, ]["Player"] <- data[pID,]["Player"]
    }
  }
  
  #(data[i, "AdjustedScoreVar"] - (data[i, "AdjustedScore"]^2/data[i, "GP"])) / (data[i, "GP"] - 1)
  
  ## Compute means and variance.
  for (i in 1:nrow(data))
  {
    weeklyPlayerData[[week]][i, "AdjustedScore"] <- (data[i, "AdjustedScore"] / data[i, "GP"])
    weeklyPlayerData[[week]][i, "AdjustedScoreVar"] <- (data[i, "AdjustedScoreVar"] / (data[i, "GP"] - 1))
  }
}

## #############################################################################
## MONTE CARLO SIMULATION
## #############################################################################
for (iterations in 1:numSimulations)
{
  
  ## Simulate player stats for each week.
  for (week in 1:5)
  {
    ## If we have real data for a week, use that. Otherwise we draw from 
    ## distribution.
    
  }
}