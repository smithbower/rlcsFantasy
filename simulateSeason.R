calcStdv <- function(S2, S, N)
{
  return(sqrt((S2 - (S^2) / N) / (N - 1)))
}

calcLogMean <- function(m, s)
{
  return(log(m) - 0.5 * log((s/m)^2 + 1))
}

calcLogStdv <- function(m, s)
{
  return(sqrt(log((s/m)^2 + 1)))
}



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

## Compute mean and stdv for each player using real data, so we can do
## simulated stats for pending weeks.
for (week in 1:length(weeklyPlayerData))
{
  data <- weeklyPlayerData[[week]]
  
  for (pID in 1:nrow(data))
  {
    weeklyPlayerData[[week]][pID, "GoalsMean"] <- data[pID, "Goals"] / data[pID, "GP"]
    weeklyPlayerData[[week]][pID, "GoalsStdv"] <- calcStdv(data[pID, "GoalsSqr"], data[pID, "Goals"], data[pID, "GP"])
    
    weeklyPlayerData[[week]][pID, "AssistsMean"] <- data[pID, "Assists"] / data[pID, "GP"]
    weeklyPlayerData[[week]][pID, "AssistsStdv"] <- calcStdv(data[pID, "AssistsSqr"], data[pID, "Assists"], data[pID, "GP"])
    
    weeklyPlayerData[[week]][pID, "SavesMean"] <- data[pID, "Saves"] / data[pID, "GP"]
    weeklyPlayerData[[week]][pID, "SavesStdv"] <- calcStdv(data[pID, "SavesSqr"], data[pID, "Assists"], data[pID, "GP"])
    
    weeklyPlayerData[[week]][pID, "ShotsMean"] <- data[pID, "Shots"] / data[pID, "GP"]
    weeklyPlayerData[[week]][pID, "ShotsStdv"] <- calcStdv(data[pID, "ShotsSqr"], data[pID, "Shots"], data[pID, "GP"])
    
    weeklyPlayerData[[week]][pID, "AdjustedScoreMean"] <- data[pID, "AdjustedScore"] / data[pID, "GP"]
    weeklyPlayerData[[week]][pID, "AdjustedScoreStdv"] <- calcStdv(data[pID, "AdjustedScoreSqr"], data[pID, "AdjustedScore"], data[pID, "GP"])
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