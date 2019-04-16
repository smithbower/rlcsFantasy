source("common.R")

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

estConf95 <- function(m, s, n)
{
  return(c(m + 1.96 * sqrt((s^2/n)), max(0, m - 1.96 * sqrt((s^2/n)))))
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
  
  ## Append expected scores for attacker, defender, and midfielder positions for each player.
  f <- apply(weeklyPlayerData[[week]][,c('GP', 'Score', 'Goals', 'Assists', 'Shots', 'Saves')], 1, function(x) scoreFunc(x[1], x[2], x[3], x[4], x[5], x[6]))
  f <- t(f)
  
  weeklyPlayerData[[week]] <- cbind(weeklyPlayerData[[week]], attackScore = f[,1], midfieldScore = f[,2], defenderScore = f[,3])
}
