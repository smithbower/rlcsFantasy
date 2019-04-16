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

estConf95 <- function(m, s, n) ##See: http://jse.amstat.org/v13n1/olsson.html
{
  return(c(m + 1.96 * sqrt((s^2/n)), max(0, m - 1.96 * sqrt((s^2/n)))))
}



## Params
season <- 7


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
## 100 - (0.2 * nWeek).
ogDataWeight <- 1.0 - (0.2 * length(weeklyPlayerData))


## Compute mean and stdv for each player using real data, so we can do
## simulated stats for pending weeks.
for (week in 1:length(weeklyPlayerData))
{
  data <- weeklyPlayerData[[week]]
  
  for (pID in 1:nrow(data))
  {
    ogID <- which(as.character(playerScores$Player) == as.character(weeklyPlayerData[[week]][pID, "Player"]))
    if (length(ogID) == 0)
      next()
    
    if (week > 1) ## Keep running totals throughout the season.
    {
      weeklyPlayerData[[week]][pID, "GP"]         <- weeklyPlayerData[[week - 1]][pID, "GP"]          + weeklyPlayerData[[week]][pID, "GP"]
      weeklyPlayerData[[week]][pID, "Score"]      <- weeklyPlayerData[[week - 1]][pID, "Score"]       + weeklyPlayerData[[week]][pID, "Score"]
      weeklyPlayerData[[week]][pID, "ScoreSqr"]   <- weeklyPlayerData[[week - 1]][pID, "ScoreSqr"]    + weeklyPlayerData[[week]][pID, "ScoreSqr"]
      weeklyPlayerData[[week]][pID, "Goals"]      <- weeklyPlayerData[[week - 1]][pID, "Goals"]       + weeklyPlayerData[[week]][pID, "Goals"]
      weeklyPlayerData[[week]][pID, "GoalsSqr"]   <- weeklyPlayerData[[week - 1]][pID, "GoalsSqr"]    + weeklyPlayerData[[week]][pID, "GoalsSqr"]
      weeklyPlayerData[[week]][pID, "Assists"]    <- weeklyPlayerData[[week - 1]][pID, "Assists"]     + weeklyPlayerData[[week]][pID, "Assists"]
      weeklyPlayerData[[week]][pID, "AssistsSqr"] <- weeklyPlayerData[[week - 1]][pID, "AssistsSqr"]  + weeklyPlayerData[[week]][pID, "AssistsSqr"]
      weeklyPlayerData[[week]][pID, "Saves"]      <- weeklyPlayerData[[week - 1]][pID, "Saves"]       + weeklyPlayerData[[week]][pID, "Saves"]
      weeklyPlayerData[[week]][pID, "SavesSqr"]   <- weeklyPlayerData[[week - 1]][pID, "SavesSqr"]    + weeklyPlayerData[[week]][pID, "SavesSqr"]
      weeklyPlayerData[[week]][pID, "Shots"]      <- weeklyPlayerData[[week - 1]][pID, "Shots"]       + weeklyPlayerData[[week]][pID, "Shots"]
      weeklyPlayerData[[week]][pID, "ShotsSqr"]   <- weeklyPlayerData[[week - 1]][pID, "ShotsSqr"]    + weeklyPlayerData[[week]][pID, "ShotsSqr"]
    }
    
    weeklyPlayerData[[week]][pID, "price"] <- playerScores[ogID, "price"]
    
    weeklyPlayerData[[week]][pID, "ScoreMean"] <- (playerScores[ogID, "Score"] / playerScores[ogID, "G"] * 1.0 - (0.2 * week)) + (data[pID, "Score"] / data[pID, "GP"] * (0.2 * week))
    weeklyPlayerData[[week]][pID, "ScoreStdv"] <- calcStdv(data[pID, "ScoreSqr"], data[pID, "Score"], data[pID, "GP"])
    ci <- estConf95(weeklyPlayerData[[week]][pID, "ScoreMean"], weeklyPlayerData[[week]][pID, "ScoreStdv"], data[pID, "GP"])
    weeklyPlayerData[[week]][pID, "ScoreMax"] <- ci[1]
    weeklyPlayerData[[week]][pID, "ScoreMin"] <- ci[2]
    
    weeklyPlayerData[[week]][pID, "GoalsMean"] <- (playerScores[ogID, "Goals"] / playerScores[ogID, "G"] * 1.0 - (0.2 * week)) + (data[pID, "Goals"] / data[pID, "GP"] * (0.2 * week))
    weeklyPlayerData[[week]][pID, "GoalsStdv"] <- calcStdv(data[pID, "GoalsSqr"], data[pID, "Goals"], data[pID, "GP"])
    ci <- estConf95(weeklyPlayerData[[week]][pID, "GoalsMean"], weeklyPlayerData[[week]][pID, "GoalsStdv"], data[pID, "GP"])
    weeklyPlayerData[[week]][pID, "GoalsMax"] <- ci[1]
    weeklyPlayerData[[week]][pID, "GoalsMin"] <- ci[2]
    
    weeklyPlayerData[[week]][pID, "AssistsMean"] <- (playerScores[ogID, "Assists"] / playerScores[ogID, "G"] * 1.0 - (0.2 * week)) + (data[pID, "Assists"] / data[pID, "GP"] * (0.2 * week))
    weeklyPlayerData[[week]][pID, "AssistsStdv"] <- calcStdv(data[pID, "AssistsSqr"], data[pID, "Assists"], data[pID, "GP"])
    ci <- estConf95(weeklyPlayerData[[week]][pID, "AssistsMean"], weeklyPlayerData[[week]][pID, "AssistsStdv"], data[pID, "GP"])
    weeklyPlayerData[[week]][pID, "AssistsMax"] <- ci[1]
    weeklyPlayerData[[week]][pID, "AssistsMin"] <- ci[2]
    
    weeklyPlayerData[[week]][pID, "SavesMean"] <- (playerScores[ogID, "Saves"] / playerScores[ogID, "G"] * 1.0 - (0.2 * week)) + (data[pID, "Saves"] / data[pID, "GP"] * (0.2 * week))
    weeklyPlayerData[[week]][pID, "SavesStdv"] <- calcStdv(data[pID, "SavesSqr"], data[pID, "Saves"], data[pID, "GP"])
    ci <- estConf95(weeklyPlayerData[[week]][pID, "SavesMean"], weeklyPlayerData[[week]][pID, "SavesStdv"], data[pID, "GP"])
    weeklyPlayerData[[week]][pID, "SavesMax"] <- ci[1]
    weeklyPlayerData[[week]][pID, "SavesMin"] <- ci[2]
    
    weeklyPlayerData[[week]][pID, "ShotsMean"] <- (playerScores[ogID, "Shots"] / playerScores[ogID, "G"] * 1.0 - (0.2 * week)) + (data[pID, "Shots"] / data[pID, "GP"] * (0.2 * week))
    weeklyPlayerData[[week]][pID, "ShotsStdv"] <- calcStdv(data[pID, "ShotsSqr"], data[pID, "Shots"], data[pID, "GP"])
    ci <- estConf95(weeklyPlayerData[[week]][pID, "ShotsMean"], weeklyPlayerData[[week]][pID, "ShotsStdv"], data[pID, "GP"])
    weeklyPlayerData[[week]][pID, "ShotsMax"] <- ci[1]
    weeklyPlayerData[[week]][pID, "ShotsMin"] <- ci[2]
  }
  
  ## Append expected scores for attacker, defender, and midfielder positions for each player.
  f <- apply(weeklyPlayerData[[week]][,c('GP', 'Score', 'Goals', 'Assists', 'Shots', 'Saves')], 1, function(x) scoreFunc(x[1], x[2], x[3], x[4], x[5], x[6]))
  f <- t(f)
  weeklyPlayerData[[week]] <- cbind(weeklyPlayerData[[week]], attackerScore = f[,1], midfieldScore = f[,2], defenderScore = f[,3])
  
  ## Append the minimum and maximum expected scores for attacker, defender, and midfielder positions for each player, based on 95% CI.
  f <- apply(weeklyPlayerData[[week]][,c('GP', 'ScoreMax', 'GoalsMax', 'AssistsMax', 'ShotsMax', 'SavesMax')], 1, function(x) scoreFunc(1, x[2], x[3], x[4], x[5], x[6]))
  f <- t(f)
  weeklyPlayerData[[week]] <- cbind(weeklyPlayerData[[week]], maxAttackerScore = f[,1], maxMidfieldScore = f[,2], maxDefenderScore = f[,3])
  
  f <- apply(weeklyPlayerData[[week]][,c('GP', 'ScoreMin', 'GoalsMin', 'AssistsMin', 'ShotsMin', 'SavesMin')], 1, function(x) scoreFunc(1, x[2], x[3], x[4], x[5], x[6]))
  f <- t(f)
  weeklyPlayerData[[week]] <- cbind(weeklyPlayerData[[week]], minAttackerScore = f[,1], minMidfieldScore = f[,2], minDefenderScore = f[,3])
  
  
  ## Remove any players that don't have OG data.
  weeklyPlayerData[[week]] <- weeklyPlayerData[[week]][complete.cases(weeklyPlayerData[[week]]),]
}


## #############################################################################
## OPTIMIZATION                                                               ##
## #############################################################################
selectTeam <- function(initialTeam, data)
{
  numPlayers <- nrow(data)
  
  gaResult <- ga(type              = "real-valued",
                 fitness           = function(x) estimatedScorePick2(x, playerScores, initialTeam),
                 suggestions       = initialTeam,
                 lower             = c(1,1,1,1,1,1),
                 upper             = c(numPlayers,numPlayers,numPlayers,numPlayers,numPlayers,numPlayers),
                 popSize           = 400,
                 maxiter           = 1000000, 
                 run               = 500,
                 optim             = TRUE,
                 names             = c("atk1", "atk2", "def1", "def2", "mid1", "mid2"),
                 seed              = 1989
  )
  
  return(gaResult)
}

estimatedScorePick2 <- function(s, prf, initialTeam)
{
  playerScores <- prf
  s <- floor(s)
  
  #Make sure we don't select the same player more than once.
  uniquePlayers <- unique(s)
  
  if (length(s) != length(uniquePlayers))
    return(0)
  
  #Make sure we don't change more than 2 players at a time - though we don't
  #care about changing positions.
  if (sum(initialTeam %in% s, na.rm = TRUE) < 4)
    return(0)
  
  #Make sure we don't spend more than our budget.
  maxBudget = 10000
  totalCost = playerScores[s[1],]$price +
    playerScores[s[2],]$price +
    playerScores[s[3],]$price +
    playerScores[s[4],]$price +
    playerScores[s[5],]$price +
    playerScores[s[6],]$price
  
  if (totalCost > maxBudget)
    return(0)
  
  
  score <- playerScores[s[1],]$attackScore +
    playerScores[s[2],]$attackScore +
    playerScores[s[3],]$defenderScore +
    playerScores[s[4],]$defenderScore +
    playerScores[s[5],]$midfieldScore +
    playerScores[s[6],]$midfieldScore
  
  return(score)
}
