## ###########################################################
## ###########################################################
## selectTeam.r                                              #
##                                                           #
## Use a genetic algorithm to select the most cost-effective #
## team for RLCS Season 7.                                   #
##                                                           #
## Solves for selecting players given cost, historical stats,#
## score multipliers for field position, and salary cap.     #
##                                                           #
## Data comes from octane.gg.                                #
##                                                           #
##                                                           #
## April 8th, 2019                                           #
## Stephen Smithbower (smithy.s@gmail.com)                   #
## ###########################################################
## ###########################################################
library(httr)
library(jsonlite)
library(ggplot2)
library(GA)
library(parallel)
library(tictoc)


## ###########################################################
## ###########################################################
## Helper Functions                                          #
## ###########################################################
## ###########################################################

## scoreFunc
## ###########################################################
## Calculates the number of points a player is expected to 
## generate in an attacker, midfield, and defender position.
##
## return: a list (attack, midfield, defender)
scoreFunc <- function(gp, score, goals, assists, saves, shots)
{
  baseScore <- score + (goals * 50) + (assists * 25) + (saves * 25) + (shots * 15)
  
  return( c((baseScore + (goals * 50)) / gp, (baseScore + (assists * 25)) / gp, (baseScore + (saves * 15)) / gp) )
}

## eff
## ###########################################################
## Calculate's a team's cost/score efficiency.
##
## return: A float.
eff <- function(x, y)
{
  eff <- c((y[x[1],]$attackScore / y[x[1],]$price),
           (y[x[1],]$attackScore / y[x[1],]$price),
           (y[x[1],]$defenderScore / y[x[1],]$price),
           (y[x[1],]$defenderScore / y[x[1],]$price),
           (y[x[1],]$midfieldScore / y[x[1],]$price),
           (y[x[1],]$midfieldScore / y[x[1],]$price))
  
  avg <- mean(eff)
  
  return(avg)
}

## estimatedScore
## ###########################################################
## Fitness function used for genetic algorithm. Given a team
## of players, their positions, and their expected points,
## return the total estimated score for the team.
##
## s:   List(atk1, atk2, def1, def2, mid1, mid2) of player IDs
## prf: List of players and their attack, defense, and
##      midfield scores.
##
## return: An expected score for a given team. Illegal teams
##         that are blow the spending cap, or have duplicate
##         players, are penalized with a score of 0.
estimatedScore <- function(s, prf)
{
  playerScores <- prf
  s <- floor(s)
  
  #Make sure we don't select the same player more than once.
  uniquePlayers <- unique(s)
  
  if (length(s) != length(uniquePlayers))
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


## ###########################################################
## ###########################################################
## Data Prep                                                 #
## ###########################################################
## ###########################################################

## Pull career data for players from octane.gg using their API.
## This has been saved to a flat file for stability/reproducibility. 
#playerResponse <- GET("https://api.octane.gg/api/stats/players/career/totals/career")
#players <- fromJSON(content(playerResponse, as = "text"))$data

## Read in cached original player data.
players <- read.csv("ogPlayerData.csv")

## Append expected scores for attacker, defender, and midfielder positions for each player.
playerScores <- apply(players[,c('G', 'Score', 'Goals', 'Assists', 'Shots', 'Saves')], 1, function(x) scoreFunc(x[1], x[2], x[3], x[4], x[5], x[6]))
f <- t(playerScores)

playerScores <- cbind(players, attackScore = f[,1], midfieldScore = f[,2], defenderScore = f[,3])


## Filter out players who have played fewer than 150 career games.
## Assume that we don't have enough data to be confident in results,
## since the data we do have is just totals.. don't have per-match
## statistics so we can't compute variability.
playerScores <- playerScores[playerScores$G > 150,]

## Add manual price data.
## Would be nice to find an API for this..
playerScores[playerScores$Player == "Express", "price"] <- 2050
playerScores[playerScores$Player == "CJCJ", "price"] <- 2050
playerScores[playerScores$Player == "kuxir97", "price"] <- 2025
playerScores[playerScores$Player == "Kaydop", "price"] <- 2025
playerScores[playerScores$Player == "Torsos", "price"] <- 1975
playerScores[playerScores$Player == "Julz", "price"] <- 1950
playerScores[playerScores$Player == "SquishyMuffinz", "price"] <- 1925
playerScores[playerScores$Player == "GarretG", "price"] <- 1925
playerScores[playerScores$Player == "JKnaps", "price"] <- 1900
playerScores[playerScores$Player == "Delusion", "price"] <- 1875
playerScores[playerScores$Player == "Kamii", "price"] <- 1875
playerScores[playerScores$Player == "shadey", "price"] <- 1875
playerScores[playerScores$Player == "Express", "price"] <- 2050
playerScores[playerScores$Player == "Gimmick", "price"] <- 1850
playerScores[playerScores$Player == "JSTN", "price"] <- 1850
playerScores[playerScores$Player == "ViolentPanda", "price"] <- 1825
playerScores[playerScores$Player == "Turbopolsa", "price"] <- 1825
playerScores[playerScores$Player == "Siki", "price"] <- 1825
playerScores[playerScores$Player == "Bango", "price"] <- 1825
playerScores[playerScores$Player == "ZeN", "price"] <- 1775
playerScores[playerScores$Player == "Wonder", "price"] <- 1775
playerScores[playerScores$Player == "Fairy Peak", "price"] <- 1750
playerScores[playerScores$Player == "EyeIgnite", "price"] <- 1725
playerScores[playerScores$Player == "Lethamyr", "price"] <- 1700
playerScores[playerScores$Player == "Drippay", "price"] <- 1700
playerScores[playerScores$Player == "Yueko", "price"] <- 1675
playerScores[playerScores$Player == "Chicago", "price"] <- 1650
playerScores[playerScores$Player == "Kronovi", "price"] <- 1600
playerScores[playerScores$Player == "Torment", "price"] <- 1600
playerScores[playerScores$Player == "Speed", "price"] <- 1575
playerScores[playerScores$Player == "AyyJayy", "price"] <- 1550
playerScores[playerScores$Player == "Bluey", "price"] <- 1550
playerScores[playerScores$Player == "remkoe", "price"] <- 1550
playerScores[playerScores$Player == "Satthew", "price"] <- 1550
playerScores[playerScores$Player == "Ferra", "price"] <- 1525
playerScores[playerScores$Player == "Karma", "price"] <- 1525
playerScores[playerScores$Player == "Tigreee", "price"] <- 1525
playerScores[playerScores$Player == "Chausette45", "price"] <- 1500
playerScores[playerScores$Player == "Deevo", "price"] <- 1500
playerScores[playerScores$Player == "Fireburner", "price"] <- 1500
playerScores[playerScores$Player == "Klassux", "price"] <- 1475
playerScores[playerScores$Player == "Ronaky", "price"] <- 1475
playerScores[playerScores$Player == "CorruptedG", "price"] <- 1450
playerScores[playerScores$Player == "Metsanauris", "price"] <- 1450
playerScores[playerScores$Player == "Rizzo", "price"] <- 1450
playerScores[playerScores$Player == "miztik", "price"] <- 1425
playerScores[playerScores$Player == "Allushin", "price"] <- 1400
playerScores[playerScores$Player == "JWismont", "price"] <- 1400
playerScores[playerScores$Player == "DudeWithTheNose", "price"] <- 1375
playerScores[playerScores$Player == "Tadpole", "price"] <- 1250

## Remove players we don't have price data for - can't pick them anyway.
playerScores <- playerScores[-which(is.na(playerScores$price)),]
numPlayers = nrow(playerScores)


## ###########################################################
## ###########################################################
## Optimization - Find best team                             #
## ###########################################################
## ###########################################################

## Use a genetic algorithm to pick the team with the highest expected
## total number of points, constrained by both the salary cap and
## the point multipliers for attacker, defender, and midfielder
## positions.
tic("Optimizing")
gaResult <- ga(type            = "real-valued",
             fitness           = function(x) estimatedScore(x, playerScores),
             lower             = c(1,1,1,1,1,1),
             upper             = c(numPlayers,numPlayers,numPlayers,numPlayers,numPlayers,numPlayers),
             popSize           = 400,
             maxiter           = 1000000, 
             run               = 500,
             optim             = TRUE,
             names             = c("atk1", "atk2", "def1", "def2", "mid1", "mid2"),
             seed              = 1989
             )
toc()


## ###########################################################
## ###########################################################
## Compute Estimated Scores for Stephen, Michael, and Dave   #
## ###########################################################
## ###########################################################
selectedPlayerIds = floor(gaResult@solution[end(gaResult@solution)[1],])
playerScores[selectedPlayerIds,]
estimatedScore(selectedPlayerIds, playerScores)


## Michael's estimated team score:
michael <- c(which(playerScores$Player == "CJCJ"), 
             which(playerScores$Player == "Kaydop"), 
             which(playerScores$Player == "remkoe"), 
             which(playerScores$Player == "Tadpole"), #Alex161 <- not listed in our data, used closest price match
             which(playerScores$Player == "Kronovi"), 
             which(playerScores$Player == "Klassux")) 

estimatedScore(michael, playerScores)


## Daves's estimated team score:
dave <- c(which(playerScores$Player == "ZeN"), #Scrub Killa 
          which(playerScores$Player == "Kaydop"), 
          which(playerScores$Player == "Rizzo"), 
          which(playerScores$Player == "CorruptedG"), 
          which(playerScores$Player == "Lethamyr"), 
          which(playerScores$Player == "Speed"))

estimatedScore(dave, playerScores)


