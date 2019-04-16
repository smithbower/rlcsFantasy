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

## Include common scripts/functions.
source("common.R")

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


