library(httr)
library(jsonlite)
library(ggplot2)
library(GA)
library(parallel)
library(tictoc)
library(DataCombine)

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
  eff <- c((y[x[1],]$attackerScore / y[x[1],]$price),
           (y[x[1],]$attackerScore / y[x[1],]$price),
           (y[x[1],]$defenderScore / y[x[1],]$price),
           (y[x[1],]$defenderScore / y[x[1],]$price),
           (y[x[1],]$midfieldScore / y[x[1],]$price),
           (y[x[1],]$midfieldScore / y[x[1],]$price))
  
  avg <- mean(eff)
  
  return(avg)
}


## teamNamesToIDs
## ###########################################################
## Converts a vector of player names to their IDs
##
## return: A vector of integers.
teamNamesToIDs <- function(team, data)
{
  t <- c(which(data$Player == as.character(team[1])), 
         which(data$Player == as.character(team[2])), 
         which(data$Player == as.character(team[3])), 
         which(data$Player == as.character(team[4])),
         which(data$Player == as.character(team[5])), 
         which(data$Player == as.character(team[6]))) 
  
  return(t)
}


## estimateScore
## ###########################################################
## Computes the sum of scores for a vector of player IDs.
##
## In order: attacker1, attacker2, defender1, defender2,
##           midfielder1, midfielder2
##
## return: A vector of integers.
estimateScore <- function(team, data)
{
  score <- data[team[1],]$attackerScore +
           data[team[2],]$attackerScore +
           data[team[3],]$defenderScore +
           data[team[4],]$defenderScore +
           data[team[5],]$midfieldScore +
           data[team[6],]$midfieldScore
  
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

playerScores <- cbind(players, attackerScore = f[,1], midfieldScore = f[,2], defenderScore = f[,3])


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