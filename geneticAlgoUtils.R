## Genetic Algo Component of Fox Duck
## Runs the genetic algo over successive generations
## Author: Nathan Danneman
## Created: August 28, 2016
## Last Edited: August 28, 2016

source("/Users/Nathan/ndgit/foxDuck/foxDuckUtils.R")

# How many ducks should be in play at any given time??
   # Hard cap at 1k ducks; randomly select which ones to reproduce
   # Selected evenly contribute to next generation
   # This will keep population capped if fox is too slow early on

# What rate of increase of fox?
  # .2 degree per step each generation to start?
  # parameterize later?

# MUTATION:
  # how many angles to alter?? 
  # and by how much??
  # let's have this part be slow: small changes to small number of turns

# a duck is a list
# with characteristics: id:Int, angles:vectorInt, genBorn:Int, parents:vectorInt

# currentDucks is a list of all ducks currently alive

## initialize

# Duck population
duckPop <- 500
# Where to store any duck that reproduced:
forebearsStorage <- list()
forebearsIDVector <- 0
# generation counter
generation <- 0
# how many mutations
maxNDeltas <- 25
# min and max of deltas:
minDelta <- -25; maxDelta <- 25
# first duckID
currDuckID <- 0
# Number of turns in a duck
numAngles <- 1000
# Degrees of Delta in a duck angle
degDelta <- 45
# Duck step size
duckStepSize <- 0.3
# Radius of pond
R <- 10
# initial fox speed in degrees/step
foxSpeed <- 2.0
# fox starting position
foxStartingPosition <- 90
# create and populate currentDucks
currentDucks <- list()
for(i in 1:duckPop){
  currentDucks[[i]] <- list()
  # increment duck ID
  currDuckID <- currDuckID + 1
  currentDucks[[i]]$id <- currDuckID 
  currentDucks[[i]]$angles <- newDuck(numAngles,degDelta)
  currentDucks[[i]]$genBorn <- generation
  currentDucks[[i]]$parents <- 0
  currentDucks[[i]]$duckFoxAngleDiff <- 0
}


for(g in 1:60){
  # increment generation
  generation <- generation + 1  

  # Compete every duck against the fox, tracking which ones survive
  angleAhead <-NULL
  survBool <- NULL
  for(i in 1:length(currentDucks)){
    out <- duckVersusFox(duckPath(currentDucks[[i]]$angles, duckStepSize, R), 
                         foxStartingPosition, foxSpeed, R, FALSE)
    survBool <- c(survBool, out$duckEscaped)
    angleAhead <- c(angleAhead, out$duckFoxAngleDiff)
  }
  
  
  
  # which ones survived
  survIndex <- which(survBool == TRUE)
  # only keep lead information for survivors
  angleAhead <- angleAhead[survBool==TRUE]
  
  if(length(survIndex) == 0){
    print("ALL THE DUCKS ARE DEAD!!!")
    break
  }
  
  # plot one chase for fun!
  toPlot <- sample(survIndex, 1)
  duckVersusFox(duckPath(currentDucks[[toPlot]]$angles, duckStepSize, R), 
                foxStartingPosition, foxSpeed, R, TRUE)
  text(-9, 11.5, paste("Generation: ", generation, sep=""))
  
  
  # put surviving ducks in a different container, called 'survivingDucks'
  survivingDucks <- list()
  for(j in 1:length(survIndex)){
    survivingDucks[[j]] <- currentDucks[[survIndex[j]]]
  }
  
  # re-make 'currentDucks' with survivors
  # make new ones as necessary to get back to full duckPop
  currentDucks <- list()
  currentDucks <- survivingDucks
  while(length(currentDucks) < duckPop){  # until we have enough ducks...
    # increment current duck ID
    currDuckID <- currDuckID + 1
    # pick a duck from surviving ducks on the basis of how much it beat the fox by
    # add to list of forebears if not already there
    toRepro <- sample(1:length(survivingDucks), size=1, prob=angleAhead)
    if(!survivingDucks[[toRepro]]$id %in% forebearsIDVector){
      forebearsStorage[[length(forebearsStorage) + 1]] <- survivingDucks[[toRepro]]
      forebearsIDVector <- c(forebearsIDVector, survivingDucks[[toRepro]]$id)
    }
    # make a new duck
    newDuck <- duckBiology(survivingDucks[[toRepro]], 
                           maxNDeltas, minDelta, maxDelta, generation, currDuckID)  
    # append to current
    currentDucks[[length(currentDucks) + 1]] <- newDuck
  }
  
  # increase fox speed in proportion to number of ducks that survived:
  foxSpeedIncrease <-  (2.0 * length(which(survBool==TRUE))/duckPop) / generation 
  
  # Print out some info:
  print("")
  print(paste("Generation: ", generation, sep="") )
  print(paste("Fox Speed this generation: ", foxSpeed, sep="") )
  print("Table of survival boolean")
  print(table(survBool))
  print(paste("Increasing fox speed by: ", foxSpeedIncrease, " next generation.", sep=""))
  print("")
  
  foxSpeed <- foxSpeed + foxSpeedIncrease

 
}















