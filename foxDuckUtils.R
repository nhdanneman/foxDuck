## The fox and the duck
## Genetic algorithm to solve for the optimal strategy of duck
##   and the relative speed of duck-to-fox in the fox-duck problem
## Author: Nathan Danneman
## Created: August 24, 2016
## Last Edited: August 28, 2016

install.packages("NISTunits", dependencies = TRUE)
require(NISTunits)
require(plotrix)

NISTdegTOradian(45)
NISTradianTOdeg(pi/2)

# duck: 
# given an xy and previous direction, pick a new direction and move a units

# fox:
# given duck's current xy, move b units around the circle towards that angle

# assume circle is radius R and centered at 0,0

quickDist <- function(x1,y1,x2,y2){sqrt( (x2-x1)^2 + (y2-y1)^2 ) }

# generates a sequence of anges within +/- degDelta degrees of previous angle (in Deg)
newDuck <- function(N, degDelta){
  m <- rep(0, N)
  for(i in 2:N){
    m[i] <- m[i-1] + runif(1, -degDelta, degDelta)
    if(m[i] < 0){m[i] <- m[i] + 360}
    if(m[i] > 360){m[i] <- m[i] - 360}
  }
  m
}


# fox takes duck's current position, number of degrees per step, fox's start degrees
# fox moves min(stepSize, deltaToDuckAngle) in direction of duck
foxUpdate <- function(foxDegStart, xDuck, yDuck, stepSizeDegree){
  out <- list()
  # what degree is the duck at:
  duckAngleDeg <- NISTradianTOdeg(atan2(yDuck, xDuck))
  if(duckAngleDeg < 0){duckAngleDeg <- duckAngleDeg + 360}
  # what direction should fox go?
  degDiff <- foxDegStart - duckAngleDeg
  # fox very close
  if( (abs(degDiff) < stepSizeDegree) | (360-abs(degDiff) < stepSizeDegree)){
    newFox <- duckAngleDeg
  } else if(degDiff <= 180 & degDiff >= 0){ # fox in front in pos space
    newFox <- foxDegStart - stepSizeDegree
  } else if(degDiff <= -180){ # fox in front across 0/360
    newFox <- foxDegStart - stepSizeDegree
  } else if(degDiff <= 0 & degDiff >= -180){ # fox behind in pos space
    newFox <- foxDegStart + stepSizeDegree
  } else if(degDiff >= 180){ # fox behind across 0/360
    newFox <- foxDegStart + stepSizeDegree
  } else {println("SHIT"); newFox <- "SHIT"}
  # move newFox degrees into 0-360:
  if(newFox > 360){newFox <- newFox - 360}
  if(newFox < 0){newFox <- newFox + 360}
  newFox  
}


# duck evaluator function
# figures out how many steps (if ever) duck reaches shore
# outputs a set of x,y coordinates from the duck
# optionally plots
duckPath <- function(duckAngles, stepSize, R){
  xy <- matrix(0, ncol=2, nrow=length(duckAngles))
  for(i in 2:nrow(xy)){
    xy[i,1] <- cos(NISTdegTOradian(duckAngles[i]))*stepSize + xy[i-1,1]
    xy[i,2] <- sin(NISTdegTOradian(duckAngles[i]))*stepSize + xy[i-1,2]
  }
  xy  
}


# takes a duck path and the fox update function
# returns a boolean duckMadeShore
# returns a boolean duckEscaped
# returns a set of fox responses (thetas in degrees)

duckVersusFox <- function(aDuckPath, foxDegStart, stepSizeDegree, R, plotBoolean){
  if(plotBoolean){
    plot(0,0,xlim=c(-R*1.1, R*1.1), ylim=c(-R*1.1, R*1.1), type="n")
    draw.circle(0,0,10)
  }
  out <- list()
  out$duckMadeShore <- FALSE
  out$duckAngleDeg <- 0
  out$duckEscaped <- FALSE
  out$foxLocation <- c(0,0)
  out$finalIter <- 0
  foxTheta <- rep(0, length(aDuckPath))
  foxTheta[1] <- foxDegStart
  for(i in 2:nrow(aDuckPath)){
    # move fox:
    foxTheta[i] <- foxUpdate(foxTheta[i-1], aDuckPath[i,1], aDuckPath[i,2], stepSizeDegree)
    if(plotBoolean){
      text(aDuckPath[i,1], aDuckPath[i,2], i)
      x <- cos(NISTdegTOradian(foxTheta[i]))*R
      y <- sin(NISTdegTOradian(foxTheta[i]))*R
      text(x,y, col="red", i)
    }      
    # check to see if duck has reached shore
    if(aDuckPath[i,1]^2 + aDuckPath[i,2]^2 >= R^2){
      out$duckMadeShore <- TRUE
      out$foxLocation <- foxTheta[i]
      # check to see if the duck escaped, or was caught
      # if angle to duck's final (onshore) position is smaller than .5 degrees, fox caught it
      
      duckAngleDeg <- NISTradianTOdeg(atan2(aDuckPath[i,2], aDuckPath[i,1]))
      if(duckAngleDeg < 0){duckAngleDeg <- duckAngleDeg + 360}  
      out$duckAngleDeg <- duckAngleDeg
      if(abs(foxTheta[i]-duckAngleDeg) < 0.5 | abs(foxTheta[i]-duckAngleDeg)> 359.5 ){
        out$duckEscaped <- FALSE
      } else { out$duckEscaped <- TRUE}
      out$finalIter <- i
      break
    }
  }
  out$foxPath <- foxTheta
  
  out
}

# duckpath(duckAngles(n, degDelta), stepSize, R) 
dp <- duckPath(newDuck(20, 50), 3, 10)
dp
#duckVersusFox(dp, foxStartDeg, degStepSize, Plot)
duckVersusFox(dp, 90, 15, 10, TRUE)  





