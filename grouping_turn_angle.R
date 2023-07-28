## Author: Emma Gibbens
## Last updated: July 27, 2023
## Association: Horch Lab, Bowdoin College
## Purpose: This script takes a csv file containing the turn angle for the cricket
## in each frame and the frames in which the light is on, and computes 
## the degree the cricket turned in response to the stimulus, and 
## whether or not the cricket turned correctly in response to the stimulus


## THIS ISN'T NECESSARY WHEN USING THE RELATED MASTER FILE, BUT CRICKETS CAN
## BE RUN INDIVIDUALLY USING JUST THIS FILE BY UNCOMMENTING THESE NEXT LINES
## input directories 
# input <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/angle_output/"
# 
# cricket_name <- "Cricket95_4Days"
# 
# output_location <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/all_turn_angle.csv"


## input file name
file.name <- paste(input, cricket_name, "_angles.csv", sep='')


## read in data
data <- read.csv(file.name, header = TRUE)


## finding the frames when the R and L lights are on
R.light.on <- data$frame_number[which(data$R_light_on == TRUE)]
L.light.on <- data$frame_number[which(data$L_light_on == TRUE)]

## finding the minimum and maximum frames for each section for lights
## takes as a parameter the frames for a light, returns a list of 2 sets of frames
separate.frames <- function(frames)
{
  find.diff <- diff(frames)
  second.set.start <- which(find.diff>50) + 1
  set1.frames <- frames[0:(second.set.start-1)]
  set2.frames <- frames[(second.set.start):length(frames)]
  return(list(set1.frames,set2.frames))
}

## creating the two sets of frames
R.light.first <- separate.frames(R.light.on)[[1]]
R.light.second <- separate.frames(R.light.on)[[2]]
L.light.first <- separate.frames(L.light.on)[[1]]
L.light.second <- separate.frames(L.light.on)[[2]]


## takes as an input the frames when the light is on
## returns the average difference between the turn angle when the 
## light is on and for the 20 previous frames 
av.diff <- function(frames)
{
  start.baseline <- frames[1] - 30
  baseline.av <- mean(data$angle[start.baseline:frames[1]])
  light.av <- mean(data$angle[frames[1]:frames[length(frames)]])
  diff.av <- light.av - baseline.av
  return(diff.av)
}


## does the same, just takes the max angle from the light on region
max.diff <- function(frames)
{
  start.baseline <- frames[1] - 30
  baseline.av <- mean(data$angle[start.baseline:frames[1]])
  light.angles <- data$angle[frames[1]:frames[length(frames)]]
  abs.max.index <- which(abs(data$angle) == max(abs(light.angles)))
  light.max <- data$angle[abs.max.index]
  diff.max <- light.max - baseline.av
  return(diff.max)
}

## does the same, just takes the maximum difference between the average
## baseline angle and the frame angle
max.angle.diff <- function(frames)
{
  start.baseline <- frames[1] - 30
  baseline.av <- mean(data$angle[start.baseline:frames[1]])
  light.angles <- data$angle[frames[1]:frames[length(frames)]]
  ## finds the difference between the angles when the light is on and the average
  light.diff <- light.angles - baseline.av
  abs.light.diff <- abs(light.diff)
  max.diff.index <- which(abs(light.diff) == max(abs.light.diff))
  max.diff <- light.diff[max.diff.index]
  if(length(max.diff) != 1)
  {
    max.diff <- max.diff[1]
  }
  return(max.diff)
}

## if the difference is positive, turn to the right. If negative, turn
## to the left

## finding turn angle using the maximum difference between the angle when
## the light is on and the baseline angle
turn.R.first <- max.angle.diff(R.light.first)
turn.R.second <- max.angle.diff(R.light.second)
turn.L.first <- max.angle.diff(L.light.first)
turn.L.second <- max.angle.diff(L.light.second)


## finding the turn angle using the difference between the average
## when the light is on and baseline average
turn.R.first.av <- av.diff(R.light.first)
turn.R.second.av <- av.diff(R.light.second)
turn.L.first.av <- av.diff(L.light.first)
turn.L.second.av <- av.diff(L.light.second)

## takes as input the turn angle and the side from which the light is on
## returns TRUE or FALSE for whether or not the cricket turned the correct
## direction
turn.accuracy <- function(turn.angle, light)
{
  if((turn.angle < 0) & (light=="right"))
  {
    return(TRUE)
  } else if((turn.angle > 0) & (light=="left")){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

## turn accuracy (true is turned correct direction, false is didn't)
correct.R.first <- turn.accuracy(turn.R.first, "right")
correct.R.second <- turn.accuracy(turn.R.second, "right")
correct.L.first <- turn.accuracy(turn.L.first, "left")
correct.L.second <- turn.accuracy(turn.L.second, "left")


## creating the variables for a data frame
cricket.name <- cricket_name
turn.angle.R <- c(turn.R.first, turn.R.second)
turn.angle.L <- c(turn.L.first, turn.L.second)
correct.R <- c(correct.R.first, correct.R.second)
correct.L <- c(correct.L.first, correct.L.second)
turn.R.av <- c(turn.R.first.av, turn.R.second.av)
turn.L.av <- c(turn.L.first.av, turn.L.second.av)


## creating the data frame
df <- data.frame(cricket.name,
                 turn.angle.R,
                 turn.angle.L,
                 correct.R,
                 correct.L,
                 turn.R.av,
                 turn.L.av)


## check if file already exists, and if it doesn't, create the file.
## if the file does exist, append the new data to the file
if (!file.exists(output_location))
{
  ## if the file doesnt exist, creates a new csv file and adds the 
  ## data to the first row
  write.csv(df, file=output_location, row.names=FALSE, quote=FALSE)
} else
{
  ## appends the data to the csv file already created 
  write.table(df, file=output_location, append=TRUE, 
              row.names=FALSE, quote=FALSE, sep=',', 
              col.names=!file.exists(output_location))
}

