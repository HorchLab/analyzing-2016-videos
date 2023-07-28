## Author: Emma Gibbens
## Last updated: July 13th, 2023
## Association: Horch Lab, Bowdoin College
## This script takes the given data (cricket video and method of analysis), and 
## finds when each of the lights turn on and off. It then takes every three
## frames between the light on and light off frame, and stores it into a csv 
## file for each light


## THE BELOW PARAMTERS ARE THE ONES THAT CHANGE
## which cricket video you are analyzing
cricket_name <- 'Cricket161_Baseline'

## which method did you use to analyze the video (imageJ or python)?
processing_type <- "imageJ"

## input directory 
input_directory <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/output/"

## final storage location for files
output_directory <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/final_output/"

## DON'T CHANGE ANY PARAMETERS AFTER THIS

## input file name
file_name_R <- paste(cricket_name,"_",processing_type,"_R.csv", sep='')
file_name_L <- paste(cricket_name,"_",processing_type,"_L.csv", sep='')

## input file path
file_path_R <- paste(input_directory, file_name_R, sep='')
file_path_L <- paste(input_directory, file_name_L, sep='')

## output file name
output_name_R <- paste(output_directory, cricket_name, "_R.csv", sep='')
output_name_L <- paste(output_directory, cricket_name, "_L.csv", sep='')

## obtaining the data from the input files 
data.R <- read.table(file_path_R, header=TRUE, sep=",")
data.L <- read.table(file_path_L, header=TRUE, sep=",")

## frame values 
red.on <- data.R$red.light.on
green.on <- data.L$green.light.on

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


## creating a new list of frames starting at the first frame and increasing by 
## 3 until the final frame of that list 
frames.by.3 <- function(frame.list)
{
  first.frame <- frame.list[1]
  last.frame <- frame.list[length(frame.list)]
  new.list <- seq(first.frame, last.frame, by=3)
  return(new.list)
}


## calling the function to retrieve the frames from set 1 and set 2 for the 
## green and red light 
red.set1 <- separate.frames(red.on)[[1]]
red.set2 <- separate.frames(red.on)[[2]]
green.set1 <- separate.frames(green.on)[[1]]
green.set2 <- separate.frames(green.on)[[2]]


## calling the function to create a new set of lists with the frames, increasing
## by intervals of 3
final.red.1 <- frames.by.3(red.set1)
final.red.2 <- frames.by.3(red.set2)
final.green.1 <- frames.by.3(green.set1)
final.green.2 <- frames.by.3(green.set2)

## creating a final list of all the frames where the light is on, for each light
total.red <- c(final.red.1, final.red.2)
total.green <- c(final.green.1, final.green.2)


## creating a csv output files that contain when all the green and red lights
## are on
data.right <- data.frame(total.red)
data.left <- data.frame(total.green)

write.csv(data.right, file=output_name_R, 
          row.names=FALSE, 
          quote=FALSE)
write.csv(data.left, file=output_name_L, 
          row.names=FALSE, 
          quote=FALSE)


