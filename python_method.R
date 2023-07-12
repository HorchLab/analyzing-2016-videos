## Author: Emma Gibbens
## Last update: July 12th, 2023
## Association: Horch Lab, Bowdoin College
## This is an R script designed to take the output of the trying_area.py and produce two csv files (one for each light)
## The csv files contain the frames in the videos during which the light is on (one column)


## need to change the input and output file names, and determine the best metric
## (R, B, or G) for each light (Red/Green), and input that as the final frames

## need to change this 
video_path <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/output_python/Cricket80_14Days.csv"

## read in data set
data.set <- read.table(video_path, header=TRUE, sep=",")



## creating a function that finds the frames
## use the graphs to identify which RGB value should be used to identify frames 
finding.frames <- function(file, light, multiplier)
{
  abs.diff <- abs(diff(light))
  av.diff <- mean(abs.diff)
  sd.diff <- sd(abs.diff)
  high.diff <- av.diff + (multiplier * sd.diff)
  plot(diff(light), pch=20)
  abline(h=high.diff, col='blue', lwd=3)
  index.higher <- which(diff(light) > high.diff)
  frame.light <- file[index.higher]
  return(frame.light)
}


## function that removes the outlier frames from the list of frames
extract.half <- function(frame.list)
{
  quartiles <- quantile(frame.list, probs=c(.25, .75), na.rm = FALSE)
  IQR <- IQR(frame.list)
  Lower <- quartiles[1] - 1.5*IQR
  Upper <- quartiles[2] + 1.5*IQR
  data.no.outlier <- subset(frame.list, frame.list > Lower & frame.list < Upper)
  return(data.no.outlier)
}

## function that splits the list of frames in 2 and finds the outliers
## and then returns a list of frames, with no outliers
extract.outlier <- function(frame.list)
{
  middle.ds <- round((length(data.set$Frame.Number))/2)
  lower.frames <- frame.list[which(frame.list <= middle.ds)]
  upper.frames <- frame.list[which(frame.list > middle.ds)]
  lower.extracted <- extract.half(lower.frames)
  upper.extracted <- extract.half(upper.frames)
  data.extracted <- append(lower.extracted, upper.extracted)
}

## finding the frames where the light is on 
R.Light.R.Frames <- finding.frames(data.set$Frame.Number, data.set$R.Light.R, 1)
R.Light.B.Frames <- finding.frames(data.set$Frame.Number, data.set$R.Light.B, 1.25)
R.Light.G.Frames <- finding.frames(data.set$Frame.Number, data.set$R.Light.G, 1)

L.Light.R.Frames <- finding.frames(data.set$Frame.Number, data.set$L.Light.R, 1.45)
L.Light.B.Frames <- finding.frames(data.set$Frame.Number, data.set$L.Light.B, 1.25)
L.Light.G.Frames <- finding.frames(data.set$Frame.Number, data.set$L.Light.G, .9)


## extracted frames - choose the best graphs
R.Light.Extracted <- extract.outlier(R.Light.B.Frames)
L.Light.Extracted <- extract.outlier(L.Light.G.Frames)

## change this for each time
final.R.frames <- R.Light.Extracted
final.L.frames <- L.Light.Extracted


## see the general information
# len.R <- length(final.R.frames)
# len.L <- length(final.L.frames)
# print(len.R)
# print(final.R.frames)
# print(len.L)
# print(final.L.frames)


## making the output file - this will need to be changed 
## 
## Specify the output file path
data.R <- data.frame(final.R.frames)
data.L <- data.frame(final.L.frames)

output_file_R <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/output/Cricket80_14Days_python_R.csv"
output_file_L <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/output/Cricket80_14Days_python_L.csv"

write.csv(data.R, file = output_file_R, row.names = FALSE)
write.csv(data.L, file = output_file_L, row.names = FALSE)


