## Author: Emma Gibbens
## Last update: July 12th, 2023
## Association: Horch Lab
## This is an R script that takes two csv files which contain the average RGB value around two lights
## in each frame of a video, and outputs two csv files which indicate in which frames the light is on.

## need to change the file names at the beginning and the output file names

## change the file names to the new files for the video
file.1 <- read.table("Cricket80_14Days_1-1452.csv", header=TRUE, sep=",")
file.2 <- read.table("Cricket80_14Days_1453-2835.csv", header=TRUE, sep=",")


## creating a function that finds the frames
finding.frames <- function(file, light)
{
  abs.diff <- abs(diff(light))
  av.diff <- mean(abs.diff)
  sd.diff <- sd(abs.diff)
  low.diff <- av.diff - (1.5 * sd.diff)
  high.diff <- av.diff + (1.5 * sd.diff)
  plot(diff(light))
  abline(h=low.diff, col='red')
  abline(h=high.diff, col='blue')
  index.higher <- which(diff(light) > high.diff)
  frame.light <- file[index.higher]
  return(frame.light)
}


## function that removes the outlier frames from the list of frames
extract.outlier <- function(frame.list)
{
  quartiles <- quantile(frame.list, probs=c(.25, .75), na.rm = FALSE)
  IQR <- IQR(frame.list)
  Lower <- quartiles[1] - 1.5*IQR
  Upper <- quartiles[2] + 1.5*IQR
  data.no.outlier <- subset(frame.list, frame.list > Lower & frame.list < Upper)
  return(data.no.outlier)
}


## getting the frames from the videos
file1.Red <- finding.frames(file.1$X, file.1$Mean.Red.light.)
file1.Green <- finding.frames(file.1$X, file.1$Mean.Green.light.)
file2.Red <- finding.frames(file.2$X, file.2$Mean.Red.light.)
file2.Green <- finding.frames(file.2$X, file.2$Mean.Green.light.)


## removing the outliers form the list of frames (and adding the first file)
## I subtracted 2 because it seemed to fit the python better
file1.Red.clean <- extract.outlier(file1.Red)
file1.Green.clean <- extract.outlier(file1.Green)
file2.Red.clean <- length(file.1$X) - 2 + extract.outlier(file2.Red)
file2.Green.clean <- length(file.1$X) - 2 + extract.outlier(file2.Green)


## finishing the job - frames - these are the final results
red.light.on <- append(file1.Red.clean,file2.Red.clean)
green.light.on <- append(file1.Green.clean, file2.Green.clean)

## seeing the general information
# print(red.light.on)
# print(length(red.light.on))
# print(green.light.on)
# print(length(green.light.on))


## making the output file - this will need to be changed 
## 
## Specify the output file path
data.R <- data.frame(red.light.on)
data.L <- data.frame(green.light.on)

output_file_R <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/output/Cricket80_14Days_imageJ_R.csv"
output_file_L <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/output/Cricket80_14Days_imageJ_L.csv"

write.csv(data.R, file = output_file_R, row.names = FALSE)
write.csv(data.L, file = output_file_L, row.names = FALSE)




