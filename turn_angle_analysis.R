## Author: Emma Gibbens
## Last updated: July 27, 2023
## Association: Horch Lab, Bowdoin College
## Purpose: finds the turn angle of the cricket for every frame in each video.
## Stores the turn angle for each frame and whether or not the light is on 
## in that frame in a single csv file

library(MASS)


## input directories 
input_DLC <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/DLC_csv_files_2016/"
input_frames <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/final_output/"

# cricket_name <- "Cricket87_3Days"

## input file names
file_name_DLC <- paste(input_DLC, cricket_name, "_editDLC_resnet50_dlc_2016_videosJun16shuffle1_250000_filtered.csv", sep='')
file_name_framesR <- paste(input_frames, cricket_name, "_R.csv", sep='')
file_name_framesL <- paste(input_frames, cricket_name, "_L.csv", sep='')

## output file names
file_name_output <- paste(cricket_name, "angles.csv", sep='_')

## final storage location for files
output_directory <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/angle_output/"

## output path
output_path <- paste(output_directory, file_name_output, sep='')

## FUNCTIONS
dist.func <- function(x1,y1,x2,y2) # calculates Euclidean distance between two vectors
{
  sqrt((x2-x1)^2 + (y2-y1)^2) 
}


center_point <- function(x, y) {
  # Estimate the density of the points using kernel density estimation
  # NOTE: when n is too small kde gets weird, N = 200 glitched.
  dens <- kde2d(x, y, n = 300)
  
  # Find the index of the point with the highest density
  max_idx <- which(dens$z == max(dens$z), arr.ind = TRUE)
  
  # Extract the x and y coordinates of the center point
  center_x <- dens$x[max_idx[1]]
  center_y <- dens$y[max_idx[2]]
  
  # Return the center point as a vector
  return(c(center_x, center_y))
}


## EXTRACT DATA
# generates dataframe from .csv file and skip first 3 lines, ignore header names
data.DLC <- read.csv(file_name_DLC, skip = 3, header = FALSE)
data.frames.R <- read.csv(file_name_framesR, header = TRUE)
data.frames.L <- read.csv(file_name_framesL, header = TRUE)

## extracts from the frame csvs
R_light_on <- data.frames.R$total.red
L_light_on <- data.frames.L$total.green


## Defining ROIs
frame_col <- data.DLC[, 1]
ab_x <- data.DLC[, 2]
thorax_x <- ab_x
ab_y <- data.DLC[, 3]
thorax_y <- ab_y

wax_x <- data.DLC[, 5]
wax_y <- data.DLC[, 6]

left_knee_x <- data.DLC[, 8]
left_knee_y <- data.DLC[, 9]

left_foot_x <- data.DLC[, 11]
left_foot_y <- data.DLC[, 12]

right_knee_x <- data.DLC[, 14]
right_knee_y <- data.DLC[, 15]

right_foot_x <- data.DLC[, 17]
right_foot_y <- data.DLC[, 18]





# Some positions of interest
thorax_x_mean <- mean(ab_x)
thorax_y_mean <- mean(ab_y)
frame_len <- length(ab_x) # this is how many frames are in the dataset

#' This function takes in 2 vectors of x and y coordinates in a timed series
#' and return the center mode of the points.
#'
#' @param x A time series of x coordinates
#' @param y A time series of y coordinates
#'
#' @return A vector of x and y coordinates of the center mode


anchor <- center_point(wax_x, wax_y)
anchor_x <- anchor[1]
anchor_y <- anchor[2]

# Find the angle between the abdomen and the center line:
# Note: This part would've been much easier computationally, but since this is
# not really a time consuming part I'm just going to use it as it is.

a <- dist.func(ab_x, ab_y, anchor_x, ab_y)
b <- dist.func(anchor_x, anchor_y, anchor_x, ab_y)
c <- dist.func(ab_x, ab_y, anchor_x, anchor_y)

angles <- acos((b^2 + c^2 - a^2) / (2 * b * c)) * 180 / pi # angle calculation

# Define center line (wax) as zero; make angles positive or negative deviation
# from that line:
for (i in 1:frame_len) {
  if(angles[i] > 40){
    angles[i] = NA
  }
  if (ab_x[i] <= anchor_x) {
    angles[i] <- -angles[i]
  }
}



final_R_list <- rep(0, times=frame_len)
final_L_list <- rep(0, times=frame_len)

## creating a list with booleans for the right light being on and off
for (i in 1:frame_len)
{
  if (i %in% R_light_on)
  {
    final_R_list[i] <- TRUE
  } else{
    final_R_list[i] <- FALSE
  }
}

for (i in 1:frame_len)
{
  if (i %in% L_light_on)
  {
    final_L_list[i] <- TRUE
  } else{
    final_L_list[i] <- FALSE
  }
}


## creating a csv output files that have the turn angle and when the 
## light is on
df <- data.frame(frame_number = frame_col,
                 angle = angles,
                 R_light_on = final_R_list, 
                 L_light_on = final_L_list)

write.csv(df, 
          file=output_path, 
          row.names=FALSE, 
          quote=FALSE)



