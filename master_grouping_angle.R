## Author: Emma Gibbens
## Last updated: July 27, 2023
## Association: Horch Lab, Bowdoin College
## Purpose: This script allows you to run the data for all the crickets,
## sourcing the "grouping_turn_angle.R". See that file for more information.

library(stringr)


## input directory
input <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/angle_output/"

output_location <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/all_turn_angle.csv"

file.list <- list.files(path=input, pattern=".csv")

## looping through the files and running them through the grouping_turn_angle
for (file.name in file.list)
{
  split.name <- strsplit(file.name, "_")[[1]]
  cricket_name <- paste(split.name[1], split.name[2], sep = "_")
  source('/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/grouping_turn_angle.R')
  print(cricket_name)
}
