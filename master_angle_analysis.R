## Author: Emma Gibbens
## Last updated: July 20, 2023
## Association: Horch Lab, Bowdoin College
## This is a document that sources the the turn_angle_analysis.R script
## It allows you to take everything in the directories that contain 
## the DLC output files and the frame files (when the light is on) and run 
## the turn angle analysis for each video

library(readxl)

## input directory 
input_DLC <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/DLC_csv_files_2016/"
input_frames <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/final_output/"

## final storage location for files
output_directory <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/angle_output/"

## read in the files that you want analyzed from the input directory
complete.analysis.vids <- read_excel("/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/output/videos to complete analysis.xlsx")


vids.frame.analysis <- complete.analysis.vids$video_name


## making the final frame decision files 
for(vid in vids.frame.analysis)
{
  ## which cricket video you are analyzing
  cricket_name <- vid
  source('/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/turn_angle_analysis.R') ## this will read the script to get the final frames
  print(cricket_name)
}

