## Author: Emma Gibbens
## Last updated: July 13, 2023
## Association: Horch Lab, Bowdoin College
## this takes the lists of videos i want analyzed in an excel file, 
## and uses the "final_frame_decision.R" to produce the final list of 
## frames for that video, and uses "comparing_results.R" to produce and store 
## the measurements of quality for that video

library(readxl)

## input directory 
input_directory <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/output/"
## final storage location for files
output_directory <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/final_output/"

## read in the files that you want analyzed from the input directory
complete.analysis.vids <- read_excel("/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/output/videos to complete analysis.xlsx")
quality.analysis.vids <- read_excel("/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/output/videos for quality analysis.xlsx")

vids.frame.analysis <- complete.analysis.vids$video_name
vids.quality.analysis <- quality.analysis.vids$video_name

## making the final frame decision files 
for(vid in vids.frame.analysis)
{
  ## which cricket video you are analyzing
  cricket_name <- vid
  source('/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/final_frame_decision.R') ## this will read the script to get the final frames
  print(cricket_name)
}


## uncomment to use again
## performing the quality analysis

# for(vid in vids.quality.analysis)
# {
#   ## which cricket video you are analyzing
#   cricket_name <- vid
#   source('/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/comparing_results.R')
# }


