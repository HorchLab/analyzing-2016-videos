## Author: Emma Gibbens
## Last updated: July 27, 2023
## Association: Horch Lab, Bowdoin College
## Purpose: this script loops through an excel file containing the videos 
## I want analyzed, and finds in the angle_output folder the file related to
## that video, and plots all the graphs for the video in a single PDF.
## It basically produces graphs with the turn angle of the cricket for every
## frame from csv files that contain the turn angle of the cricket for every 
## frame

library(ggplot2)
library(readxl)

input_directory <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/angle_output/"
## read in the files that you want analyzed from the input directory
complete.analysis.vids <- read_excel("/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/output/videos to complete analysis.xlsx")
vids.frame.analysis <- complete.analysis.vids$video_name

pdf("attempt1.pdf", height=8.5, width=11)

## adds all the videos to a single PDF 
for(vid in vids.frame.analysis)
{
  input_file <- paste(input_directory, vid, "_angles.csv", sep='')
  df <- read.csv(input_file, header = TRUE)
  
  len <- length(df$frame_number)
  
  combined <- rep(0, times=len)
  
  for (i in 1:len)
  {
    if(df$R_light_on[i]==1)
    {
      combined[i] <- "Right_on"
    } else if(df$L_light_on[i]==1) {
      combined[i] <- "Left_on"
    } else {
      combined[i] <- "Off"
    }
  }
  
  df.2 <- data.frame(frame_num = df$frame_number,
                     angle = df$angle,
                     light_on = combined)
  
  p <- ggplot(data=df) + 
    geom_line(mapping=aes(x=df.2$frame_num, y=df.2$angle)) +
    geom_point(mapping=aes(x=df.2$frame_num, y=df$R_light_on), col="red") +
    geom_point(mapping=aes(x=df.2$frame_num, y=df$L_light_on), col="yellow") +
    geom_hline(yintercept=0, col="black", size=2) +
    labs(title=vid,
         x="Frame Number", 
         y="Angle (+ is R, - is L)")
  
  print(p)
}


dev.off()


## BASICALLY IS JUST WHAT'S IN THE FOR LOOP
# input_file <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/angle_output/Cricket95_4Days_angles.csv"
# 
# df <- read.csv(input_file, header = TRUE)
# 
# len <- length(df$frame_number)
# 
# combined <- rep(0, times=len)
# 
# for (i in 1:len)
# {
#   if(df$R_light_on[i]==1)
#   {
#     combined[i] <- "Right_on"
#   } else if(df$L_light_on[i]==1) {
#     combined[i] <- "Left_on"
#   } else {
#     combined[i] <- "Off"
#   }
# }
# 
# df.2 <- data.frame(frame_num = df$frame_number,
#                    angle = df$angle,
#                    light_on = combined)
# 
# p <- ggplot(data=df) + 
#   geom_point(mapping=aes(x=df.2$frame_num, y=df.2$angle, color=df.2$light_on),
#              position = "jitter", size=1.2) +
#   labs(x="Frame Number", 
#        y="Angle (+ is R, - is L)",
#        color="Light On") + 
#   scale_color_manual(values=c("yellow", "blue", "red"))
# 
# print(p)


