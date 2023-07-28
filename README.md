# horch-scripts
Scripts created for the Horch Lab at Bowdoin College

### Python Script
The python script "area_av_RGB.py" is used to identify the color of the light for an area around a pixel identified using DeepLabCut (DLC). The inputs are the location of the light in each frame (csv file produced by DLC), and the video itself. The output is a csv file that includes the average RBG value for the area around the center of the red and green lights in each frame.

### R Scripts
* The R script imageJ_method.R should be used to process the videos analyzed using the ImageJ method.  
* The R Script python.meth.R should be used to process the videos analyzed using the Python method.  
* The R Script analyzing_all_turn_angles.R should be used to process the data file containing the turn angle and correct turn (t/f) in response to the stimuli for all of the crickets
