# horch-scripts
Scripts created for the Horch Lab at Bowdoin College

### Python Script
The python script "area_av_RGB.py" is used to identify the color of the light for an area around a pixel identified using DeepLabCut (DLC). The inputs are the location of the light in each frame (csv file produced by DLC), and the video itself. The output is a csv file that includes the average RBG value for the area around the center of the red and green lights in each frame.

### R Scripts
* imageJ_method.R : should be used to process the videos analyzed using the ImageJ method. It produces a csv file with a list of frames in which it determines the light is on, using the data taken from ImageJ.   
* python.meth.R : should be used to process the videos analyzed using the Python method. It produces a csv file with a list of frames in which it determines the light is on, using the data taken from Python.
* final_frame_decision.R : takes either the list of frames produced by the ImageJ or Python methods, and creates a final list of frames it determines the light is on for a video and stores that in a new csv.
* master_light_analysis.R : sources the final_frame_decision.R and allows you to loop through all the videos you wish to through a single run of the code
* comparing_results.R : should be used to compare the frames derived from the ImageJ method and Python method to assess the quality of the of the frame extraction.
* graphing_quality_results.R : creates graphs using the data from the comparing_results.R
* master_angle_analysis.R : sources the the turn_angle_analysis.R script. takes everything in the directories that contain the DLC output files and the frame files (when the light is on) and run the turn angle analysis for each video
* graphing_turn_angle.R : produces graphs with the turn angle of the cricket for every frame, from csv files that contain the turn angle of the cricket for every frame, and stores in a PDF
* grouping_turn_angle.R : This script takes a csv file containing the turn angle for the cricket in each frame and the frames in which the light is on, and computes the degree the cricket turned in response to the stimulus, and whether or not the cricket turned correctly in response to the stimulus
* master_grouping_angle.R : sourcing the grouping_turn_angle.R for the data from each of the crickets
* analyzing_all_turn_angles.R : should be used to process the data file containing the turn angle and correct turn (t/f) in response to the stimuli for all of the crickets
