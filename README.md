# horch-scripts
Scripts created for the Horch Lab at Bowdoin College

### Python Script
The python script "area_av_RGB.py" is used to identify the color of the light for an area around a pixel identified using DeepLabCut (DLC). The inputs are the location of the light in each frame (csv file produced by DLC), and the video itself. The output is a csv file that includes the average RBG value for the area around the center of the red and green lights in each frame.

### R Scripts
* The R script imageJ_method.R should be used to process the videos analyzed using the ImageJ method. It produces a csv file with a list of frames in which it determines the light is on, using the data taken from ImageJ.   
* The R Script python.meth.R should be used to process the videos analyzed using the Python method. It produces a csv file with a list of frames in which it determines the light is on, using the data taken from Python.
* The R script final_frame_decision.R takes either the list of frames produced by the ImageJ or Python methods, and creates a final list of frames it determines the light is on for a video and stores that in a new csv.
* The R script master_light_analysis.R sources the final_frame_decision.R and allows you to loop through all the videos you wish to through a single run of the code
* The R script comparing_results.R should be used to compare the frames derived from the ImageJ method and Python method to assess the quality of the of the frame extraction.
* The R script graphing_quality_results.R creates graphs using the data from the comparing_results.R
* The R Script analyzing_all_turn_angles.R should be used to process the data file containing the turn angle and correct turn (t/f) in response to the stimuli for all of the crickets
