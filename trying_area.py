import numpy as np
import cv2 as cv
import csv


## Create a VideoCapture object and read from input file - need to change these
movie = '/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/Cricket80_14Days.mov'
csv_file = '/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/Cricket80_14DaysDLC_resnet50_dlc_light_xyJun20shuffle1_250000.csv'
output_file = '/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/output_python/Cricket80_14Days.csv'
cap = cv.VideoCapture(movie)

## don't change code after this

## this is the beginning of the actual code
def rgb_to_ycbcr(rgb_value):
    ## converter for rbg to ycbcr, don't actually implement in this iteration of the code
    # Create a blank image with 1x1 resolution
    img = np.zeros((1, 1, 3), dtype=np.uint8)
    # Assign the RGB value to the pixel in the image
    img[0, 0] = rgb_value

    # Convert the image from RGB to YCbCr color space
    img_ycbcr = cv.cvtColor(img, cv.COLOR_RGB2YCrCb)

    # Extract the YCbCr value from the image
    ycbcr_value = img_ycbcr[0, 0]

    return ycbcr_value


def get_pixel_color(frame, x, y):
    ## funciton that takes a frame # and pixel xy coordinate and returns the RGB value at that 
    ## coordinate
    try:
        # Get the color values (BGR) of the pixel at coordinates (x, y), from a frame in a video
        pixel_color = frame[y, x]
        # Convert BGR to RGB
        rgb_color = (int(pixel_color[2]), int(pixel_color[1]), int(pixel_color[0]))
        return rgb_color
    except TypeError:
        print("returned TypeError - don't know why")


## takes the frame and pixel and radius, returns the average pixel color around the area
def extract_pixel_color(frame, x, y, radius):
    try:
        # Extract a region of interest (ROI) around the pixel
        roi = frame[max(0, y-radius):min(y+radius, frame.shape[0]), 
                    max(0, x-radius):min(x+radius, frame.shape[1])]
        
        # Calculate the average color within the ROI
        average_color = cv.mean(roi)[:3]  # Exclude alpha channel if present
        return average_color
    except AttributeError:
        print("hit type error")


## pixel value - stored as a dictionary, with keys as frameNr, and pixel(x,y) for R light
## and pixel(x,y) for L light
pixel_xy = {}
with open(csv_file, "r") as contents:
    lines = contents.readlines()
    for line in lines:
        if line[0].isnumeric():
            row = line.split(",")
            pixel_xy[int(row[0])] = [float(row[1]), float(row[2]), float(row[4]), float(row[5])]
        else:
            pass

## pixel dictionary, with keys as the frame number and values as the RBG value 
## new dictionary for each video
pixel_dict = {}

## getting a frame from the video
success,frame = cap.read()


## setting the first frame
frameNr = 0
## looping through the frames
while success:
    # process frames
    success,frame = cap.read()
    ## using the function to get the color from the pixels in the frame
    pixel_color_right = extract_pixel_color(frame, round(pixel_xy[frameNr][0]), round(pixel_xy[frameNr][1]), 6)
    pixel_color_left = extract_pixel_color(frame, round(pixel_xy[frameNr][2]), round(pixel_xy[frameNr][3]), 6)
    ## placing that pixel color in the dictionary, with the frame as the key
    pixel_dict[frameNr] = [pixel_color_right, pixel_color_left]
    frameNr += 1

# Release the video file
cap.release()

## creating a csv output file that will have rows with the frame # and rbg values
with open(output_file, 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(["Frame Number", "R Light R", "R Light G", "R Light B", "L Light R", "L Light G", "L Light B"])
    try:
        for key, value in pixel_dict.items():
            writer.writerow([key, value[0][0], value[0][1], value[0][2], value[1][0], value[1][1], value[1][2]])
    except TypeError:
        pass
