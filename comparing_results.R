## Author: Emma Gibbens
## Last updated: July 11, 2023
## Association: Horch Lab
## comparing the results of the python and ImageJ methods of finding when the 
## light is on. Returns 4 percentages, two for each light, which demonstrate
## what percent of ImageJ frames were also identified through the python method
## and vice versa.


## THIS CODE MAY BE CHANGED ONLY IF YOU ARE MANUALLY INPUTTING EACH VIDEO

# ## which cricket video you are analyzing
# cricket_name <- 'Cricket101_3Days'

## input directory 
input_directory <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/output/"

## list of files
file_list <- list.files(path = input_directory, pattern='.csv') # make sure to identify which directory

## final storage location for file
output_location <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/final_output/quality_analysis.csv"

## NO CODE AFTER THIS SHOULD BE CHANGED


## function that finds the files that contain the cricket's name, returns a list
## with indeces that go from 1-4
find.files <- function(input_directory, cricket_name) 
{
  file.list <- list.files(input_directory, full.names = TRUE)
  matching.files <- grep(cricket_name, file.list, value = TRUE)
  return(matching.files)
}

## finding the percent of frames in the imageJ file are also in the python file
## takes as its input the relevant imageJ frames and python frames, returns a 
## float of the number of imageJ frames found in the python frames, divided
## by the total number of imageJ frames
imageJ.percentage <- function(imageJ.frames, python.frames)
{
  len.imageJ <- length(imageJ.frames)
  in.python <- list()
  notin.python <- list()
  for(i in 1:len.imageJ)
  {
    if(imageJ.frames[i] %in% python.frames)
    {
      in.python <- append(in.python, imageJ.frames[i])
    }
    else
    {
      notin.python <- append(notin.python, imageJ.frames[i])
    }
  }
  len.in.python <- length(in.python)
  len.notin.python <- length(notin.python)
  frac <- len.in.python/len.imageJ
  return(frac)
}


## finding the percent of frames in the python file are also in the imageJ file
## takes as its input the relevant imageJ frames and python frames, returns a 
## float of the number of python frames found in the imageJ frames, divided
## by the total number of python frames
python.percentage <- function(imageJ.frames, python.frames)
{
  len.python <- length(python.frames)
  in.imageJ <- list()
  notin.imageJ <- list()
  for(i in 1:len.python)
  {
    if(python.frames[i] %in% imageJ.frames)
    {
      in.imageJ <- append(in.imageJ, python.frames[i])
    }
    else
    {
      notin.imageJ <- append(notin.imageJ, python.frames[i])
    }
  }
  len.in.imageJ <- length(in.imageJ)
  len.notin.imageJ <- length(notin.imageJ)
  frac <- len.in.imageJ/len.python
  return(frac)
}


## extracting the files from the directory to compare against each other
imageJ.L.file <- grep("imageJ_L.csv", find.files(input_directory, cricket_name), value=TRUE)
python.L.file <- grep("python_L.csv", find.files(input_directory, cricket_name), value=TRUE)
imageJ.R.file <- grep("imageJ_R.csv", find.files(input_directory, cricket_name), value=TRUE)
python.R.file <- grep("python_R.csv", find.files(input_directory, cricket_name), value=TRUE)

## reading the aforementioned csv files
imageJ.L.data <- read.table(imageJ.L.file, header=TRUE, sep=",")
python.L.data <- read.table(python.L.file, header=TRUE, sep=",")
imageJ.R.data <- read.table(imageJ.R.file, header=TRUE, sep=",")
python.R.data <- read.table(python.R.file, header=TRUE, sep=",")

imageJ.L.frames <- imageJ.L.data$green.light.on
python.L.frames <- python.L.data$final.L.frames
imageJ.R.frames <- imageJ.R.data$red.light.on
python.R.frames <- python.R.data$final.R.frames


## calling the imageJ.percentage function - finding the percentage of frames in
## the imageJ file that are also in the python file
imageJ.L.in.python <- (imageJ.percentage(imageJ.L.frames, python.L.frames) * 100)
imageJ.R.in.python <- (imageJ.percentage(imageJ.R.frames, python.R.frames) * 100)


## calling the python.percentage function - finding the percentage of frames
## in the python file that are also in the imageJ file 
python.L.in.imageJ <- (python.percentage(imageJ.L.frames, python.L.frames) * 100)
python.R.in.imageJ <- (python.percentage(imageJ.R.frames, python.R.frames) * 100)


##
## add the above values (python and imageJ relative to each other)
## to a csv file that contains all the values for all of the videos
##


## create df with the above values
df <- data.frame(
  video.name = cricket_name,
  ImageJ.L.in.python = imageJ.L.in.python,
  ImageJ.R.in.python = imageJ.R.in.python,
  Python.L.in.ImageJ = python.L.in.imageJ,
  Python.R.in.ImageJ = python.R.in.imageJ
)

## check if file already exists, and if it doesn't, create the file.
## if the file does exist, append the new data to the file
if (!file.exists(output_location))
{
  ## if the file doesnt exist, creates a new csv file and adds the 
  ## data to the first row
  write.csv(df, file=output_location, row.names=FALSE, quote=FALSE)
} else
{
  ## appends the data to the csv file already created 
  write.table(df, file=output_location, append=TRUE, 
              row.names=FALSE, quote=FALSE, sep=',', 
              col.names=!file.exists(output_location))
}

