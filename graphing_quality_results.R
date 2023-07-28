## Author: Emma Gibbens
## Last updated: July 13th, 2023
## Association: Horch Lab at Bowdoin College
## Purpose: This R script takes the data returned from the comparing_results 
## script and graphs it to demonstrate level of anticipated accuracy. 
## Author: Emma Gibbens
## Last updated: July 13th, 2023
## Association: Horch Lab, Bowdoin College
## Takes the data produced by the comparing_results.R
## It creates histograms of the percentages of frames that are consistent
## between the two methods (ImageJ and python) for each video and light


library(ggplot2)
library("gridExtra")
library("cowplot")

## created by Emma Gibbens on July 12, 2023 for Horch Lab 

## this file takes the data returned from the comparing_results script and 
## graphs it to demonstrate level of anticipated accuracy

input_file_path <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/final_output/quality_analysis.csv"

df <- read.table(input_file_path, header=TRUE, sep=",")

## plotting the histogram of values for the percentage of Image J frames (left light) 
## that are also in the python file
p <- ggplot(data=df)
p <- p + geom_histogram(mapping=aes(x=df$ImageJ.L.in.python), fill="blue",
                        alpha=0.6)
p <- p + labs(x="% of ImageJ frames also in Python",
              title="ImageJ in Python, Left Light")
p <- p + coord_cartesian(xlim = c(20, 100))


## plotting the histogram of values for the percentage of Image J frames (right light) 
## that are also in the python file
q <- ggplot(data=df)
q <- q + geom_histogram(mapping=aes(x=df$ImageJ.R.in.python), fill="red",
                        alpha=0.6)
q <- q + labs(x="% of ImageJ frames also in Python",
              title="ImageJ in Python, Right Light")
q <- q + coord_cartesian(xlim = c(20, 100))


## plotting the histogram of values for the percentage of python frames (left light) 
## that are also in the ImageJ file
r <- ggplot(data=df)
r <- r + geom_histogram(mapping=aes(x=df$Python.L.in.ImageJ), fill="green",
                        alpha=0.6)
r <- r + labs(x="% of Python frames also in ImageJ",
              title="Python in ImageJ, Left Light")
r <- r + coord_cartesian(xlim = c(20, 100))

## plotting the histogram of values for the percentage of python frames (right light) 
## that are also in the ImageJ file
s <- ggplot(data=df)
s <- s + geom_histogram(mapping=aes(x=df$Python.R.in.ImageJ), fill="purple",
                        alpha=0.6)
s <- s + labs(x="% of Python frames also in ImageJ",
              title="Python in ImageJ, Right Light")
s <- s + coord_cartesian(xlim = c(20, 100))


t <- ggplot(data=df)
t <- t + geom_histogram(mapping=aes(x=df$ImageJ.L.in.python), fill="blue",
                        position="identity", alpha=0.6)
t <- t + geom_histogram(mapping=aes(x=df$ImageJ.R.in.python), fill="red",
                        position="identity", alpha=0.6)
t <- t + geom_histogram(mapping=aes(x=df$Python.L.in.ImageJ), fill="green",
                        position="identity", alpha=0.6)
t <- t + geom_histogram(mapping=aes(x=df$Python.R.in.ImageJ), fill="purple",
                        position="identity", alpha=0.6)

## both of the left lights combined 
v <- ggplot(data=df)
v <- v + geom_histogram(mapping=aes(x=df$ImageJ.L.in.python + df$Python.L.in.ImageJ), 
                        fill="blue", alpha=0.6)
v <- v + labs(x="% combined left", title="Both Analyses of Left Light")
v <- v + coord_cartesian(xlim = c(60, 200))


w <- ggplot(data=df)
w <- w + geom_histogram(mapping=aes(x=df$ImageJ.R.in.python + df$Python.R.in.ImageJ),
                        fill="red", alpha=0.6)
w <- w + labs(x="% combined right", title="Both Analyses of Right Light")
w <- w + coord_cartesian(xlim = c(60, 200))

## creating pretty groups of graphs
u <- plot_grid(p, q, r, s, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)
x <- plot_grid(v, w, labels=c("A", "B"), ncol=2, nrow=1)



