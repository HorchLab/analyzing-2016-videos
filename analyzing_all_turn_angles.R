## Author: Emma Gibbens
## Last updated: July 27, 2023
## Association: Horch Lab, Bowdoin College
## this script is for analyzing all the data from a csv file that contains the 
## turn angle and the correct turn (t/f) for each of the crickets. It attempts to analyze the data and 
## graph the data using ggplot

library(ggplot2)
library(readxl)
library(tidyverse)
library(broom)
library(AICcmodavg)

input <- "/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/all_turn_angle.csv"

data <- read.csv(input, header = TRUE)


data.sex.control <- read_excel(path="/Users/egibbens/Library/CloudStorage/OneDrive-BowdoinCollege/Desktop/Horch_Lab/Data_sex_control.xlsx")

df.filter.treatment <- filter(data.sex.control, control==FALSE)


##
## BEGINNING OF DATA PROCESSING
##

# Function to find row indices based on a specific string in a data frame
find.rows.with.partial.string <- function(data.frame, column.name, search.string) {
  # Convert the specified column to character (in case it's a factor)
  data.frame[[column.name]] <- as.character(data.frame[[column.name]])
  
  # Use grepl to find the row indices where the string is present (partially or fully)
  indices <- which(grepl(search.string, data.frame[[column.name]]))
  
  return(indices)
}


## data frames that contain only the specified days post deafferentation
Baseline <- filter.rows.with.string(data, "cricket.name", "Baseline")
Acute <- filter.rows.with.string(data, "cricket.name", "Acute")
Day3 <- filter.rows.with.string(data, "cricket.name", "_3Days")
Day4 <- filter.rows.with.string(data, "cricket.name", "_4Days")
Day5 <- filter.rows.with.string(data, "cricket.name", "_5Days")
Day14 <- filter.rows.with.string(data, "cricket.name", "14Days")
Day15 <- filter.rows.with.string(data, "cricket.name", "15Days")
Day18 <- filter.rows.with.string(data, "cricket.name", "18Days")
Day19 <- filter.rows.with.string(data, "cricket.name", "19Days")
Day20 <- filter.rows.with.string(data, "cricket.name", "20Days")
Day26 <- filter.rows.with.string(data, "cricket.name", "26Days")


## creating groups of days
Day3_5 <- rbind(Day3, Day4, Day5)
Day14_15 <- rbind(Day14, Day15)
Day18_20 <- rbind(Day18, Day19, Day20)

## adding a column to the data frames that have their period after deafferentation
base <- rep("Baseline", length(Baseline$cricket.name))
Baseline$period <- base

acute <- rep("Acute", length(Acute$cricket.name))
Acute$period <- acute

day3_5 <- rep("Day3_5", length(Day3_5$cricket.name))
Day3_5$period <- day3_5

day14_15 <- rep("Day14_15", length(Day14_15$cricket.name))
Day14_15$period <- day14_15

day18_20 <- rep("Day18_20", length(Day18_20$cricket.name))
Day18_20$period <- day18_20

day26 <- rep("Day26", length(Day26$cricket.name))
Day26$period <- day26

## combining the above data frames into a single df with all the data and periods
new.df <- rbind(Baseline, Acute, Day3_5, Day14_15, Day18_20, Day26)



##
## DATA ANALYSIS - DON'T KNOW IF I USE ANY OF THIS
##

## finding the average turn angles for each group
av.turn.base.R <- mean(Baseline$turn.angle.R)
av.turn.base.L <- mean(Baseline$turn.angle.L)
av.turn.acute.R <- mean(Acute$turn.angle.R)
av.turn.acute.L <- mean(Acute$turn.angle.L)
av.turn.Day3_5.R <- mean(Day3_5$turn.angle.R)
av.turn.Day3_5.L <- mean(Day3_5$turn.angle.L)
av.turn.Day14_15.R <- mean(Day14_15$turn.angle.R)
av.turn.Day14_15.L <- mean(Day14_15$turn.angle.L)
av.turn.Day18_20.R <- mean(Day18_20$turn.angle.R)
av.turn.Day18_20.L <- mean(Day18_20$turn.angle.L)
av.turn.Day26.R <- mean(Day26$turn.angle.R)
av.turn.Day26.L <- mean(Day26$turn.angle.L)


## making a data frame with all the data from the average turn 
av.turn.df.L <- data.frame(av.turn.base.L,
                         av.turn.acute.L,
                         av.turn.Day3_5.L,
                         av.turn.Day14_15.L,
                         av.turn.Day18_20.L,
                         av.turn.Day26.L)

av.turn.df.R <- data.frame(av.turn.base.R,
                           av.turn.acute.R,
                           av.turn.Day3_5.R,
                           av.turn.Day14_15.R,
                           av.turn.Day18_20.R,
                           av.turn.Day26.R)



## calculating what percent turned the correct direction. Input is a list of data
percent.correct <- function(data)
{
  total.len <- length(data)
  correct.total <- which(data == TRUE)
  count <- length(correct.total)
  per.correct <- (count/total.len) * 100
  return(per.correct)
}


## calling the percent.correct function for each condition
per.c.base.R <- percent.correct(Baseline$correct.R)
per.c.base.L <- percent.correct(Baseline$correct.L)
per.c.acute.R <- percent.correct(Acute$correct.R)
per.c.acute.L <- percent.correct(Acute$correct.L)
per.c.Day3_5.R <- percent.correct(Day3_5$correct.R)
per.c.Day3_5.L <- percent.correct(Day3_5$correct.L)
per.c.Day14_15.R <- percent.correct(Day14_15$correct.R)
per.c.Day14_15.L <- percent.correct(Day14_15$correct.L)
per.c.Day18_20.R <- percent.correct(Day18_20$correct.R)
per.c.Day18_20.L <- percent.correct(Day18_20$correct.L)
per.c.Day26.R <- percent.correct(Day26$correct.R)
per.c.Day26.L <- percent.correct(Day26$correct.L)


## creating a data frame with the percent correct from each condition
per.c.df <- data.frame(per.c.base.R,
                       per.c.base.L,
                       per.c.acute.R,
                       per.c.acute.L,
                       per.c.Day3_5.R,
                       per.c.Day3_5.L,
                       per.c.Day14_15.R,
                       per.c.Day14_15.L,
                       per.c.Day18_20.R,
                       per.c.Day18_20.L,
                       per.c.Day26.R,
                       per.c.Day26.L)

per.c.df.R <- data.frame(per.c.base.R,
                         per.c.acute.R,
                         per.c.Day3_5.R,
                         per.c.Day14_15.R,
                         per.c.Day18_20.R,
                         per.c.Day26.R)

per.c.df.L <- data.frame(per.c.base.L,
                       per.c.acute.L,
                       per.c.Day3_5.L,
                       per.c.Day14_15.L,
                       per.c.Day18_20.L,
                       per.c.Day26.L)



## DOING STATISTICS 
## PLEASE DO THIS AGAIN - I THINK IT'S ALL WRONG 
## ATTEMPTING AN ANOVA
## these statistical tests are kinda questionable because we have a 
## repeated measures/grouping hybrid going on

## USING ONLY THE DATA WITH NO CONTROLS - ANOVAS
two.way.L.nc <- aov(turn.angle.L ~ period + sex, data = data.sex.control)

two.way.R.nc <- aov(turn.angle.R ~ period + sex, data = data.sex.control)

two.way.L.nc.av <- aov(turn.L.av ~ period + sex, data = data.sex.control)

two.way.R.nc.av <- aov(turn.R.av ~ period + sex, data = data.sex.control)



## TESTING VARIANCE
# Shapiro-Wilk normality test for baseline - not normal enough
base.sw.R <- with(data.sex.control, shapiro.test(turn.angle.R[period == "Baseline"]))
base.sw.L <- with(data.sex.control, shapiro.test(turn.angle.L[period == "Baseline"]))


## creating data frames with all the data from only two of the periods
df.base.acute <- filter(data.sex.control, period=="Baseline" | period == "Acute")
df.acute.Day3_5 <- filter(data.sex.control, period=="Acute" | period == "Day3_5")
df.acute.Day14_15 <- filter(data.sex.control, period=="Acute" | period == "Day14_15")
df.acute.Day18_20 <- filter(data.sex.control, period=="Acute" | period == "Day18_20")
df.base.Day14_15 <- filter(data.sex.control, period=="Baseline" | period == "Day14_15")
df.Day3_5.Day14_15 <- filter(data.sex.control, period=="Day3_5" | period == "Day14_15")
df.Day14_15.Day18_20 <- filter(data.sex.control, period=="Day14_15" | period == "Day18_20")


## Wilcoxon test with independent samples - doing an unpaired t-test
wilcox.base.acute.L <- wilcox.test(turn.angle.L ~ period, data = df.base.acute,
                   exact = FALSE) ## sig p-value < 2.2e-16
wilcox.acute.day3_5.L <- wilcox.test(turn.angle.L ~ period, data = df.acute.Day3_5,
                                   exact = FALSE) ## not sig p-value = 0.8698
wilcox.acute.day14_15.L <- wilcox.test(turn.angle.L ~ period, data = df.acute.Day14_15,
                                     exact = FALSE) ## sig p-value = 0.009118
wilcox.acute.day18_20.L <- wilcox.test(turn.angle.L ~ period, data = df.acute.Day18_20,
                                       exact = FALSE) ## not sig p-value = 0.1119
wilcox.base.Day14_15.L <- wilcox.test(turn.angle.L ~ period, data = df.base.Day14_15,
                                       exact = FALSE) ## sig p-value = 4.711e-05
wilcox.Day3_5.Day14_15.L <- wilcox.test(turn.angle.L ~ period, data = df.Day3_5.Day14_15,
                                        exact = FALSE) ## sig p-value = 0.02727
wilcox.Day14_15.Day18_20 <- wilcox.test(turn.angle.L ~ period, data = df.Day14_15.Day18_20,
                                        exact = FALSE) ## not sig p-value = 0.2538


## trying to find p-values, please remember that i have no idea what im doing
## finding the difference of two proportions - left correct base vs. acute
h.o.base.acute.L <- 0
## finding the pooled proportion (see 220 of OpenIntro Stats)
num.corr.base.L <- length(which((data.sex.control$correct.L == TRUE)&(data.sex.control$period=="Baseline")))
num.corr.acute.L <- length(which((data.sex.control$correct.L == TRUE)&(data.sex.control$period=="Acute")))
tot.base <- length(which(data.sex.control$period =="Baseline"))
tot.acute <- length(which(data.sex.control$period =="Acute"))
tot.base.acute <- tot.base + tot.acute
p.pool.base.acute.L <- (num.corr.base.L + num.corr.acute.L)/(tot.base.acute)
point.est <- (num.corr.acute.L/tot.acute) - (num.corr.base.L/tot.base)
se.base.acute.L <- sqrt(((p.pool.base.acute.L * (1-p.pool.base.acute.L))/tot.base) +
  ((p.pool.base.acute.L * (1-p.pool.base.acute.L))/tot.acute))
## z score and p.value 
z.base.acute.L <- (point.est - h.o.base.acute.L)/se.base.acute.L
p.val.base.acute.L <- pnorm(z.base.acute.L)




## SWTICHING TO PRISM
iso.fem.base <- which((data.sex.control$period=="Baseline")&(data.sex.control$sex=="F"))
mu.fem.base <- mean(data.sex.control$turn.angle.L[iso.fem.base])
sd.fem.base <- sd(data.sex.control$turn.angle.L[iso.fem.base])
num.fem.base <- length(iso.fem.base)

iso.male.base <- which((data.sex.control$period=="Baseline")&(data.sex.control$sex=="M"))
mu.male.base <- mean(data.sex.control$turn.angle.L[iso.male.base])
sd.male.base <- sd(data.sex.control$turn.angle.L[iso.male.base])
num.male.base <- length(iso.male.base)

iso.fem.acute <- which((data.sex.control$period=="Acute")&(data.sex.control$sex=="F"))
mu.fem.acute <- mean(data.sex.control$turn.angle.L[iso.fem.acute])
sd.fem.acute <- sd(data.sex.control$turn.angle.L[iso.fem.acute])
num.fem.acute <- length(iso.fem.acute)

iso.male.acute <- which((data.sex.control$period=="Acute")&(data.sex.control$sex=="M"))
mu.male.acute <- mean(data.sex.control$turn.angle.L[iso.male.acute])
sd.male.acute <- sd(data.sex.control$turn.angle.L[iso.male.acute])
num.male.acute <- length(iso.male.acute)

iso.fem.day3_5 <- which((data.sex.control$period=="Day3_5")&(data.sex.control$sex=="F"))
mu.fem.day3_5 <- mean(data.sex.control$turn.angle.L[iso.fem.day3_5])
sd.fem.day3_5 <- sd(data.sex.control$turn.angle.L[iso.fem.day3_5])
num.fem.day3_5 <- length(iso.fem.day3_5)

iso.male.day3_5 <- which((data.sex.control$period=="Day3_5")&(data.sex.control$sex=="M"))
mu.male.day3_5 <- mean(data.sex.control$turn.angle.L[iso.male.day3_5])
sd.male.day3_5 <- sd(data.sex.control$turn.angle.L[iso.male.day3_5])
num.male.day3_5 <- length(iso.male.day3_5)

iso.fem.day14_15 <- which((data.sex.control$period=="Day14_15")&(data.sex.control$sex=="F"))
mu.fem.day14_15 <- mean(data.sex.control$turn.angle.L[iso.fem.day14_15])
sd.fem.day14_15 <- sd(data.sex.control$turn.angle.L[iso.fem.day14_15])
num.fem.day14_15 <- length(iso.fem.day14_15)

iso.male.day14_15 <- which((data.sex.control$period=="Day14_15")&(data.sex.control$sex=="M"))
mu.male.day14_15 <- mean(data.sex.control$turn.angle.L[iso.male.day14_15])
sd.male.day14_15 <- sd(data.sex.control$turn.angle.L[iso.male.day14_15])
num.male.day14_15 <- length(iso.male.day14_15)

iso.fem.day18_20 <- which((data.sex.control$period=="Day18_20")&(data.sex.control$sex=="F"))
mu.fem.day18_20 <- mean(data.sex.control$turn.angle.L[iso.fem.day18_20])
sd.fem.day18_20 <- sd(data.sex.control$turn.angle.L[iso.fem.day18_20])
num.fem.day18_20 <- length(iso.fem.day18_20)


iso.male.day18_20 <- which((data.sex.control$period=="Day18_20")&(data.sex.control$sex=="M"))
mu.male.day18_20 <- mean(data.sex.control$turn.angle.L[iso.male.day18_20])
sd.male.day18_20 <- sd(data.sex.control$turn.angle.L[iso.male.day18_20])
num.male.day18_20 <- length(iso.male.day18_20)




##
## MAKING THE GRAPHS
##

## the pretty graphs

##
## 
## taking into account the controls in the experiment 
## 
## 



## right turn using the largest difference in turn angle from when the stimulus
## is on and the baseline average - separating by treatment and control
turn.R.tvc <- ggplot(data=data.sex.control) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="red", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="green", alpha=0.005)+
  geom_boxplot(mapping=aes(y = turn.angle.R, x= period, 
                          fill=control)) +
  labs(title = "Right Stimulus - Max Diff", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  stat_summary(aes(y = turn.angle.R, x=period,
                   fill=control),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days")) +
  scale_fill_discrete(name = "Exp. Condition", labels=c("treatment", "control"))


## left turn using the largest difference in turn angle from when the stimulus
## is on and the baseline average - separating by treatment and control
turn.L.tvc <- ggplot(data=data.sex.control) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="green", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="red", alpha=0.005)+
  geom_boxplot(mapping=aes(y = turn.angle.L, x=period, 
                           fill=control)) +
  labs(title = "Left Stimulus - Max Diff", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  stat_summary(aes(y = turn.angle.L, x=period,
                   fill=control),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days")) +
  scale_fill_discrete(name = "Exp. Condition", labels=c("treatment", "control"))



## right turn using the difference in average turn angle from when the stimulus
## is on and the baseline average - includes control
turn.R.av.tvc <- ggplot(data=data.sex.control) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="red", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="green", alpha=0.005)+
  geom_boxplot(mapping=aes(y = turn.R.av, x=period, 
                           fill=control)) +
  labs(title = "Right Stimulus - Average", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  stat_summary(aes(y = turn.R.av, x=period,
                   fill=control),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days")) +
  scale_fill_discrete(name = "Exp. Condition", labels=c("treatment", "control"))


## left turn using the difference in average turn angle from when the stimulus
## is on and the baseline average - includes control
turn.L.av.tvc <- ggplot(data=data.sex.control) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="green", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="red", alpha=0.005)+
  geom_boxplot(mapping=aes(y = turn.L.av, x=period, 
                           fill=control)) +
  labs(title = "Left Stimulus - Average", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  stat_summary(aes(y = turn.L.av, x=period,
                   fill=control),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days")) +
  scale_fill_discrete(name = "Exp. Condition", labels=c("treatment", "control"))






## doing the same as above, but adding filters for sex 

## right turn using the largest difference in turn angle from when the stimulus
## is on and the baseline average - separating by treatment and control, faceted
## by sex
turn.R.tvc.sex <- ggplot(data=data.sex.control) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="red", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="green", alpha=0.005)+
  geom_boxplot(mapping=aes(y = turn.angle.R, x=period, 
                           fill=control)) +
  labs(title = "Right Stimulus - Max Diff", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  stat_summary(aes(y = turn.angle.R, x=period,
                   fill=control),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days")) +
  scale_fill_discrete(name = "Exp. Condition", labels=c("treatment", "control")) +
  facet_wrap(~sex, labeller = as_labeller(c("F" = "Female", 
                                                             "M" = "Male"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## left turn using the largest difference in turn angle from when the stimulus
## is on and the baseline average - separating by treatment and control, faceted
## by sex
turn.L.tvc.sex <- ggplot(data=data.sex.control) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="green", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="red", alpha=0.005)+
  geom_boxplot(mapping=aes(y = turn.angle.L, x=period, 
                           fill=control)) +
  labs(title = "Left Stimulus - Max Diff", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  stat_summary(aes(y = turn.angle.L, x=period,
                   fill=control),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days")) +
  scale_fill_discrete(name = "Exp. Condition", labels=c("treatment", "control")) +
  facet_wrap(~sex, labeller = as_labeller(c("F" = "Female", 
                                                             "M" = "Male"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## right turn using the difference in average turn angle from when the stimulus
## is on and the baseline average - organize by treatment type, filter sex
turn.R.av.sex <- ggplot(data=data.sex.control) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="red", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="green", alpha=0.005)+
  geom_boxplot(mapping=aes(y = turn.R.av, x=period, 
                           fill=control)) +
  labs(title = "Right Stimulus - Average", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  stat_summary(aes(y = turn.R.av, x=period,
                   fill=control),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days")) +
  scale_fill_discrete(name = "Exp. Condition", labels=c("treatment", "control")) +
  facet_wrap(~sex, labeller = as_labeller(c("F" = "Female", "M" = "Male"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## left turn using the difference in average turn angle from when the stimulus
## is on and the baseline average - includes control - filters sex
turn.L.av.sex <- ggplot(data=data.sex.control) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="green", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="red", alpha=0.005)+
  geom_boxplot(mapping=aes(y = turn.L.av, x=period, 
                           fill=control)) +
  labs(title = "Left Stimulus - Average", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  stat_summary(aes(y = turn.L.av, x=period,
                   fill=control),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days")) +
  scale_fill_discrete(name = "Exp. Condition", labels=c("treatment", "control")) +
  facet_wrap(~sex, labeller = as_labeller(c("F" = "Female", "M" = "Male"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## finding the proportion correct for controls vs. no controls
per.correct.R.wc <- ggplot(data=data.sex.control) +
  geom_bar(mapping=aes(x=period, fill=correct.R), 
           position="fill") +
  labs(title="Right Stimulus - Proportion Correct",
       x = "Period post-deafferentation",
       y="Proportion turned correct direction") +
  scale_fill_discrete(name="Correct turn? (T/F)") + 
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days")) +
  facet_wrap(~control, labeller = as_labeller(c("FALSE" = "Treatment", 
                                                "TRUE" = "Control"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## finding the proportion correct for controls vs. no controls
per.correct.L.wc <- ggplot(data=data.sex.control) +
  geom_bar(mapping=aes(x=period, fill=correct.L), 
           position="fill") +
  labs(title="Left Stimulus - Proportion Correct",
       x = "Period post-deafferentation",
       y="Proportion turned correct direction") +
  scale_fill_discrete(name="Correct turn? (T/F)") + 
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days")) +
  facet_wrap(~control, labeller = as_labeller(c("FALSE" = "Treatment", 
                                                "TRUE" = "Control"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## 
## find the turn angle, but filter out the controls and just group by sex
## DOING ALL THE SAME THINGS JUST NO CONTROLS
## 


## right turn using the largest difference in turn angle from when the stimulus
## is on and the baseline average  - NO CONTROLS
turn.R.sex <- ggplot(data=df.filter.treatment) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="red", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="green", alpha=0.005)+
  geom_boxplot(mapping=aes(y = turn.angle.R, 
                           x=period, 
                           fill=sex)) +
  labs(title = "Right Stimulus - Max Diff (Treatment Only)", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  stat_summary(aes(y = turn.angle.R, x=period,
                   fill=sex),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day3_5" = "3-5 Days")) + 
  scale_fill_discrete(name = "Sex", labels=c("Female", "Male"))


## left turn using the largest difference in turn angle from when the stimulus
## is on and the baseline average  - NO CONTROLS
turn.L.sex <- ggplot(data=df.filter.treatment) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="green", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="red", alpha=0.005)+
  geom_boxplot(mapping=aes(y = turn.angle.L, 
                           x=period, 
                           fill=sex)) +
  labs(title = "Left Stimulus - Max Diff (Treatment Only)", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  stat_summary(aes(y = turn.angle.L, x=period,
                   fill=sex),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day3_5" = "3-5 Days")) + 
  scale_fill_discrete(name = "Sex", labels=c("Female", "Male"))


## right turn using the difference in average turn angle from when the stimulus
## is on and the baseline average  - NO CONTROLS
turn.R.av.nc.sex <- ggplot(data=df.filter.treatment) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="red", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="green", alpha=0.005)+
  geom_boxplot(mapping=aes(y = turn.R.av, 
                           x=period, 
                           fill=sex)) +
  labs(title = "Right Stimulus - Average Diff (Treatment Only)", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  stat_summary(aes(y = turn.R.av, x=period,
                   fill=sex),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day3_5" = "3-5 Days")) + 
  scale_fill_discrete(name = "Sex", labels=c("Female", "Male"))



## left turn using the difference in average turn angle from when the stimulus
## is on and the baseline average  - NO CONTROLS
turn.L.av.nc.sex <- ggplot(data=df.filter.treatment) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="green", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="red", alpha=0.005)+
  geom_boxplot(mapping=aes(y = turn.L.av, 
                           x=period, 
                           fill=sex)) +
  labs(title = "Left Stimulus - Average Diff (Treatment Only)", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  stat_summary(aes(y = turn.L.av, x=period,
                   fill=sex),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day3_5" = "3-5 Days")) + 
  scale_fill_discrete(name = "Sex", labels=c("Female", "Male"))


## finding the sample sizes for NO CONTROL
sample.size.nc <- c((length(which(df.filter.treatment$period=="Baseline"))/2), 
                 (length(which(df.filter.treatment$period=="Acute"))/2),
                 (length(which(df.filter.treatment$period=="Day3_5"))/2),
                 (length(which(df.filter.treatment$period=="Day14_15"))/2),
                 (length(which(df.filter.treatment$period=="Day18_20"))/2))


## bar chart of proportion correct - NO CONTROL
per.correct.R.nc <- ggplot(data=df.filter.treatment) +
  geom_bar(mapping=aes(x=period, fill=correct.R), 
           position="fill") +
  labs(title="Right Stimulus - Proportion Correct (Treatment Only)",
       x = "Period post-deafferentation",
       y="Proportion turned correct direction") +
  scale_fill_discrete(name="Correct turn? (T/F)") + 
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day3_5" = "3-5 Days")) +
  annotate(geom ="text", x=c("Baseline", "Acute", "Day3_5", "Day14_15", 
                             "Day18_20"), y=0.05, label=sample.size.nc, 
           size=5, colour="white")


per.correct.L.nc <- ggplot(data=df.filter.treatment) +
  geom_bar(mapping=aes(x=period, fill=correct.L), 
           position="fill") +
  labs(title="Left Stimulus - Proportion Correct (Treatment Only)",
       x = "Period post-deafferentation",
       y="Proportion turned correct direction") +
  scale_fill_discrete(name="Correct turn? (T/F)") + 
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day3_5" = "3-5 Days")) +
  annotate(geom ="text", x=c("Baseline", "Acute", "Day3_5", "Day14_15", 
                             "Day18_20"), y=0.05, label=sample.size.nc, 
           size=5, colour="white")

## percent correct L faceted by sex
per.correct.L.nc.sex <- ggplot(data=df.filter.treatment) +
  geom_bar(mapping=aes(x=period, fill=correct.L), 
           position="fill") +
  labs(title="Left Stimulus - Proportion Correct (Treatment Only)",
       x = "Period post-deafferentation",
       y="Proportion turned correct direction") +
  scale_fill_discrete(name="Correct turn? (T/F)") + 
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day3_5" = "3-5 Days"))  + 
  facet_wrap(~df.filter.treatment$sex, 
             labeller = as_labeller(c("F" = "Female", "M" = "Male"))) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##
##
## MAKING THE HISTOGRAMS WITHOUT THE CONTROL

## histogram of the turn angles for the left stimulus - using max diff
## grouped by sex
turn.L.hist.nc <- ggplot(data=df.filter.treatment, 
                         aes(x=turn.angle.L, fill=sex, color=sex)) +
  geom_histogram(binwidth=1, position="dodge", alpha=0.5) +
  facet_wrap(~factor(period,
                     levels = c("Baseline", "Acute", "Day3_5", "Day14_15", "Day18_20")),
             labeller = as_labeller(c("Acute"="Acute", "Baseline"="Baseline",
                                      "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                                      "Day3_5" = "3-5 Days")),
             scales="free_y") +
  geom_vline(aes(xintercept=0), color="blue") +
  labs(title="Left Stimulus - Histogram of Turn Angles (Max Diff, Treatment Only)",
       x = "Turn angle in degrees (+ is R, - is L)")


## histogram of the turn angles for the right stimulus - using max diff 
## grouped by sex
turn.R.hist.nc <- ggplot(data=df.filter.treatment, 
                         aes(x=turn.angle.R, fill=sex, color=sex)) +
  geom_histogram(binwidth=1, position="dodge", alpha=0.5) +
  facet_wrap(~factor(period,
                     levels = c("Baseline", "Acute", "Day3_5", "Day14_15", "Day18_20")),
             labeller = as_labeller(c("Acute"="Acute", "Baseline"="Baseline",
                                      "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                                      "Day3_5" = "3-5 Days")),
             scales="free_y") +
  geom_vline(aes(xintercept=0), color="blue") +
  labs(title="Right Stimulus - Histogram of Turn Angles (Max Diff, Treatment Only)",
       x = "Turn angle in degrees (+ is R, - is L)")


## histogram of the turn angles for the left stimulus - using average
## grouped by sex - NO CONTROL
turn.L.hist.av.nc <- ggplot(data=df.filter.treatment, 
                         aes(x=turn.L.av, fill=sex, color=sex)) +
  geom_histogram(binwidth=1, position="dodge", alpha=0.5) +
  facet_wrap(~factor(period,
                     levels = c("Baseline", "Acute", "Day3_5", "Day14_15", "Day18_20")),
             labeller = as_labeller(c("Acute"="Acute", "Baseline"="Baseline",
                                      "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                                      "Day3_5" = "3-5 Days")),
             scales="free_y") +
  geom_vline(aes(xintercept=0), color="blue") +
  labs(title="Left Stimulus - Histogram of Turn Angles (Average, Treatment Only)",
       x = "Turn angle in degrees (+ is R, - is L)")



## histogram of the turn angles for the right stimulus - using average
## grouped by sex - NO CONTROL
turn.R.hist.av.nc <- ggplot(data=df.filter.treatment, 
                            aes(x=turn.R.av, fill=sex, color=sex)) +
  geom_histogram(binwidth=1, position="dodge", alpha=0.5) +
  facet_wrap(~factor(period,
                     levels = c("Baseline", "Acute", "Day3_5", "Day14_15", "Day18_20")),
             labeller = as_labeller(c("Acute"="Acute", "Baseline"="Baseline",
                                      "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                                      "Day3_5" = "3-5 Days")),
             scales="free_y") +
  geom_vline(aes(xintercept=0), color="blue") +
  labs(title="Right Stimulus - Histogram of Turn Angles (Average, Treatment Only)",
       x = "Turn angle in degrees (+ is R, - is L)")






##
##
## OLD (PRE FILTERNING FOR CONTROLS) GRAPHS
##
##




## right turn using the largest difference in turn angle from when the stimulus
## is on and the baseline average 
turn.R <- ggplot(data=new.df) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="red", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="green", alpha=0.005)+
  geom_boxplot(mapping=aes(y = new.df$turn.angle.R, x=new.df$period, 
                           fill=new.df$period)) +
  labs(title = "Right Stimulus - Max Diff", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  theme(legend.position="none") +
  stat_summary(aes(y = new.df$turn.angle.R, x=new.df$period),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days")) 


## left turn using the largest difference in turn angle from when the stimulus
## is on and the baseline average 
turn.L <- ggplot(data=new.df) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="green", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="red", alpha=0.005)+
  geom_boxplot(mapping=aes(y = new.df$turn.angle.L, x=new.df$period, 
                           fill=new.df$period)) +
  labs(title = "Left Stimulus - Max Diff", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  theme(legend.position="none") +
  stat_summary(aes(y = new.df$turn.angle.L, x=new.df$period),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days"))


## right turn using the average difference in turn angle from when the stimulus
## is on and the baseline average 
turn.R.av <- ggplot(data=new.df) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="red", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="green", alpha=0.005)+
  geom_boxplot(mapping=aes(y = new.df$turn.R.av, x=new.df$period, 
                           fill=new.df$period)) +
  labs(title = "Left Stimulus - Average Diff", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  theme(legend.position="none") +
  stat_summary(aes(y = new.df$turn.R.av, x=new.df$period),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days"))

## left turn using the average difference in turn angle from when the stimulus
## is on and the baseline average 
turn.L.av <- ggplot(data=new.df) + 
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=0, ymax=20), fill="green", alpha=0.005)+
  geom_rect(mapping=aes(xmin=-Inf, xmax=Inf, ymin=-25, ymax=0), fill="red", alpha=0.005)+
  geom_boxplot(mapping=aes(y = new.df$turn.L.av, x=new.df$period, 
                           fill=new.df$period)) +
  labs(title = "Left Stimulus - Average Diff", 
       x = "Period post-deafferentation",
       y="Turn angle in degrees (+ is R, - is L)") +
  theme(legend.position="none") +
  stat_summary(aes(y = new.df$turn.L.av, x=new.df$period),
               fun=mean, geom="point", shape=23, size=4) +
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days"))


sample.size <- c((length(which(new.df$period=="Baseline"))/2), 
                 (length(which(new.df$period=="Acute"))/2),
                 (length(which(new.df$period=="Day3_5"))/2),
                 (length(which(new.df$period=="Day14_15"))/2),
                 (length(which(new.df$period=="Day18_20"))/2),
                 (length(which(new.df$period=="Day26"))/2))


## bar chart of proportion correct
per.correct.R <- ggplot(data=new.df) +
  geom_bar(mapping=aes(x=new.df$period, fill=new.df$correct.R), 
           position="fill") +
  labs(title="Right Stimulus - Proportion Correct",
       x = "Period post-deafferentation",
       y="Proportion turned correct direction") +
  scale_fill_discrete(name="Correct turn? (T/F)") + 
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days")) +
  annotate(geom ="text", x=c("Baseline", "Acute", "Day3_5", "Day14_15", 
                             "Day18_20", "Day26"), y=0.05, label=sample.size, 
           size=5, colour="white")


## bar chart of proportion incorrect
per.correct.L <- ggplot(data=new.df) +
  geom_bar(mapping=aes(x=new.df$period, fill=new.df$correct.L), 
           position="fill") +
  labs(title="Left Stimulus - Proportion Correct",
       x = "Period post-deafferentation",
       y="Proportion turned correct direction") +
  scale_fill_discrete(name="Correct turn? (T/F)") + 
  scale_x_discrete(limits = c("Baseline", "Acute", "Day3_5", "Day14_15", 
                              "Day18_20", "Day26"), 
                   labels=c("Acute"="Acute", "Baseline"="Baseline",
                            "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                            "Day26" = "26 Days", "Day3_5" = "3-5 Days")) +
  annotate(geom ="text", x=c("Baseline", "Acute", "Day3_5", "Day14_15", 
                             "Day18_20", "Day26"), y=0.05, label=sample.size, 
           size=5, colour="white")



## histogram of the turn angles for the left stimulus - using max diff
turn.L.hist <- ggplot(data=new.df, aes(x=new.df$turn.angle.L)) +
  geom_histogram(binwidth=1) +
  facet_wrap(~factor(new.df$period,
                     levels = c("Baseline", "Acute", "Day3_5", "Day14_15", "Day18_20", "Day26")),
             labeller = as_labeller(c("Acute"="Acute", "Baseline"="Baseline",
                                      "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                                      "Day26" = "26 Days", "Day3_5" = "3-5 Days")),
             scales="free_y") +
  geom_vline(aes(xintercept=0), color="blue") +
  labs(title="Left Stimulus - Histogram of Turn Angles (Max Diff)",
       x = "Turn angle in degrees (+ is R, - is L)")


## histogram of the turn angles for the right stimulus - using max diff
turn.R.hist <- ggplot(data=new.df, aes(x=new.df$turn.angle.R)) +
  geom_histogram(binwidth=1) +
  facet_wrap(~factor(new.df$period,
                     levels = c("Baseline", "Acute", "Day3_5", "Day14_15", "Day18_20", "Day26")),
             labeller = as_labeller(c("Acute"="Acute", "Baseline"="Baseline",
                                      "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                                      "Day26" = "26 Days", "Day3_5" = "3-5 Days")),
             scales="free_y") +
  geom_vline(aes(xintercept=0), color="blue") +
  labs(title="Right Stimulus - Histogram of Turn Angles (Max Diff)",
       x = "Turn angle in degrees (+ is R, - is L)")


## histogram of turn angles for the right stimulus - using average diff
turn.av.R.hist <- ggplot(data=new.df, aes(x=new.df$turn.R.av)) +
  geom_histogram(binwidth=1) +
  facet_wrap(~factor(new.df$period,
                     levels = c("Baseline", "Acute", "Day3_5", "Day14_15", "Day18_20", "Day26")),
             labeller = as_labeller(c("Acute"="Acute", "Baseline"="Baseline",
                                      "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                                      "Day26" = "26 Days", "Day3_5" = "3-5 Days")),
             scales="free_y") +
  geom_vline(aes(xintercept=0), color="blue") +
  labs(title="Right Stimulus - Histogram of Turn Angles (Average)",
       x = "Turn angle in degrees (+ is R, - is L)")


## histogram of turn angles for the left stimulus - using average diff
turn.av.L.hist <- ggplot(data=new.df, aes(x=new.df$turn.L.av)) +
  geom_histogram(binwidth=1) +
  facet_wrap(~factor(new.df$period,
                     levels = c("Baseline", "Acute", "Day3_5", "Day14_15", "Day18_20", "Day26")),
             labeller = as_labeller(c("Acute"="Acute", "Baseline"="Baseline",
                                      "Day14_15"="14-15 Days", "Day18_20"="18-20 Days",
                                      "Day26" = "26 Days", "Day3_5" = "3-5 Days")),
             scales="free_y") +
  geom_vline(aes(xintercept=0), color="blue") +
  labs(title="Left Stimulus - Histogram of Turn Angles (Average)",
       x = "Turn angle in degrees (+ is R, - is L)")



