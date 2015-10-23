library(plyr)
library(ggplot2)
library(Cairo)

group_names <- c('exp','ctrl')
true_effect <- 0

# simulate study with N participants (not all are included in analysis)
n <- 20 * 2

# alternate group
group  <- factor(rep_len(group_names, n))

# randomize age
age    <-  round(rgamma(n, 4, 0.5) + 18)

# randomize gender
gender <- factor(sample(0:1, n, replace=TRUE), labels=c("male", "female"))

# create data frame
allData <- data.frame(
  group  = group,
  age    = age,
  gender = gender,
  y      = scale(rnorm(n, 0, 1))
)

# get indices of all rows belonging to group1
indices <- which(allData$group == 'exp')

# add true effect to all values of group1
allData$y[indices] <- allData$y[indices] + true_effect 

dv <- 'y'

means <- aggregate(allData[,dv], by=allData[c("group","gender")], FUN=mean)

ggplot(allData, aes(x = group, y = y, colour = gender)) + 
  geom_point(data = means, mapping=aes(y = dv), shape = 0, size=8) +
  geom_point(shape=16, size=3) +
  geom_line(data = means, mapping=aes(y = dv, group = gender)) + 
  stat_boxplot(geom ='errorbar',geom_params = list(fill = "white", color = "steelblue"), stat_params = list(color="grey",width = 0.5)) +
  geom_boxplot(width=0.5, colour="grey", fill=NA)

