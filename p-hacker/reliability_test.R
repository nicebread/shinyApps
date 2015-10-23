library(ggplot2)
library(Cairo)

set.seed(1)

num <- 20    		#number of people to simulate

rel <- .3	#the reliability of the observed extraversion score

item_max  <- 5
item_min  <- 0
item_mean <- (item_max - item_min) / 2 		#mean of true extraversion


items <- 3

#generate the data using the random normal distribution 

#first simulate true (latent) scores
true_scores <- rnorm(num, item_mean)   #true trait extraversion is normally distributed with sigma=1

values <- true_e
groups <- rep("true", length(values))

set.seed(NULL)

sum_scores <- rep(0,num)

# sum up scores
for(g in 1:items) {
  # create scores for each item
  true_scores <- rnorm(num, item_mean)   #true trait extraversion is normally distributed with sigma=1
  
  item_scores <- ( sqrt(rel) * (true_e - item_mean) ) + ( sqrt(1-rel) * rnorm(num) ) + mean_E
  # limit scores according to min and max value of item
  item_scores[item_scores < 0] <- 0
  item_scores[item_scores > 6] <- 6
  # sum up scores
  sum_scores <- sum_scores + ( sqrt(rel)*(true_e-mean_E) + sqrt(1-rel)*rnorm(num) + mean_E )
}

# compute average of scores
sum_scores <- sum_scores / items

values <- c(values, sum_scores)
groups <- c(groups, rep("with rel", num))

  
df <- data.frame(
  group = groups,
  val   = values
)


g <- ggplot(df, aes(x = group, y = val), ymin=0, ymax=6) +
  ylim(0,6) +
  geom_boxplot()
print(g)