#Data sets for Regular season HR total starters Boston, Houston

Houston <- c(8,13,13,15,31,16,10,17,25)

Boston <- c(5,15,10,23,21,16,13,32,43)

#Bootstrap to create a ci the difference in means

mean(Houston) - mean(Boston)

nreps <- 10000

hr_dist <- replicate(nreps, mean(sample(Houston, replace = TRUE)) - mean(sample(Boston, replace = TRUE)))

quantile(hr_dist, c(.1,.9))

#The strength of the team is the team, meaning you don't want a big difference between your biggest
#hr hitter and your worst hr hitter. A test statistic for this is
#hr = (max HR (team 1) - min HR (team 1)) / ((max HR (team 2) - min HR (team 2)) 

test <- (max(Boston)- min(Boston)) / (max(Houston) - min(Houston))


all <- c(Boston, Houston)
dist <- numeric(nreps)

for (i in 1:nreps){
  x <- sample(1:18, 9, replace = FALSE)
  new1 <- all[x]
  new2 <- all[-x]
  dist[i] <- (max(new1)- min(new1)) / (max(new2) - min(new2))
}

est <- mean(dist >= test)

ci <- est + c(-1,1)*1.96*sqrt(est*(1-est)/nreps)

c(Estimate = est, Lower = ci[1], Upper = ci[2])
