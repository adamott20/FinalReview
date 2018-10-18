#Data sets for Regular season HR total starters Boston, Houston

Houston <- c(8,13,13,15,31,16,10,17,25)

Boston <- c(5,15,10,23,21,16,13,32,43)

#My friend bet me dinner that Houston would hit more HR than Boston in this series. I'm
#willing to take this bet if I'm 80% confident that the average Red Sox starter hit more 
#HR than the average Houston starter over the course of the season. Use the bootstrap procedure
#to form an 80% ci on the mean difference in HR's for starters of the two teams. From the 
#ci, should I take the bet?

mean(Houston) - mean(Boston)

nreps <- 10000

hr_dist <- replicate(nreps, mean(sample(Houston, replace = TRUE)) - mean(sample(Boston, replace = TRUE)))

quantile(hr_dist, c(.1,.9))

#I should not take the bet.

#The strength of the team is the team, meaning you don't want a big difference between your biggest
#hr hitter and your worst hr hitter. A test statistic for this is
#hr = (max HR (team 1) - min HR (team 1)) / ((max HR (team 2) - min HR (team 2))
#Use a permutation test to see if this test statistic is abnormal (greater than 5% of the
#distribution)

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

#Based on this p value, I cannot conclude that the test statistic for this series is exceptional.