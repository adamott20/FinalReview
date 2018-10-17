#Data sets for Regular season HR total starters Boston, Houston

Houston <- c(8,13,13,15,31,16,10,17,25)

Boston <- c(5,15,10,23,21,16,13,32,43)

#Bootstrap (like the roommate vs you shower time) the difference in means

mean(Houston) - mean(Boston)



#The strength of the team is the team, meaning you don't want a big difference between your biggest
#hr hitter and your worst hr hitter. A test statistic for this is
#hr = (max HR (team 1) - min HR (team 1)) / ((max HR (team 2) - min HR (team 2)) 

(max(Boston)- min(Boston)) / (max(Houston) - min(Houston))
