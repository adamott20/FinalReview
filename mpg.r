#Read in mtcars data set

data(mtcars)

#Select only the mpg and am vectors

car <- mtcars[,c(1,9)]
head(car)
dim(car)

# Set number of repetitions
nreps <- 10000

### PERMUTATION ###
# Is miles per gallon better for manual cars than for automatic cars? (0 automatic, 1 manual)
auto <- car[car$am == 0,1]
manual <- car[car$am == 1,1]
obs_diff <- mean(manual) - mean(auto)

car_perm <- car
diff <- replicate(nreps, {
  car_perm$am <- sample(car$am) 
  auto_new <- car_perm[car_perm$am == 0,1]
  manual_new <- car_perm[car_perm$am == 1,1]
  mean(manual_new) - mean(auto_new)
})
c("p value" = mean(diff >= obs_diff))
# Yes, manual cars have better miles per gallon than automatic cars



### BOOTSTRAP ###
# What is a 95% confidence interval for the mean miles per gallon of automatic cars?
auto <- car[car$am == 0,1]

vec <- replicate(nreps, mean(sample(auto, replace = TRUE)))

quantile(vec, c(.025, .975))
mean(auto)
