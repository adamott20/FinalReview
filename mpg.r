#Read in mtcars data set

data(mtcars)

#Select only the mpg and am vectors

cars <- mtcars[,c(1,9)]

#Use permutation test to see if there is a signifcant difference in mpg between automatic
#and manual cars
