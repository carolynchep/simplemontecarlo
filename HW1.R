#function to simulate the roll of three fair dice, returning the sum
rollDie <- function() {
  die <- sample(1:6, 3)
  return(die)
}

# a function to estimate the probability of a sum of 10 when rolling three fair dice,
#returning that probability estimate. Include a parameter that indicates the number of rolls used in
#producing the estimate, defaulting to 1000.
oneEstimate <- function(num_rolls = 1000) {
  # this function returns the estimate of a sum of 10
  # when rolling three fair dice
  count <- 0
  for (i in 1:num_rolls) {
    if (sum(rollDie())== 10) {
      count <- count + 1
    }
  }
  return(count / num_rolls)
}

#a function that will create and return a vector of probability estimates, where each estimate is
#the result of calling your second function above. The current function should have a parameter
#that indicates the number of estimates in the vector (defaulting to 100) and the number of rolls used
#per estimate (defaulting to 1000). Return the vector of estimates at the end


getEstimates <- function(num_estimates = 100, num_rolls = 1000)
{
  # this function creates a vector of size num_estimates and fills
  # that vector with estimates of rolling a sum of 10 using
  # num_rolls rolls per computed estimate
  estimates <- rep(0, num_estimates)
  for (i in 1:num_estimates) {
    estimates[i] <- oneEstimate(num_rolls)
  }
  return(estimates)
}


#a separate function that, given a vector of probability estimates, will produce a histogram of
#those estimates, and will superimpose vertical lines for the mean and for the mean +/- two standard
#deviations.

oneHistogram <- function(num_rolls = 1000)
{
  all_estimates <- getEstimates(100, num_rolls)
  hist(all_estimates, xlim = c(0.05, 0.3))
  m <- mean(all_estimates)
  abline(v = m, col = "blue", lwd = 2)
  s <- sd(all_estimates)
  abline(v = c(m - 2*s, m + 2*s), col = "red", lwd = 2, lty = "dashed") 
}
#oneHistogram(100)

#par(mfrow = c(1,3)) # display 1 row, 3 columns

#Question 5, fixing the number of estimates at 1000 and changing number of rolls to 10, 1000, and 1000
par(mfrow = c(2,3)) # display 2 rows, 3 columns
oneHistogram <- function(num_estimates = 1000, num_rolls = 1000)
{
  all_estimates <- getEstimates(num_estimates, num_rolls)
  hist(all_estimates, xlim = c(0, 0.5), main = "")
  title(paste(num_rolls, "rolls", num_estimates, "estimates"))
  m <- mean(all_estimates)
  abline(v = m, col = "blue", lwd = 2)
  s <- sd(all_estimates)
  abline(v = c(m - 2*s, m + 2*s), col = "red", lwd = 2, lty = "dashed") 
  print(quantile(all_estimates, c(0.5, 0.75, 0.95, 0.99)))
}
oneHistogram(num_rolls = 10)
oneHistogram(num_rolls =  100)
oneHistogram(num_rolls = 1000)



#Question 6
oneHistogram(num_estimates = 10)
oneHistogram(num_estimates = 100)
oneHistogram(num_estimates = 1000)





