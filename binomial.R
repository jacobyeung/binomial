
#Private Checker Functions

library(ggplot2)
library(testthat)

#checking for valid probability value
check_prob = function(prob){
  if(typeof(prob) != "double"){
    stop("pinvalid prob value")
  } else if(prob < 0 | prob > 1){
    stop("p has to be a number between 0 and 1")
  }
  return(TRUE)
}

#Checking for valid value
check.integer = function(x){
  if(round(x) == x){
    return(TRUE)
  }
  return(FALSE)
}

check_trials = function(trials){
  if(!is.double(trials)){
    stop("input must be an integer")
  } else if(trials < 0){
    stop("invalid trials value")
  } else if(round(trials) != trials){
    stop("input must be integer")
  }
  return(TRUE)
}

#Check if input is a valid value for a number of successes
check_success = function(success, trials){
  check_trials(trials)
  if(!isTRUE(all(success == floor(success)))){
    stop("invalid success value")
  }
  if((sum(success[success > trials])) > 0 | sum(success[success < 0]) > 0){
    stop("invalid success value")
  }
  return(TRUE)
}

#Private Auxiliary Functions
# calculates the mean of a binomial distribution
aux_mean = function(trials, prob){
  mean = trials * prob
  return(mean)
}
# calculates the variance of a binomial distribution
aux_variance = function(trials, prob){
  variance = trials * prob * (1 - prob)
  return(variance)
}
# calculates the mode of a binomial distribution
aux_mode = function(trials, prob){
  if(prob == 0.5 & trials %% 2 == 1){
    return(c((trials + 1) * prob, (trials + 1) * prob - 1))
  }
  return(floor((trials + 1) * prob))
}
# calculates the skewness of a binomial distribution
aux_skewness = function(trials, prob){
  if(prob == 1 | prob == 0){
    stop("Invalid prob value")
  } else if(trials == 0){
    stop("Invalid trials value")
  }
  return((1 - 2 * prob)/sqrt(trials * prob * (1 - prob)))
}
# calculates the kurtosis of a binomial distribution
aux_kurtosis = function(trials, prob) {
  if(prob == 0 | prob == 1){
    stop("Invalid prob value")
  } else if(trials == 0){
    stop("Invalid trials value")
  }
  return((1 - 6 * prob * (1 - prob))/(trials * prob * (1 - prob)))
}
aux_mean(10, 0.3)
aux_variance(10, 0.3)
aux_mode(10, 0.3)
aux_skewness(10, 0.3)
aux_kurtosis(10, 0.3)

# Function bin_choose()
#' @title bin_choose
#' @description calculates combinations in which k successes can occur in n trials
#' @param n (integer) number of trials
#' @param k (integer) number of successes
#' @return number of combinations mentioned above in description
#' @export
#' @examples bin_choose(n = 5, k = 2)
#' @examples bin_choose(5, 0)
bin_choose = function(n, k){
  check_trials(n)
  if(sum(k < 0) > 0){
    stop("k must be a non-negative integer")
  }
  else if(sum(k > n) == length(k)){
    stop("k cannot be greater than n")
  }
  return(factorial(n)/(factorial(k) * factorial(n - k)))
}
bin_choose(n = 5, k = 2)
bin_choose(5, 0)
bin_choose(5, 1:3)

#Function bin_probability()
#' @title bin_probability
#' @description calculates the probability of k successes on n repeated trials
#' @param success (integer) number of successes
#' @param trials (integer) number of trials
#' @param prob (numeric) probability of success
#' @return the probability of k successes on n repeated trials
#' @export
#' @examples bin_probability(success = 2, trials = 5, prob = 0.5)
#' @examples bin_probability(success = 0:2, trials = 5, prob = 0.5)
bin_probability = function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  return(bin_choose(trials,success) * prob ** success * (1 - prob) ** (trials - success))
}
bin_probability(success = 2, trials = 5, prob = 0.5)
bin_probability(success = 0:2, trials = 5, prob = 0.5)
bin_probability(success = 55, trials = 100, prob = 0.45)

#Function bin_distribution()}
#' @title bin_distribution
#' @description creates a data.frame with two classes: c("bindis", "data.frame")
#' @param trials (integer) number of trials
#' @param prob (numeric) probability of success
#' @return a data.frame with two classes: c("bindis", "data.frame")
#' @export
#' @examples bin_distribution(trials = 5, prob = 0.5)
bin_distribution = function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  s = c(0:trials)
  f = bin_probability(s, trials, prob)
  distribution = data.frame("Success" = s, "Probability" = f)
  class(distribution) = c("bindis", "data.frame")
  return(distribution)
}
bin_distribution(5, 0.5)
#Function plot.bindis()
#' @export
plot.bindis = function(x){
  barplot(x$Probability, main = "Calculated Binomial Distribution")
}
dis1 <- bin_distribution(trials = 5, prob = 0.5)
plot(dis1)

#Function bin_cumulative()
#' @title bin_cumulative
#' @description creates a data.frame with two classes: c("bincum", "data.frame")
#' @param trials (integer) number of trials
#' @param prob (numeric) probability of success
#' @return a data.frame with two classes: c("bincum", "data.frame")
#' @export
#' @examples bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob) {
  cum = bin_distribution(trials, prob)
  cum$cumulative = cumsum(cum$Probability)
  class(cum) <- c("bincum", "data.frame")
  return(cum)
}
bin_cumulative(trials = 5, prob = 0.5)
#Function plot.bincum()
#' @export
plot.bincum = function(x) {
  ggplot(x, aes(Success, cumulative), geom = "point", size = 10) +
    geom_line()
}
dis2 <- bin_cumulative(trials = 5, prob = 0.5)
plot(dis2)

#Function bin_variable()
#' @title bin_variable
#' @description creates a binomial random variable object of class "binvar"
#' @param trials (integer) number of trials
#' @param prob (numeric) probability of success
#' @return a binomial random variable object of class "binvar"
#' @export
#' @examples bin_variable(trials = 10, p = 0.3)
bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  binomial_variable = list(trials = trials, prob = prob)
  structure(binomial_variable, class = "binvar")
}
#Function print.binvar()
#' @export
print.binvar <- function(x) {
  cat("\"Binomial Variable\"\n\n")
  cat("Parameters\n")
  cat(paste("- number of trials:", x[[1]], "\n"))
  cat(paste("- prob of success :", x[[2]], "\n"))
}
bin1 <- bin_variable(trials = 10, p = 0.3)
bin1
#Function summary.binvar()
#' @export
summary.binvar <- function(x) {
  sum_binvar <- list(
    trials = x[[1]],
    prob = x[[2]],
    mean = aux_mean(x[[1]], x[[2]]),
    variance = aux_variance(x[[1]], x[[2]]),
    mode = aux_mode(x[[1]], x[[2]]),
    skewness = aux_skewness(x[[1]], x[[2]]),
    kurtosis = aux_kurtosis(x[[1]], x[[2]])
  )
  class(sum_binvar) <- "summary.binvar"
  return(sum_binvar)
}
#Function print.summary.binvar()
#' @export
print.summary.binvar <- function(x) {
  cat("\"Summary Binomial\"\n\n")
  cat("Parameters\n")
  cat(paste("- number of trials:", x$trials),"\n")
  cat(paste("- prob of success :", x$prob),"\n\n")
  cat("Measures\n")
  cat(paste("- mean    :", x$mean,"\n"))
  cat(paste("- variance:", x$variance,"\n"))
  cat(paste("- mode    :", x$mode,"\n"))
  cat(paste("- skewness:", x$skewness,"\n"))
  cat(paste("- kurtosis:", x$kurtosis,"\n"))
}
bin1 <- bin_variable(trials = 10, p = 0.3)
binsum1 = summary(bin1)
binsum1

#Functions of measures
#' @title bin_mean()
#' @description calculates the mean of a binomial distribution
#' @param trials (integer) number of trials
#' @param prob (numeric) probability of success
#' @return the mean of a binomial distribution
#' @export
#' @examples bin_mean(10, 0.3)
bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials,prob))
}
#' @title binomial variance
#' @description calculates the variance of a binomial distribution
#' @param trials (integer) number of trials
#' @param prob (numeric) probability of success
#' @return the variance of a binomial distribution
#' @export
#' @examples bin_variance(10, 0.3)
bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}
#' @title binomial mode
#' @description calculates the mode of a binomial distribution
#' @param trials (integer) number of trials
#' @param prob (numeric) probability of success
#' @return the mode of a binomial distribution
#' @export
#' @examples bin_mode(10, 0.3)
bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}
#' @title binomial skewness
#' @description calculates the skewness of a binomial distribution
#' @param trials (integer) number of trials
#' @param prob (numeric) probability of success
#' @return the skewness of a binomial distribution
#' @export
#' @examples bin_skewness(10, 0.3)
bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}
#' @title binomial kurtosis
#' @description calculates the kurtosis of a binomial distribution
#' @param trials (integer) number of trials
#' @param prob (numeric) probability of success
#' @return the kurtosis of a binomial distribution
#' @export
#' @examples bin_kurtosis(10, 0.3)
bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}
bin_mean(10, 0.3)
bin_variance(10, 0.3)
bin_mode(10, 0.3)
bin_skewness(10, 0.3)
bin_kurtosis(10, 0.3)














