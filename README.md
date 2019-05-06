---
title:README
author:"Jacob Yeung"
output:github_document:
html_preview: false

# Binomial Distribution
  
The package "binomial" is a method to calculate the various attributes of a binomial distribution given parameters.

## Using the math Choose function

To calculate the number of comibnations in which k successes can occur in n trials, we use the bin_choose() function:

```{r}
bin_choose(n = 5, k = 2)
```

# Creating a Binomial Random Variable

To create a binomial random variable with k trials and probability prob, we use the function bin_variable():

```{r}
variable = bin_variable(trials = 5, prob = 0.5)
```


## Calculating the Binomial Probability

To calculate the probability of k successes, each independent with probability prob, on n repeated trials, we use the bin_probability() function:

```{r}
bin_probability(success = 2, trials = 5, prob = 0.5)
```

## Calculating the Binomial Distribution

To calculate the binomial distribution of k trials and probability prob, we use the bin_distribution() function:

```{r}
distribution = bin_distribution(k = 5, prob = 0.5)
distribution
```

### Displaying the Binomial Distribution

To plot the above calculated binomial distribution, we use the function plot.bindis():

```{r}
plot.bindis(distribution)
```

## Finding the Cumulative Distribution of a Binomial Random Variable

To find the cumulative distribution of k trials and probability prob, we use the function bin_cumulative():

```{r}
cumulative_distribution = bin_cumulative(trials = 5, prob = 0.5)
cumulative_distribution
```

### Displaying the Cumulative Distribution

To plot the above calculated cumulative distribution, we use the function plot.bincum():

```{r}
plot.bincum(cumulative_distribution)
```

# Creating the Summary of a Binomial Variable

To create the summary of a binomial variable, we use the function summary.binvar():

```{r}
summary.binvar(variable)
```

## Displaing the Summary of a Binomial Variable

To displat the summary of a binomial variable, we use the function print.summary.binvar():

```{r}
print.summary.binvar(variable)
```
