# homework
## Easy ----
# These problems use the samples from the posterior distribution for the globe tossing example.
# This code will give you a specific set of samples, so that you can check your answers exactly.
# R code:
p_grid <- seq( from=0 , to=1 , length.out=1000 ) 3.27
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

## 3E1 ----
# How much posterior probability lies below p = 0.2?
sum( posterior[p_grid < .2] )

## 3E2 ----
# How much posterior probability lies above p = 0.8?
sum( posterior[p_grid > .8] )

## 3E3 ----
# How much posterior probability lies between p = 0.2 and p = 0.8?
sum( posterior[p_grid < .8 & p_grid > .2] )

## 3E4 ----
# 20% of the posterior probability lies below which value of p?
quantile(x = samples, probs = .2)

## 3E5 ----
# 20% of the posterior probability lies above which value of p?
quantile(x = samples, probs = .8) #?????????

## 3E6 ----
# Which values of p contain the narrowest interval equal to 66% of the posterior probability?
library(rethinking)
HPDI(samples = samples, prob = .66)

## 3E7 ----
# Which values of p contain 66% of the posterior probability, 
# assuming equal posterior probability both below and above the interval?
low.p <- ((100-66)/2)/100
high.p <- 1-low.p
quantile(samples, c(low.p, high.p)) # ????

## 3M1 ----
# Suppose the globe tossing data had turned out to be 8 water in 15 tosses. 
# Construct the posterior distribution, using grid approximation. 
# Use the same flat prior as before.
w <- 8
n <- 15
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(x = 1, length(p_grid))
likelihood <- dbinom(x = w, size = n, prob = p_grid)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
m1 <- posterior
plot(posterior ~ p_grid, type = "l")

## 3M2 ----
# Draw 10,000 samples from the grid approximation from above. 
# Then use the samples to calculate the 90% HPDI for p.
trials <- 1e4
samples <- sample(x = p_grid, size = trials, prob = posterior, replace = TRUE)
m2 <- HPDI(samples = samples, prob = .9)
m2
plot(posterior ~ p_grid, type = "l")
shade(posterior ~ p_grid, HPDI(samples = samples, prob = .9) )

## 3M3 ----
# Construct a posterior predictive check for this model and data. 
# This means simulate the distribution of samples, averaging over the posterior uncertainty in p.
# What is the probability of observing 8 water in 15 tosses?
posterior.predictive.distribution <- rbinom(n = trials, size = n, prob = samples)
m3p <- posterior.predictive.distribution
simplehist(x = posterior.predictive.distribution, xlab = "Dummy Water Count")
m3 <- mean(posterior.predictive.distribution == 8)
m3

## 3M4 ----
# Using the posterior distribution constructed from the new (8/15) data, now calculate the probability of observing 6 water in 9 tosses.
posterior.predictive.distribution <- rbinom(n = trials, size = 9, prob = samples)
m4p <- posterior.predictive.distribution
simplehist(x = posterior.predictive.distribution, xlab = "Dummy Water Count")
m4 <- mean(posterior.predictive.distribution == 6)
m4

## 3M5 ----
# Start over at 3M1, but now use a prior that is zero below p = 0:5 and a constant above p = 0:5.
# This corresponds to prior information that a majority of the Earth’s surface is water.
# Repeat each problem above and compare the inferences.
# What difference does the better prior make? If it helps,
# compare inferences (using both priors) to the true value p = 0:7.

# 3M5-3M1 ----
# Suppose the globe tossing data had turned out to be 8 water in 15 tosses. 
# Construct the posterior distribution, using grid approximation. 
# Use the same flat prior as before.
w <- 8
n <- 15
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- ifelse(test = p_grid < .5, yes = 0, no = 1)
likelihood <- dbinom(x = w, size = n, prob = p_grid)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
plot(posterior ~ p_grid, type = "l")
lines(m1 ~ p_grid, col='blue', type = "l")

# 3M5-3M2 ----
# Draw 10,000 samples from the grid approximation from above. 
# Then use the samples to calculate the 90% HPDI for p.
trials <- 1e4
samples <- sample(x = p_grid, size = trials, prob = posterior, replace = TRUE)
HPDI(samples = samples, prob = .9)
plot(posterior ~ p_grid, type = "l")
shade(posterior ~ p_grid, HPDI(samples = samples, prob = .9) )
library(scales)
lines(m1 ~ p_grid, col='blue', type = "l")
shade(m1 ~ p_grid, col=alpha('blue',.2), m2)

# 3M5-3M3 ----
# Construct a posterior predictive check for this model and data. 
# This means simulate the distribution of samples, averaging over the posterior uncertainty in p.
# What is the probability of observing 8 water in 15 tosses?
posterior.predictive.distribution <- rbinom(n = trials, size = n, prob = samples)
mean(posterior.predictive.distribution == 8)
m3

# 3M5-3M4 ----
# Using the posterior distribution constructed from the new (8/15) data, now calculate the probability of observing 6 water in 9 tosses.
posterior.predictive.distribution <- rbinom(n = trials, size = 9, prob = samples)
mean(posterior.predictive.distribution == 6)
m4

## Hard ----
# The practice problems here all use the data below.
# These data indicate the gender (male=1, female=0) of officially reported first and second born children in 100 two-child families.
library(rethinking)
data(homeworkch3) # includes birth1 and birth2
# Use these vectors as data.
# So for example to compute the total number of boys born across all of these births,
# you could use:
sum(birth1) + sum(birth2)

## 3H1 ----
# Using grid approximation, compute the posterior distribution for the probability of a birth being a boy.
# Assume a uniform prior probability.
# Which parameter value maximizes the posterior probability?
total.births <- length(birth1) + length(birth2)
boys.born <- sum(birth1 + birth2)
girls.born <- total.births - boys.born

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(x = 1, length(p_grid))
likelihood <- dbinom(x = boys.born, size = total.births, prob = p_grid)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
plot(posterior ~ p_grid, type = "l")

h1 <- p_grid[which.max(posterior)]
h1
abline(v=h1, col='blue', lty=2, lwd=3)

## 3H2 ----
# Using the sample function, draw 10,000 random parameter values from the posterior distribution you calculated above.
# Use these samples to estimate the 50%, 89%, and 97% highest posterior density intervals.
trials <- 1e4
samples <- sample(x = p_grid, size = trials, prob = posterior, replace = TRUE)
h2 <- HPDI(samples = samples, prob = c(.5, .89, .97))
h2
dens(samples, show.HPDI=.97)
dens(samples, show.HPDI=.89, add=TRUE)
dens(samples, show.HPDI=.5, add=TRUE)

## 3H3 ----
# Use rbinom to simulate 10,000 replicates of 200 births.
# You should end up with 10,000 numbers, each one a count of boys out of 200 births.
# Compare the distribution of predicted numbers of boys to the actual count in the data (111 boys out of 200 births).
# There are many good ways to visualize the simulations, but the dens command (part of the rethinking package) is probably the easiest way in this case.
# Does it look like the model fits the data well?
# That is, does the distribution of predictions include the actual observation as a central, likely outcome?
n <- total.births
posterior.predictive.distribution <- rbinom(n = trials, size = n, prob = samples)
dens(posterior.predictive.distribution, adj = .1)
abline(v = boys.born, col = "red", lty=2, lwd=2)

## 3H4 ----
# Now compare 10,000 counts of boys from 100 simulated first borns only to the number of boys in the first births, birth1.
# How does the model look in this light?
n <- 100
sum(birth1)
posterior.predictive.distribution <- rbinom(n = trials, size = n, prob = samples)
dens(posterior.predictive.distribution, adj = .1)
abline(v = sum(birth1), col = "red", lty=2, lwd=2 )

## 3H5 ----
# The model assumes that sex of first and second births are independent.
# To check this assumption, focus now on second births that followed female first borns.
boys.born.after.girls <- birth2[birth1 == 0]
# Compare 10,000 simulated counts of boys to only those second births that followed girls.
# To do this correctly, you need to count the number of first borns who were girls and simulate that many births, 10,000 times.
posterior.predictive.distribution <- rbinom(n = trials, size = length(boys.born.after.girls), prob = samples)
# Compare the counts of boys in your simulations to the actual observed count of boys following girls.
# How does the model look in this light?
# Any guesses what is going on in these data?
dens(posterior.predictive.distribution, adj = .1)
abline(v = sum(boys.born.after.girls), col = "red", lty=2, lwd=2 )
