# homework

## 2M1 Recall the globe tossing model from the chapter. Compute and plot the grid approximate
# posterior distribution for each of the following sets of observations. In each case, assume a uniform
# prior for p.
# (1) W, W, W
# (2) W, W, W, L
# (3) L, W, W, L, W, W, W

p_grid <- seq(from = 0, to = 1, length.out = 100)
prior <- rep(x = 1, length = length(p_grid))

compute_posterior <- function(w, n, prior, p = p_grid) {
  likelihood <- dbinom(x = w, size = n, prob = p)
  unstandardized.posterior <- likelihood * prior
  return( unstandardized.posterior / sum(unstandardized.posterior) )
}

plot_posterior <- function(x, y) {
  plot(x = x, y = y, type="b", xlab = "Probability of Water", ylab = "Posterior Probability")
  title <- paste( length(x), "Points")
  mtext(title)
}

# (1)
w <- 3
n <- 3
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)

# (2)
w <- 3
n <- 4
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)

# (3)
w <- 5
n <- 7
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)

## 2M2 Now assume a prior for p that is equal to zero when p < 0:5 and is a positive constant when
# p >= 0:5. Again compute and plot the grid approximate posterior distribution for each of the sets of
# observations in the problem just above.

prior <- ifelse(test = p_grid < .5, yes = 0, no = 1)

# (1)
w <- 3
n <- 3
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)

# (2)
w <- 3
n <- 4
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)

# (3)
w <- 5
n <- 7
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)

## 2M3 Suppose there are two globes, one for Earth and one for Mars. The Earth globe is 70% covered
# in water. The Mars globe is 100% land. Further suppose that one of these globes—you don’t know
# which—was tossed in the air and produced a “land” observation. Assume that each globe was equally
# likely to be tossed. Show that the posterior probability that the globe was the Earth, conditional on
#seeing “land” (Pr(Earthjland)), is 0.23.

prior <- c(.5, .5) # probability of tossing earth or mars
likelihood <- c(.3, 1) # likelihood of land on earth and mars
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
round( posterior[1], 2) == .23

## 2M4 Suppose you have a deck with only three cards. Each card has two sides, and each side is either
# black or white. One card has two black sides. The second card has one black and one white side. The
# third card has two white sides. Now suppose all three cards are placed in a bag and shuffled. Someone
# reaches into the bag and pulls out a card and places it flat on a table. A black side is shown facing up,
# but you don’t know the color of the side facing down. Show that the probability that the other side is
# also black is 2/3. Use the counting method (Section 2 of the chapter) to approach this problem. This
# means counting up the ways that each card could produce the observed data (a black side facing up
#                                                                            on the table).

card.1.likelihood <- 2
card.2.likelihood <- 1
card.3.likelihood <- 0
likelihood <- c(card.1.likelihood, card.2.likelihood, card.3.likelihood)
prior <- rep(x = 1, length = length(likelihood)) # we don't know which card so all equally likely
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# the probability the other size is black is equal to the probability that we've drawn card 1
posterior[1] == 2/3

## 2M5 Now suppose there are four cards: B/B, B/W, W/W, and another B/B. Again suppose a card is
# drawn from the bag and a black side appears face up. Again calculate the probability that the other
# side is black.

card.1.likelihood <- 2
card.2.likelihood <- 1
card.3.likelihood <- 0
card.4.likelihood <- 2
likelihood <- c(card.1.likelihood, card.2.likelihood, card.3.likelihood, card.4.likelihood)
prior <- rep(x = 1, length = length(likelihood))
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# the probability the other size is black is equal to the probability that we've drawn card 1 or 4
posterior[1] + posterior[4]

## 2M6 Imagine that black ink is heavy, and so cards with black sides are heavier than cards with white
# sides. As a result, it’s less likely that a card with black sides is pulled from the bag. So again assume
# there are three cards: B/B, B/W, and W/W. After experimenting a number of times, you conclude that
# for every way to pull the B/B card from the bag, there are 2 ways to pull the B/W card and 3 ways to
# pull the W/W card. Again suppose that a card is pulled and a black side appears face up. Show that
# the probability the other side is black is now 0.5. Use the counting method, as before.

card.1.likelihood <- 2
card.2.likelihood <- 1
card.3.likelihood <- 0
likelihood <- c(card.1.likelihood, card.2.likelihood, card.3.likelihood)
prior <- c(1, 2, 3)
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# the probability the other size is black is equal to the probability that we've drawn card 1
posterior[1] == .5

## 2M7 Assume again the original card problem, with a single card showing a black side face up. Before
# looking at the other side, we draw another card from the bag and lay it face up on the table. The face
# that is shown on the new card is white. Show that the probability that the first card, the one showing
# a black side, has black on its other side is now 0.75. Use the counting method, if you can. Hint: Treat
# this like the sequence of globe tosses, counting all the ways to see each observation, for each possible
# first card.

card.1.2.likelihood <- 2
card.2.1.likelihood <- 0
card.1.3.likelihood <- 4
card.3.1.likelihood <- 0
card.2.3.likelihood <- 2
card.3.2.likelihood <- 0

likelihood <- c(card.1.2.likelihood, card.2.1.likelihood, card.1.3.likelihood, card.3.1.likelihood, card.2.3.likelihood, card.3.2.likelihood)
prior <- rep(x = 1, length = length(likelihood))
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# the probability that the other side of the first card is black is equal to the probability that the first card is card 1,
# which equals the probability that the sequence we've chosen is either (1, 2), or (1, 3)
posterior[1] + posterior[3] == .75

## 2H1 Suppose there are two species of panda bear. Both are equally common in the wild and live
#  the same places. They look exactly alike and eat the same food, and there is yet no genetic assay
# capable of telling them apart. They differ however in their family sizes. Species A gives birth to twins
# 10% of the time, otherwise birthing a single infant. Species B births twins 20% of the time, otherwise
# birthing singleton infants. Assume these numbers are known with certainty, from many years of field
# research.
# Now suppose you are managing a captive panda breeding program. You have a new female panda
# of unknown species, and she has just given birth to twins. What is the probability that her next birth
# will also be twins?

# find posterior for plausibility of each pandas species following the first birth of twins
species.1.likelihood <- .1
species.2.likelihood <- .2
likelihood <- c(species.1.likelihood, species.2.likelihood)
prior <- c(1, 1)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# probability next birth is set of twins
sum(likelihood*posterior)

## 2H2 Recall all the facts from the problem above. Now compute the probability that the panda we
# have is from species A, assuming we have observed only the first birth and that it was twins.
species.1.likelihood <- .1
species.2.likelihood <- .2
likelihood <- c(species.1.likelihood, species.2.likelihood)
prior <- c(1, 1)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# probability pandas is from species 1
posterior[1]

## 2H3 Continuing on from the previous problem, suppose the same panda mother has a second birth
# that it is not twins, but a singleton infant. Compute the posterior probability that this panda is
# species A.

species.1.likelihood <- .1 * (1 - .1)
species.2.likelihood <- .2 * (1 - .2)
likelihood <- c(species.1.likelihood, species.2.likelihood)
prior <- c(1, 1)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# probability pandas is from species 1
posterior[1]

## 2H4 A common boast of Bayesian statisticians is that Bayesian inference makes it easy to use all of
# the data, even if the data are of different types.
# So suppose now that a veterinarian comes along who has a new genetic test that she claims can
# identify the species of our mother panda. But the test, like all tests, is imperfect. This is the information
# you have about the test:
# * the probability it correctly identifies a species A panda is 0.8.
# * The probability it correctly identifies a species B panda is 0.65.
# The vet administers the test to your panda and tells you that the test is positive for species A. First
# ignore your previous information from the births and compute the posterior probability that your
# panda is species A. Then redo your calculation, now using the birth data as well.

# without birth information
species.1.likelihood <- .8
species.2.likelihood <- 1 - .65
likelihood <- c(species.1.likelihood, species.2.likelihood)
prior <- c(1, 1)
unstandardized.posterior <- likelihood * prior
posterior.vet.test <- unstandardized.posterior / sum(unstandardized.posterior)

# probability pandas is from species 1, given veterinarian test
posterior.vet.test[1]

# with birth information
species.1.likelihood <- .1 * (1 - .1)
species.2.likelihood <- .2 * (1 - .2)
likelihood <- c(species.1.likelihood, species.2.likelihood)
prior <- c(1, 1)
unstandardized.posterior <- likelihood * prior
posterior.birth.info <- unstandardized.posterior / sum(unstandardized.posterior)
posterior.birth.info[1]

# probability pandas is from species 1, given veterinarian test and birth information
composite.unstandardized.posterior <- posterior.vet.test * posterior.birth.info
composite.posterior <- composite.unstandardized.posterior / sum(composite.unstandardized.posterior)
composite.posterior[1]
