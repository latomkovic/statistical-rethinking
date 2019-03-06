# notes

## 5.1 - model divorce rate as a function of the (standarized) median age of marriage
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce

d$MedianAgeMarriage.standardized <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage)) / sd(d$MedianAgeMarriage)
m5.1 <- map(
  alist(
    Divorce ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta.A * MedianAgeMarriage.standardized,
    alpha ~ dnorm(mean = 10, sd = 10),
    beta.A ~ dnorm(mean = 0, sd = 1),
    sigma ~ dunif(min = 0, max = 10)
  ),
  data = d
)

## 5.2 - plot the confidence interval around the mean of the Guassian
median.age.marriage.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu <- link(m5.1, data = data.frame(MedianAgeMarriage.standardized=median.age.marriage.seq))
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI)

plot(Divorce ~ MedianAgeMarriage.standardized, data = d, col=rangi2)
abline(m5.1)
shade(object = mu.PI, lim = median.age.marriage.seq)

## 5.3 - model divorce rate as a function of the (standardized) marriage rate
d$Marriage.standardized <- (d$Marriage - mean(d$Marriage)) / sd(d$Marriage)
m5.2 <- map(
  alist(
    Divorce ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta.R * Marriage.standardized,
    alpha ~ dnorm(mean = 10, sd = 10),
    beta.R ~ dnorm(mean = 0, sd = 1),
    sigma ~ dunif(min = 0, max = 10)
  ),
  data = d
)

## 5.4 - model divorce rate as a function of both marriage rate and median age of marriage
m5.3 <- map(
  alist(
    Divorce ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta.median.age.marriage * MedianAgeMarriage.standardized + beta.marriage.rate * Marriage.standardized,
    alpha ~ dnorm(mean = 10, sd = 10),
    beta.median.age.marriage ~ dnorm(mean = 0, sd = 1),
    beta.marriage.rate ~ dnorm(mean = 0, sd = 1),
    sigma ~ dunif(min = 0, max = 10)
  ),
  data = d
)

## 5.6 - model marriage rate as a function of median age of marriage
m5.4 <- map(
  alist(
    Marriage.standardized ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta.median.age.marriage * MedianAgeMarriage.standardized,
    alpha ~ dnorm(mean = 0, sd = 10),
    beta.median.age.marriage ~ dnorm(mean = 0, sd = 1),
    sigma ~ dunif(min = 0, max = 10)
  ),
  data = d
)

## 5.7 - compute marriage rate residuals
mu <- coef(m5.4)['alpha'] + coef(m5.4)['beta.median.age.marriage'] * d$MedianAgeMarriage.standardized
marriage.rate.residuals <- d$Marriage.standardized - mu

## 5.8 - plot residuals
plot(Marriage.standardized ~ MedianAgeMarriage.standardized, data = d, col=rangi2)
abline(m5.4)
for (i in 1:length(marriage.rate.residuals)) {
  x <- d$MedianAgeMarriage.standardized[i]
  y <- d$Marriage.standardized[i]
  lines( c(x, x), c(mu[i], y), lwd = .5, col = col.alpha("black", .7))
}

## 5.9 - create counterfactual plot for standardized marriage rate vs. divorce rate

# prepare new counterfactual data
median.age.marriage.average <- mean(d$MedianAgeMarriage.standardized)
marriage.rate.seq <- seq(from = -3, to = 3, length.out = 30)
pred.data <- data.frame(Marriage.standardized = marriage.rate.seq, MedianAgeMarriage.standardized = median.age.marriage.average)

# compute counterfactual mean divorce rate
mu <- link(m5.3, data=pred.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI)

# simulate counterfactual divorce rate outcomes
divorce.rate.simulations <- sim(m5.3, data = pred.data, n = 1e4)
divorce.rate.simulations.PI <- apply(X = divorce.rate.simulations, MARGIN = 2, FUN = PI)

# plot results
plot(Divorce ~ Marriage.standardized, data = d, type = "n")
mtext("MedianAgeMarriage.standardized = 0")
lines(marriage.rate.seq, mu.mean)
shade(object = mu.PI, lim = marriage.rate.seq)
shade(object = divorce.rate.simulations.PI, lim = marriage.rate.seq)

## 5.10 - create counterfactual plot for standardized median age of marriage vs. divorce rate

# prepare new counterfactual data
marriage.rate.average <- mean(d$MedianAgeMarriage.standardized)
median.age.marriage.seq <- seq(from = -3, to = 3, length.out = 30)
pred.data <- data.frame(Marriage.standardized = marriage.rate.average, MedianAgeMarriage.standardized = median.age.marriage.seq)

# compute counterfactual mean divorce rate
mu <- link(m5.3, data = pred.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI)

# simulate counterfactual divorce rate outcomes
divorce.rate.simulations <- sim(m5.3, data = pred.data, n = 1e4)
divorce.rate.simulations.PI <- apply(X = divorce.rate.simulations, MARGIN = 2, FUN = PI)

# plot results
plot(Divorce ~ MedianAgeMarriage.standardized, data = d, type = "n")
mtext("MedianAgeMarriage.standardized = 0")
lines(median.age.marriage.seq, mu.mean)
shade(object = mu.PI, lim = median.age.marriage.seq)
shade(object = divorce.rate.simulations.PI, lim = median.age.marriage.seq)

## 5.11 - simulate divorce rates using our original data
mu <- link(m5.3)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI)
divorce.rate.simulations <- sim(m5.3, n = 1e4)
divorce.rate.simulations.PI <- apply(X = divorce.rate.simulations, MARGIN = 2, FUN = PI)

## 5.12 - plot actual divorce rates vs. observed divorce rates
plot( mu.mean ~ d$Divorce , col=rangi2 , ylim=range(mu.PI) ,
      xlab="Observed divorce" , ylab="Predicted divorce" )
abline( a=0 , b=1 , lty=2 )
for ( i in 1:nrow(d) )
  lines( rep(d$Divorce[i],2) , c(mu.PI[1,i],mu.PI[2,i]) ,
         col=rangi2 )

## 5.13
identify(x = d$Divorce, y = mu.mean, labels = d$Loc, cex = .8)

## 5.14 - compute and plot model residuals
divorce.rate.residuals <- d$Divorce - mu.mean
o <- order(divorce.rate.residuals)
dotchart( divorce.rate.residuals[o] , labels=d$Loc[o] , xlim=c(-6,5) , cex=0.6 )
abline( v=0 , col=col.alpha("black",0.2) )
for ( i in 1:nrow(d) ) {
  j <- o[i] # which State in order
  lines( d$Divorce[j]-c(mu.PI[1,j],mu.PI[2,j]) , rep(i,2) )
  points( d$Divorce[j]-c(divorce.rate.simulations.PI[1,j], divorce.rate.simulations.PI[2,j]), rep(i,2), pch=3 , cex=0.6 , col="gray" )
}

# 5.15 - manufacturing spurious association
N <- 100                           # number of cases
x_real <- rnorm(N)                 # x_real as Gaussian w/ mean 0 and stddev 1
x_spur <- rnorm(N, x_real)         # x_spur as Gaussian w/ mean=x_real
y <- rnorm(N, x_real)              # y as Gaussian w/ mean=x_real
d <- data.frame(y,x_real,x_spur)   # bind all together in data frame

# x_spur will easily predict y, but if you put both x_real and x_spur in, x_spur gets knocked out


## 5.16
data(milk)
d <- milk
str(d)
pairs(~kcal.per.g+log(mass)+neocortex.perc, data=d)

## 5.18
d$neocortex.perc # missing values!
## 5.19
dcc <- d[complete.cases(d), ]

## 5.(20?)
m5.5 <- map(
  alist(
    kcal.per.g~dnorm(mu,sigma),
    mu <- a + bp*neocortex.perc,
    a ~ dnorm(0,100),
    bp ~ dnorm(0,1),
    sigma ~ dunif(0,1)
  ),
  data=dcc
)

## 5.21
precis(m5.5, digits=3)

## 5.22
coef(m5.5)["bp"] * ( 76 - 55 ) # a change from smallest to largest 
                               # neocortex (55-75) would result in expected change:

## 5.23
np.seq <- 0:100
pred.data <- data.frame( neocortex.perc=np.seq )
mu <- link( m5.5 , data=pred.data , n=1e4 )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )
plot( kcal.per.g ~ neocortex.perc , data=dcc , col=rangi2 )
lines( np.seq , mu.mean )
lines( np.seq , mu.PI[1,] , lty=2 )
lines( np.seq , mu.PI[2,] , lty=2 )

## 5.24
dcc$log.mass <- log(dcc$mass)

## 5.26
m5.7 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bn*neocortex.perc + bm*log.mass ,
    a ~ dnorm( 0 , 100 ) ,
    bn ~ dnorm( 0 , 1 ) ,
    bm ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 1 )
  ),
  data=dcc)
precis( m5.7)

## 5.27
mean.log.mass <- mean( log(dcc$mass) )
np.seq <- 0:100
pred.data <- data.frame(
  neocortex.perc=np.seq,
  log.mass=mean.log.mass
)
mu <- link( m5.7 , data=pred.data , n=1e4 )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )
plot( kcal.per.g ~ neocortex.perc , data=dcc , type="n" )
lines( np.seq , mu.mean )
lines( np.seq , mu.PI[1,] , lty=2 )
lines( np.seq , mu.PI[2,] , lty=2 )

## 5.28
N <- 100                         # number of cases
rho <- 0.7                       # correlation btw x_pos and x_neg
x_pos <- rnorm( N )              # x_pos as Gaussian
x_neg <- rnorm( N , rho*x_pos ,  # x_neg correlated with x_pos
                sqrt(1-rho^2) )
y <- rnorm( N , x_pos - x_neg )  # y equally associated with x_pos, x_neg
d <- data.frame(y,x_pos,x_neg)   # bind all together in data frame

pairs(d)

## 5.29 - predict height w/ left or right leg   
N <- 100                         # number of individuals
height <- rnorm(N,10,2)          # sim total height of each              
leg_prop <- runif(N,0.4,0.5)     # leg as proportion of height                    
leg_left <- leg_prop*height +    # sim left leg as proportion + error                     
  rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height +   # sim right leg as proportion + error                      
  rnorm( N , 0 , 0.02 )
d <-                             # combine into data frame                      
  data.frame(height,leg_left,leg_right)   

## 5.30
m5.8 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dunif( 0 , 10 )
  ),
  data=d )
precis(m5.8)
plot(precis(m5.8))
post <- extract.samples(m5.8)
plot( bl ~ br , post , col=col.alpha(rangi2,0.1) , pch=16 )

## 5.33
sum_blbr <- post$bl + post$br
dens( sum_blbr , col=rangi2 , lwd=2 , xlab="sum of bl and br" )

d$leg_total <- d$leg_left + d$leg_right
plot(height~leg_total, d, col=rangi2)
leg_list <- seq(from=1,to=15,length.out=30)
leg_dat <- list(leg_left=leg_list,
                leg_right=leg_list)
mu <- link(m5.8, data=leg_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
lines(leg_list*2, mu.mean)
shade(mu.PI, leg_list*2)

## 5.34
m5.9 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    sigma ~ dunif( 0 , 10 ) ),
  data=d )
precis(m5.9)
plot(precis(m5.9))

## 5.36
# kcal.per.g regressed on perc.fat
m5.10 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bf*perc.fat ,
    a ~ dnorm( 0.6 , 10 ) ,
    bf ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ),
  data=dcc )
# kcal.per.g regressed on perc.lactose
m5.11 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bl*perc.lactose ,
    a ~ dnorm( 0.6 , 10 ) ,
    bl ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ), data=dcc )
precis( m5.10 , digits=3 )
precis( m5.11 , digits=3 )

## 5.37
m5.12 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , sigma ) ,
    mu <- a + bf*perc.fat + bl*perc.lactose ,
    a ~ dnorm( 0.6 , 10 ) ,
    bf ~ dnorm( 0 , 1 ) ,
    bl ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ),
  data=dcc )
precis( m5.12 , digits=3 )

## 5.38
pairs( ~ kcal.per.g + perc.fat + perc.lactose ,
       data=dcc , col=rangi2 )

## 5.62
data(cars)
glimmer(dist ~ speed, data = cars)
