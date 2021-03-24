library(bayesm)
library(tidyverse)
data(margarine)


## Exercise 1 Data Description

### Average and dispersion in product characteristics
apply(margarine$choicePrice[,3:12],2,mean)
apply(margarine$choicePrice[,3:12],2,sd)

### Market share, and market share by product characteristics

# Market share
table(margarine$choicePrice$choice)/length(margarine$choicePrice$choice)

# Market share by product characteristics
choice_by_price <- t(apply(margarine$choicePrice[,3:12], 1,function(x) x > apply(margarine$choicePrice[,3:12],2,mean)))

choice_by_price_ <- data.frame(cbind(margarine$choicePrice[,2], choice_by_price))
colnames(choice_by_price_) <- c("choice",1:10)
choice_by_price__ <- choice_by_price_ %>% 
  pivot_longer(!choice, names_to = "choice_", values_to = "over_avg") %>%
  filter(choice == choice_) %>%
  select(choice, over_avg)

t(table(choice_by_price__))[1,]/length(margarine$choicePrice$choice)
t(table(choice_by_price__))[2,]/length(margarine$choicePrice$choice)

### Mapping between observed attributes and choices

choice_demos <- merge(margarine$choicePrice, margarine$demos, "hhid")

# mapping between income and choice
table(choice_demos[,c(2,13)])

# mapping between family size and choice
table(choice_demos[,c(2,14)])
table(choice_demos[,c(2,15)])
table(choice_demos[,c(2,16)])

# mapping between education status and choice
table(choice_demos[,c(2,17)])

# mapping between job status and choice
table(choice_demos[,c(2,18)])

# mapping between retirement status and choice
table(choice_demos[,c(2,19)])


## Exercise 2 First Model

# Our first model specification is conditional logit model, since the 
# regressors(price) vary across alternatives.

choice <- 1:10
names(choice) <- 1:10
y <- as.matrix(map_df(choice, function(x) as.integer(margarine$choicePrice$choice == x)))
x_1 <- choice_demos[,3:12]

cl_p <- function(x,b) {
  e <- exp(matrix(rep(c(0,b[1:9]),nrow(x)),byrow = TRUE,nrow(x))+x*b[10])
  e_sum <- apply(e,1,sum)
  return(e/e_sum)
}
cl_ll <- function(y,x,b) {
  ln_p <- log(cl_p(x,b))
  return(-sum(y * ln_p))
}

set.seed(1)
cl <- optim(function(b) cl_ll(y=y,x=x_1,b=b), par = runif(10),method = "BFGS")
cl$par

# The first 9 parameters are intercepts of goods 2 to 10, and the last parameter
#is the effect of price. 

# If one good's intercept is positive, it means that compare to the good 1, 
# individual is more likely to choose that good. One the other hand, if one 
# good's intercept is negative, then individual is less likely to choose that 
# good compare to good 1. 

# The negative sign of last parameter indicates that the higher the price is, 
# the less likely individual will choose that good.


## Exercise 3 Second Model

# Our second model specification is  multinomial logit model, since the 
# regressors(family income) are invariant across alternatives.

x_2 <- as.matrix(choice_demos[,13],ncol=1)

ml_p <- function(x,b) {
  e <- exp(
    matrix(rep(c(0,b[1:9]),nrow(x)),
           byrow = TRUE,
           nrow(x)
    )
    +t(apply(x,1,function(x)x*c(0,b[10:18])))
  )
  e_sum <- apply(e,1,sum)
  return(e/e_sum)
}
ml_ll <- function(y,x,b) {
  ln_p <- log(ml_p(x,b))
  return(-sum(y * ln_p))
}

set.seed(1)
ml <- optim(function(b) ml_ll(y=y,x=x_2,b=b), par = runif(18), method = "BFGS")
ml$par

# The first 9 parameters are intercepts of goods 2 to 10, and the last 9 
# parameters are the effect of income of goods 2 to 10. 

# If one good's sign of effect is positive, it means that individual with higher
# income is more likely to choose that good compare to good 1. One the other
# hand, if one good's sign of effect is negative, individual with higher income 
# is less likely to choose that good compare to good 1.


## Exercise 4 Marginal Effects

### first model 

# prob of i individual choose j
p_1 <- cl_p(x_1,cl$par)
# indicator variable
idc <- array(0, dim = c(nrow(x_1),10,10))
for (i in 1:nrow(x_1)) {
  diag(idc[i,,]) <- 1
}

cl_me <- array(0, dim = c(nrow(x_1),10,10))
for (i in 1:nrow(x_1)) {
  for (j in 1:10) {
    for (k in 1:10) {
      cl_me[i,j,k] <- p_1[i,j]*(idc[i,j,k] - p_1[i,k])*cl$par[10]
    }
  }
}

apply(cl_me, c(2,3), mean)

# It is not surprise that all the diagonal elements are negative while the other
# elements are all positive. It means that if one good's price increase, people 
# will be willing to choose other goods.

### second model 

# prob of i individual choose j
p_2 <- ml_p(x_2,ml$par)
# beta
ml_b <- c(0,ml$par[10:18])

ml_me <- array(0, dim = c(nrow(x_2),10))
for (i in 1:nrow(x_2)) {
  b_bar <- sum(p_2[i,]*ml_b)
  for (j in 1:10) {
    ml_me[i,j] <- p_2[i,j]*(ml_b[j]-b_bar)
  }
}
for (i in 1:nrow(x_2)) {
  b_bar <- sum(p_2[i,]*ml_b)
  ml_me[i,] <- p_2[i,]*(ml_b-b_bar)
}

apply(ml_me, 2, mean)

# For goods 1, 2, 5, 7, if individual's income raise, he or she will choice 
# these goods less, and will turn to the rest goods. 


## Exercise 5 IIA

mx_ll <- function(y,x,b,mx_p) {
  ln_p <- log(mx_p(x,b))
  return(-sum(y * ln_p))
}

### full data

x_3 <- as.matrix(choice_demos[,3:13],ncol=1)

mx_p_1 <- function(x,b) {
  e <- exp(
    matrix(rep(c(0,b[1:9]),nrow(x)),
           byrow = TRUE,
           nrow(x)
    )
    +x[,1:10]*b[10]
    +t(apply(matrix(x[,11],ncol=1),1,function(x)x*c(0,b[11:19])))
  )
  e_sum <- apply(e,1,sum)
  return(e/e_sum)
}

set.seed(1)
mx_1 <- optim(function(b) mx_ll(y=y,x=x_3,b=b,mx_p=mx_p_1), par = runif(19), method = "BFGS")

mx_1$par

### remove second choice

x_4 <- x_3[,-2]

mx_p_2 <- function(x,b) {
  e <- exp(
    matrix(rep(c(0,b[1:8]),nrow(x)),
           byrow = TRUE,
           nrow(x)
    )
    +x[,1:9]*b[9]
    +t(apply(matrix(x[,10],ncol=1),1,function(x)x*c(0,b[10:17])))
  )
  e_sum <- apply(e,1,sum)
  return(e/e_sum)
}

set.seed(1)
mx_2 <- optim(function(b) mx_ll(y=y[,-2],x=x_4,b=b,mx_p=mx_p_2), par = runif(17), method = "BFGS")

mx_2$par

### MTT statistics

l_1 <- mx_ll(y=y[,-2],x=x_4,b=mx_1$par[-c(1,11)],mx_p=mx_p_2)
l_2 <- mx_ll(y=y[,-2],x=x_4,b=mx_2$par,mx_p=mx_p_2)
MTT <- 2*(l_1-l_2)
c_v <- qchisq(0.95, length(mx_2$par))
MTT < c_v

# Since MTT less than critical value, we conclude that under 5% significant 
# level, we cannot reject that IIA hold. 

