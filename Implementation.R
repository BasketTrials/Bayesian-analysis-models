library(R2OpenBUGS)

set.seed(123)

theta.alt = c(0.59, 1.02, 1.17, 0.13, 0.95, 0.75)

true.g1 = 1
true.g2 = 1.3


kMod = 6
Nobs = 90

z1 = rnorm(Nobs, mean = 6, sd = 0.1)
z2 = rnorm(Nobs, mean = 4, sd = 0.1)


# unequal size
Module = c(rep(1, 10), rep(2, 14), rep(3, 10), rep(4, 20), rep(5, 16), rep(6, 20))
true.trt = c(rep(1, 5), rep(0, 5), rep(1, 7), rep(0, 7), rep(1, 5), rep(0, 5), rep(1, 10), rep(0, 10),
             rep(1, 8), rep(0, 8), rep(1, 10), rep(0, 10))

Trt = true.trt
Trt.star = true.trt
Nobs.star = Nobs
true.theta = theta.alt

y = true.g1*z1 + true.g2*z2 + c(true.theta[1]*true.trt[1:10], true.theta[2]*true.trt[11:25],
                                true.theta[3]*true.trt[26:35], true.theta[4]*true.trt[36:55],
                                true.theta[5]*true.trt[56:72], true.theta[6]*true.trt[73:90]) + rnorm(Nobs, mean = 0, sd = 0.4)

y.star = y
Module.star = Module
z1.star = z1
z2.star = z2

#------------------------------- proposed approach -------------------------------#

s0 = 0.1

lSlab = 0.001
uSlab = 10

spike = 100

Prior.theta = rep(0, 6)
Prior.sd.theta = rep(3, 6)

sig.theta = c(0.25, 0.50)


data <- list("kMod", "Nobs", "y", "Module", "z1", "z2", "Trt", "Prior.theta", "Prior.sd.theta",
             "s0", "lSlab", "uSlab", "spike", 
             "Nobs.star", "y.star", "Module.star", "z1.star", "z2.star", "Trt.star", "sig.theta")


inits <- function(){
  list(
    tau.y = 20,
    tau.g1 = 100, tau.g2 = 100
  )
}


parameters <- c("theta.star")

# parameters <- c("pCat")    
# if interested in to computing the interval probabilitis for making Go decision, according to
# Criterion 1: Pr(theta > 0.25) > 90%, or
# Criterion 2: Pr(theta > 0.50) > 90% 

MCMCSim1 <- bugs(data, inits, parameters, "HDist.txt", codaPkg = F, 
                n.chains = 2, n.burnin = 3000, n.iter = 13000)

#------------------------------- standard HM -------------------------------#

Prior.mu.theta = c(0, 1)
Prior.tau.HN = 0.25

sig.theta = c(0.25, 0.50)

data <- list("kMod", "Nobs", "y", "Module", "z1", "z2", "Trt", 
             "Prior.mu.theta", "Prior.tau.HN", "sig.theta")


inits <- function(){
  list(
    tau.y = 20, tau.g1 = 100, tau.g2 = 100,
    mu.theta = 0
  )
}


parameters <- c("theta")

# parameters <- c("pCat")    
# if interested in to computing the interval probabilitis for making Go decision, according to
# Criterion 1: Pr(theta > 0.25) > 90%, or
# Criterion 2: Pr(theta > 0.50) > 90% 

MCMCSim2 <- bugs(data, inits, parameters, "StandardHM.txt", codaPkg = F, 
                n.chains = 2, n.burnin = 3000, n.iter = 13000)


#------------------------------- EXNEX -------------------------------#

Prior.mu.theta = c(0, 1)
Prior.tau.HN = 0.25

sig.theta = c(0.25, 0.50)


nex.theta = 0
nex.sig = 10

pMix = c(0.5, 0.5)

sig.theta = c(0.25, 0.50)


data <- list("kMod", "Nobs", "y", "Module", "z1", "z2", "Trt", 
             "Prior.mu.theta", "Prior.tau.HN", "nex.theta", "nex.sig", "pMix", "sig.theta")


inits <- function(){
  list(
    tau.y = 20, tau.g1 = 100, tau.g2 = 100,
    mu.theta = 0
  )
}



parameters <- c("theta")

# parameters <- c("pCat")    
# if interested in to computing the interval probabilitis for making Go decision, according to
# Criterion 1: Pr(theta > 0.25) > 90%, or
# Criterion 2: Pr(theta > 0.50) > 90% 

MCMCSim3 <- bugs(data, inits, parameters, "EXNEX.txt", codaPkg = F, 
                 n.chains = 2, n.burnin = 3000, n.iter = 13000)


#------------------------------- Stratified -------------------------------#
set.seed(123)

z1a = rnorm(Nobs, mean = 6, sd = 0.1)
z2a = rnorm(Nobs, mean = 4, sd = 0.1)

ya = true.g1*z1a + true.g2*z2a + c(true.theta[1]*true.trt[1:10], true.theta[2]*true.trt[11:25],
                                   true.theta[3]*true.trt[26:35], true.theta[4]*true.trt[36:55],
                                   true.theta[5]*true.trt[56:72], true.theta[6]*true.trt[73:90]) + rnorm(Nobs, mean = 0, sd = 0.4)

Prior.theta = 0
Prior.sd.theta = 10

sig.theta = c(0.25, 0.50)


data <- list("Nobs", "y", "z1", "z2", "Trt", "Prior.theta", "Prior.sd.theta", "sig.theta")

inits <- function(){
  list(
    tau.y = 20
  )
}


parameters <- c("theta")

# parameters <- c("pCat")    
# if interested in to computing the interval probabilitis for making Go decision, according to
# Criterion 1: Pr(theta > 0.25) > 90%, or
# Criterion 2: Pr(theta > 0.50) > 90% 

nthetaM = numeric(6)

for(k in 1:kMod){
  y = ya[Module == k]
  Nobs = length(y)
  z1 = z1a[Module == k]
  z2 = z2a[Module == k]
  Trt = true.trt[Module == k]
  
  MCMCSim4 <- bugs(data, inits, parameters, "Stratified.txt", codaPkg = F, 
                  n.chains = 2, n.burnin = 3000, n.iter = 13000)
  
  nthetaM[k] = MCMCSim4$mean$theta
  

}

