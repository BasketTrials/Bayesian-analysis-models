library(R2OpenBUGS)

set.seed(123)

theta.alt = c(0.59, 1.17, 1.02, 0.95, 0.13, 0.75)

true.g0 = 5
true.g1 = 3
true.g2 = 1.3

kMod = 6
Nobs = 90

z1.true = rnorm(Nobs, mean = 6, sd = 0.2)
z2.true = rnorm(Nobs, mean = 4, sd = 0.2)


# unequal size
Module = c(rep(1, 10), rep(2, 10), rep(3, 14), rep(4, 16), rep(5, 20), rep(6, 20))
true.trt = c(rep(1, 5), rep(0, 5), rep(1, 5), rep(0, 5), rep(1, 7), rep(0, 7), 
             rep(1, 8), rep(0, 8), rep(1, 10), rep(0, 10), rep(1, 10), rep(0, 10))


true.theta = theta.alt


y.true = true.g0 + true.g1*z1.true + true.g2*z2.true + c(true.theta[1]*true.trt[1:10], true.theta[2]*true.trt[11:20],
                                true.theta[3]*true.trt[21:34], true.theta[4]*true.trt[35:50],
                                true.theta[5]*true.trt[51:70], true.theta[6]*true.trt[71:90]) + rnorm(Nobs, mean = 0, sd = 0.4)

#------------------------------- Proposed approach -------------------------------#
y = array(0, dim = c(2, Nobs))
y[1,] = y.true
y[2,] = y.true

z1 = array(0, dim = c(2, Nobs))
z1[1, ] = z1.true
z1[2, ] = z1.true

z2 = array(0, dim = c(2, Nobs))
z2[1, ] = z2.true
z2[2, ] = z2.true

Trt = array(0, dim = c(2, Nobs))
Trt[1, ] = true.trt
Trt[2, ] = true.trt

s0 = 0.1


lSlab = 0.01
uSlab = 1

spike = 100

Prior.theta = rep(0, 6)
Prior.sd.theta = rep(10, 6)

cutoff.theta = c(0.25, 0.50)


Prior.g0 = c(0, 5)
Prior.g1 = c(0, 5)
Prior.g2 = c(0, 5)
Prior.sig.HN = c(1, 1, 1)


data <- list("kMod", "Nobs", "y", "Module", "z1", "z2", "Trt", "Prior.theta", "Prior.sd.theta",
             "s0", "lSlab", "uSlab", "spike", "Prior.g0", "Prior.g1", "Prior.g2", "Prior.sig.HN",
             "cutoff.theta")


inits <- function(){
  list(
    tau.y = 10,
    sigma0 = 1, sigma1 = 1, sigma2 = 1
  )
}


parameters <- c("theta", "pCat")

# parameters <- c("pCat")    
# if interested in to computing the interval probabilitis for making Go decision, according to
# Criterion 1: Pr(theta > 0.25) > 90%, or
# Criterion 2: Pr(theta > 0.50) > 90% 

MCMCSim1 <- bugs(data, inits, parameters, "HDist.txt", codaPkg = F,
                n.chains = 2, n.burnin = 3000, n.iter = 13000)


#------------------------------- Standard HM -------------------------------#
y = y.true
z1 = z1.true
z2 = z2.true
Trt = true.trt

Prior.mu.theta = c(0, 10)
Prior.tau.HN = 0.125

cutoff.theta = c(0.25, 0.50)


Prior.g0 = c(0, 5)
Prior.g1 = c(0, 5)
Prior.g2 = c(0, 5)
Prior.sig.HN = c(1, 1, 1)


data <- list("kMod", "Nobs", "y", "Module", "z1", "z2", "Trt", "cutoff.theta",
             "Prior.mu.theta", "Prior.tau.HN", "Prior.g0", "Prior.g1", "Prior.g2", 
             "Prior.sig.HN")


inits <- function(){
  list(
    tau.y = 10, 
    sigma0 = 1, sigma1 = 1, sigma2 = 1
  )
}


parameters <- c("theta", "pCat")

# parameters <- c("pCat")    
# if interested in to computing the interval probabilitis for making Go decision, according to
# Criterion 1: Pr(theta > 0.25) > 97.5%, or
# Criterion 2: Pr(theta > 0.30) > 97.5% 

MCMCSim2 <- bugs(data, inits, parameters, "StandardHM.txt", codaPkg = F, 
                n.chains = 2, n.burnin = 3000, n.iter = 13000)


#------------------------------- EXNEX -------------------------------#
y = y.true
z1 = z1.true
z2 = z2.true
Trt = true.trt

Prior.mu.theta = c(0, 10)
Prior.tau.HN = 0.125

nex.theta = 0
nex.sig = 10

pMix = c(0.5, 0.5)


cutoff.theta = c(0.25, 0.50)


Prior.g0 = c(0, 5)
Prior.g1 = c(0, 5)
Prior.g2 = c(0, 5)
Prior.sig.HN = c(1, 1, 1)


data <- list("kMod", "Nobs", "y", "Module", "z1", "z2", "Trt", "cutoff.theta",
             "Prior.mu.theta", "Prior.tau.HN", "nex.theta", "nex.sig", "pMix",
             "Prior.g0", "Prior.g1", "Prior.g2", "Prior.sig.HN")


# inits <- function(){
#   list(
#     tau.y = 10, 
#     sigma0 = 1, sigma = matrix(c(1, 1), ncol=2),
#     # tau = matrix(c(1, 1), ncol=2),
#     mu.g0 = 0, mu.gamma = c(0, 0), mu.theta = 0
#   )
# }


inits <- function(){
  list(
    tau.y = 10, 
    sigma0 = 1, sigma1 = 1, sigma2 = 1
  )
}


parameters <- c("theta", "pCat")

# parameters <- c("pCat")    
# if interested in to computing the interval probabilitis for making Go decision, according to
# Criterion 1: Pr(theta > 0.25) > 97.5%, or
# Criterion 2: Pr(theta > 0.30) > 97.5% 

MCMCSim3 <- bugs(data, inits, parameters, "EXNEX.txt", codaPkg = F,
                 n.chains = 2, n.burnin = 3000, n.iter = 13000)


#------------------------------- Stratified -------------------------------#
Prior.theta = 0
Prior.sd.theta = 10

cutoff.theta = c(0.25, 0.50)

Prior.g0 = c(0, 5)
Prior.g1 = c(0, 5)
Prior.g2 = c(0, 5)


data <- list("Nobs", "y", "z1", "z2", "Trt", "Prior.theta", "Prior.sd.theta", 
             "Prior.g0", "Prior.g1", "Prior.g2", "cutoff.theta")

inits <- function(){
  list(
    tau.y = 10
    # gamma0 = 1, gamma1 = 1, gamma2 = 1
    # gamma0 = -12, gamma1 = 2, gamma 2 = 2
  )
}


parameters <- c("theta", "pCat")

# parameters <- c("pCat")    
# if interested in to computing the interval probabilitis for making Go decision, according to
# Criterion 1: Pr(theta > 0.25) > 97.5%, or
# Criterion 2: Pr(theta > 0.30) > 97.5% 

nthetaM = numeric(6)

for(k in 1:kMod){
  y = y.true[Module == k]
  Nobs = length(y)
  z1 = z1.true[Module == k]
  z2 = z2.true[Module == k]
  Trt = true.trt[Module == k]
  
  MCMCSim4 <- bugs(data, inits, parameters, "Stratified.txt", codaPkg = F, 
                  n.chains = 2, n.burnin = 3000, n.iter = 13000)
  
  nthetaM[k] = MCMCSim4$mean$theta

}

