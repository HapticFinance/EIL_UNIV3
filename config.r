#!/usr/bin/env Rscript
path = getwd()
source(paste(path, "/install.r", sep=""))

# Initial parameters (Define the initial parameters for the simulation)

# Number of simulations
n_sim <- 1000

# Initial price
S0 <- 1000

# Volatility of the stock price
sigma <- 0.001

# Drift of the stock price
mu <- 0.000075

# Initial variance of the variance process
v0 <- 0.04

# Mean reversion speed of the variance process
kappa <- 1

# Long-term variance of the variance process
theta <-  0.04

# Volatility of the variance process
sigma_v <- 0.2

# Correlation between the stock price and the variance processes
rho <- -0.5

#  Time horizon of the simulation
T <- 168

# EIL threshold (below this value scientific notation is used)
EIL_THRESHOLD <- 0.001

# Total value of LP position in the numéraire token (e.g. DAI)
V <- 5000

# Liquidity ranges
ranges <- list(
    c(999, 1001),
    c(990, 1010),
    c(909, 1100),
    c(833.33, 1200),
    c(500, 2000),
    c(200, 5000)
)