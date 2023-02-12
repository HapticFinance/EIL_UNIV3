#!/usr/bin/env Rscript
path = getwd()
source(paste(path, "/install.r", sep=""))

# Initial parameters

# Number of simulations
n_sim <- 25000

# Initial price
S0 <- 1000

# Volatility
sigma <- 0.001

# Drift
mu <- 0.000075

# Time
T <- 168

# EIL threshold
EIL_THRESHOLD <- 0.001

# Total value of LP position in numeraire token
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