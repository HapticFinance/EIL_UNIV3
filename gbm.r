#!/usr/bin/env Rscript
path = getwd()
source(paste(path, "/install.r", sep=""))

gbm_loop <- function(nsim, t, mu, sigma, S0) {

    T <- t
    dt <- 0.1
    steps <- T/dt
    gbm <- matrix(ncol = nsim, nrow = steps)

    for (simu in 1:nsim) {
        gbm[1, simu] <- S0
        for (i in 2:steps) {
            epsilon <- rnorm(1)
            gbm[i, simu] <- gbm[(i - 1), simu] * exp((mu - sigma * sigma / 2) * dt + sigma * epsilon * sqrt(dt))
        }
    }

    return(gbm)
}
