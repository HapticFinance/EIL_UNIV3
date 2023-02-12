#!/usr/bin/env Rscript
path = getwd()
source(paste(path, "/install.r", sep=""))
source(paste(path, "/gbm.r", sep=""))
set.seed(3)

calc_abs_il <- function(P, P1, Pa, Pb) {

    range_factor <- sqrt(Pb / Pa)
    px <- P1 / sqrt(Pb * Pa)

    CE <- (sqrt(range_factor) / (sqrt(range_factor) - 1))

    a1 <- (sqrt(range_factor) - px) /  (px + 1)
    a2 <- (CE) * (((2 * sqrt(px)) / (px + 1)) -1)
    a3 <- ((px * (sqrt(range_factor) - 1))) / (px + 1)

    IL <- 0

    if (range_factor <= 5) {
        if ((Pb / P) < px) {
            IL <- a1
        } else if ((Pa / P) > px) {
            IL <- a3
        } else {
            IL <- a2
        }
    } else {
        IL <- ((2 * sqrt(px)) / (px + 1)) - 1
    }

    return(list(r = range_factor, IL = IL, expected_price = P1))
}

# ranges_list <- list(
#     c(999, 1001),
#     c(990, 1010),
#     c(909, 1100),
#     c(833.33, 1200),
#     c(500, 2000),
#     c(200, 5000)
# )

# for (i in 1:length(ranges_list)) {
#     range <- ranges_list[[i]]
#     il <- calc_abs_il(1000, 1024.98, range[1], range[2])
#     print(il$IL)
# }

# il <- calc_abs_il(1000, 1012.68, 999, 1001)
# il

get_expected_prices <- function(P0, mu) {

    static_data <- matrix(ncol = 3, nrow = 2)

    static_data[1, 1] <- mu
    static_data[1, 2] <- (P0 * exp(mu * 168))
    static_data[1, 3] <- (P0 * exp(mu * 720))
    static_data[2, 1] <- 2 * mu
    static_data[2, 2] <- (P0 * exp(2 * mu * 168))
    static_data[2, 3] <- (P0 * exp(2 * mu * 720))

    return(list(data = static_data))
}

# Adapted from https://github.com/Victorletzelter/brownian_motion
# Define a function to calculate the price at time t
price_at_t <- function(n_paths, P0, Pa, Pb, mu, sigma, t) {

    T <- t
    Bo = 0
    t = seq(0, T, by = 0.1)
    gbms = matrix(0, nrow = n_paths, ncol = length(t))

    for (i in 1:n_paths) {

        # Simulation of increments
        B.acc = rnorm(t)

        # Simulation of a trajectory > we have an MB B.sim
        B.sim = Bo + cumsum(B.acc)

        # Simulation of the price
        gbm = P0 * exp((mu - sigma^2 / 2) * t + sigma * B.sim)

        # expectation vs prediction:
        pred = P0 * exp(mu * t)

        gbms[i, ] = gbm
    }

    return(list(gbms = gbms, pred = pred, IL_v = c()))
}

# Random number Box–Muller transform
random_bm <- function(mu, sigma) {
    u <- runif(1, min=0, max=1)
    v <- runif(1, min=0, max=1)
    mag <- sigma * sqrt(-2 * log(u))
    return (mag * cos(2 * pi * v) + mu)
}
