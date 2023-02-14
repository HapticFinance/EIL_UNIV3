#!/usr/bin/env Rscript
path = getwd()
source(paste(path, "/install.r", sep=""))
set.seed(2)

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
price_at_t <- function(n_paths, P0, Pa, Pb, mu, sigma, T) {

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

# Define a function to simulate stock prices using the Heston model
price_at_t_heston <- function(n_paths, P0, mu, sigma, T, v0, kappa, theta, sigma_v, rho) {

  # Set up the time grid
  dt <- 0.1
  n_steps <- round(T / dt)
  t <- seq(0, T, by = dt)

  # Simulate the Brownian motion components
  dW1 <- matrix(rnorm(n_paths * n_steps), nrow = n_paths, ncol = n_steps)
  dW2 <- matrix(rnorm(n_paths * n_steps), nrow = n_paths, ncol = n_steps)
  W1 <- apply(dW1, 1, cumsum)
  W2 <- apply(dW2, 1, cumsum)

  # Simulate the stock price paths
  stock_price_simulations <- matrix(0, nrow = n_paths, ncol = n_steps + 1)
  stock_price_simulations[, 1] <- P0

  for (i in 1:n_paths) {
    for (j in 2:(n_steps + 1)) {
      v <- max(0, v0 + kappa * (theta - v0) * dt + sigma_v * sqrt(v0 * dt) * dW1[i, j - 1])
      stock_price_simulations[i, j] <- stock_price_simulations[i, j - 1] * exp((mu - v / 2) * dt + sqrt(v * dt) * (rho * dW1[i, j - 1] + sqrt(1 - rho^2) * dW2[i, j - 1]))
    }
  }

  return(list(gbms = stock_price_simulations, pred = P0 * exp(mu * t), IL_v = c()))
}

# Define a function to simulate stock prices using the Heston model with a drift term for the volatility process
price_at_t_heston_drift <- function(n_paths, P0, mu, sigma, T, v0, kappa, theta, sigma_v, rho, xi) {

  # Set up the time grid
  dt <- 0.1
  n_steps <- round(T / dt)
  t <- seq(0, T, by = dt)

  # Simulate the Brownian motion components
  dW1 <- matrix(rnorm(n_paths * n_steps), nrow = n_paths, ncol = n_steps)
  dW2 <- matrix(rnorm(n_paths * n_steps), nrow = n_paths, ncol = n_steps)
  dZ <- matrix(rnorm(n_paths * n_steps), nrow = n_paths, ncol = n_steps)
  W1 <- apply(dW1, 1, cumsum)
  W2 <- apply(dW2, 1, cumsum)

  # Simulate the stock price paths
  stock_price_simulations <- matrix(0, nrow = n_paths, ncol = n_steps + 1)
  stock_price_simulations[, 1] <- P0

  # Simulate the volatility paths
  volatility_simulations <- matrix(0, nrow = n_paths, ncol = n_steps + 1)
  volatility_simulations[, 1] <- v0

  for (i in 1:n_paths) {
    for (j in 2:(n_steps + 1)) {
      v <- volatility_simulations[i, j - 1]
      dZv <- xi * (theta - v) * dt + sigma_v * sqrt(v * dt) * dZ[i, j - 1]
      v <- max(0, v + kappa * (theta - v) * dt + sigma_v * sqrt(v * dt) * dW1[i, j - 1] + dZv)
      volatility_simulations[i, j] <- v
      stock_price_simulations[i, j] <- stock_price_simulations[i, j - 1] * exp((mu - v / 2) * dt + sqrt(v * dt) * (rho * dW1[i, j - 1] + sqrt(1 - rho^2) * dW2[i, j - 1]))
    }
  }

  return(list(gbms = stock_price_simulations, pred = P0 * exp(mu * t), IL_v = volatility_simulations))
}

# Random number Box–Muller transform
random_bm <- function(mu, sigma) {
    u <- runif(1, min=0, max=1)
    v <- runif(1, min=0, max=1)
    mag <- sigma * sqrt(-2 * log(u))
    return (mag * cos(2 * pi * v) + mu)
}
