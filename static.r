#!/usr/bin/env Rscript
path = getwd()
source(paste(path, "/install.r", sep=""))
# set.seed(2)

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

bs_option <- function(S0, K, sigma, r, T, flag) {
  
  # Calculate the option value using the Black-Scholes formula
  d1 <- (log(S0/K) + (r + sigma^2/2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  if (flag == "Call") {
    option_value <- S0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else if (flag == "Put") {
    option_value <- K * exp(-r * T) * pnorm(-d2) - S0 * pnorm(-d1)
  } else {
    stop("Invalid flag parameter. Must be 'Call' or 'Put'.")
  }
  
  # Return the option value
  return(option_value)
}

# Adapted from https://github.com/Victorletzelter/brownian_motion
# Define a function to calculate the price at time t
price_at_t <- function(n_paths, P0, mu, sigma, T) {

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

# Define a function to simulate stock prices using the Heston model with a drift term for the volatility process
price_at_t_heston <- function(n_paths, P0, mu, sigma, T, v0, kappa, theta, sigma_v, rho) {
  
  dt <- 0.1
  t = seq(0, T, by = dt)
  
  dW1 <- matrix(rnorm(n_paths * length(t)), nrow = n_paths, ncol = length(t))
  dW2 <- matrix(rnorm(n_paths * length(t)), nrow = n_paths, ncol = length(t))
  dZ <- matrix(rnorm(n_paths * length(t)), nrow = n_paths, ncol = length(t))
  
  W1 <- apply(dW1, 1, cumsum)
  W2 <- apply(dW2, 1, cumsum)
  
  # Simulate the stock price paths
  stock_price_simulations <- matrix(0, nrow = n_paths, ncol = length(t) + 1)
  stock_price_simulations[, 1] <- P0
  
  # Simulate the volatility paths
  volatility_simulations <- matrix(0, nrow = n_paths, ncol = length(t) + 1)
  volatility_simulations[, 1] <- v0
  
  pred <- numeric(length(t))
  
  for (i in 1:n_paths) {
    for (j in 2:(length(t) + 1)) {
      v <- volatility_simulations[i, j - 1]
      dZv <- sigma * (theta - v) * dt + sigma_v * sqrt(v * dt) * dZ[i, j - 1]
      v <- max(0, v + kappa * (theta - v) * dt + sigma_v * sqrt(v * dt) * dW1[i, j - 1] + dZv)
      volatility_simulations[i, j] <- v
      stock_price_simulations[i, j] <- stock_price_simulations[i, j - 1] * exp((mu - v / 2) * dt + sqrt(v * dt) * (rho * dW1[i, j - 1] + sqrt(1 - rho^2) * dW2[i, j - 1]))
      
      # Expected price
      pred[j - 1] <- P0 * exp(mu * t[j - 1])
    }
  }
  
  return(list(gbms = stock_price_simulations, pred = pred ))
}


# result <- price_at_t_heston(n_paths = 10, P0 = 1000, mu = 0.1, sigma = 0.2, T = 1, v0 = 0.04, kappa = 1.5, theta = 0.04, sigma_v = 0.3, rho = -0.5)

# tail(result$gbms[, ncol(result$gbms)])

# Random number Box–Muller transform
random_bm <- function(mu, sigma) {
    u <- runif(1, min=0, max=1)
    v <- runif(1, min=0, max=1)
    mag <- sigma * sqrt(-2 * log(u))
    return (mag * cos(2 * pi * v) + mu)
}
