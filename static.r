#!/usr/bin/env Rscript
path = getwd()
source(paste(path, "/install.r", sep=""))
source(paste(path, "/gbm.r", sep=""))
set.seed(8)

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

ranges_list <- list(
    c(999, 1001),
    c(990, 1010),
    c(909, 1100),
    c(833.33, 1200),
    c(500, 2000),
    c(200, 5000)
)

for (i in 1:length(ranges_list)) {
    range <- ranges_list[[i]]
    il <- calc_abs_il(1000, 1055.485, range[1], range[2])
    print(il$IL)
}

il <- calc_abs_il(1000, 1014.93, 999, 1001)
il

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


# mu <- 0.000075
# sigma <- 0.001
# T <- 168

# P0 <- 1000
# expected_price <- (P0 * exp(mu * T))

# P1 <- expected_price
# Pa <- 999 
# Pb <- 1001

# n_paths <- 1

# tic("compute GBM")


# Define a function to calculate the price at time t
price_at_t <- function(n_paths, P0, Pa, Pb, mu, sigma, t) {

    # T <- t
    # dt <- 0.01
    # steps <- T/dt
    # gbm <- matrix(ncol = steps, nrow = n_paths)
    # IL_v <- c()

    # #gbm <- somebm::bm(x0 = P0, mu = mu, sigma = sigma, t0 = 0, t = 1, n = t * 100)
    # gbm <- GBM_simulate(10,  1/1000 , mu * 100 , sigma,  P0,  0.00001)

    # # for (simu in 1:n_paths) {
    # #     gbm[simu, 1] <- P0
    # #     for (i in 2:steps) {
    # #         epsilon <- rnorm(1)
    # #         gbm[simu, i] <- gbm[simu, (i - 1)] * exp((mu - sigma^2 / 2) * dt + sigma * epsilon * sqrt(dt))
    # #         #data <- calc_abs_il(gbm[simu, i] , Pa, Pb)
    # #         #IL_v <- c(IL_v, data$IL)
    # #     }
    # # }

    #set.seed(1)
    # Simulation d’un mvt Brownien ### discretisation du temps
    T <- t
    Bo = 0
    t = seq(0, T, by = 0.1)

    # Simulation of increments
    B.acc = rnorm(t)

    # Simulation of a trajectory > we have an MB B.sim
    B.sim = Bo + cumsum(B.acc)

    # MB simulated, we can make several like this
    gbm = 1000 * exp((mu - sigma^2 / 2) * t + sigma * B.sim)

    # expectation vs prediction:
    pred = 1000 * exp(mu * t)

    # plot(gbm, type="l",xlab=glue::glue("Expectation {pred[length(pred)]} sigma {sigma} mu {mu}"), ylab="Ordinate axis")
    # lines(pred, col="red")
    
    return(list(gbm = gbm, pred = pred, IL_v = c()))
}

# price_at_t(1, 1000, 999, 1001, 0.000075, 0.001, 168)

# Generate n_paths simulations of the price at time T
# results <- price_at_t(n_paths, P0, mu, sigma, T)
# IL_v <- results$IL_v
# results <- results$gbm

# # Show the results
# # mean(results[nrow(results),])

# avgs <- c()

# for (i in 1:nrow(results)) {
#     # print(mean(results[i, 1:ncol(results)]))
#     avg <- mean(results[i, 1:ncol(results)])
#     avgs <- c(avgs, avg)
# }

# # print(mean(avgs))

# #last_price <- mean(results[nrow(results),])
# #last_price <- mean(avgs)

# # Last generated price
# last_generated_price <- results[nrow(results), ncol(results)]
# last_generated_price

# # Minimum generated price of all paths
# min_price <-  min(results, na.rm = TRUE)
# # Maximum generated price of all paths
# max_price <-  max(results, na.rm = TRUE)

# average_max_min <- mean(c(max_price, max_price))
# average_max_min

# data <- calc_abs_il(1114.048, Pa, Pb)
# # data

# IL <- data$IL
# IL

# mean(IL_v)
# toc()

# last_price <- mean(results[nrow(results),])
# last_price


# data <- calc_abs_il(last_price, Pa, Pb)
# data

# IL <- data$IL
# expected_price <- data$expected_price

# static_data <- matrix(ncol = 3, nrow = 2)

# static_data[1, 1] <- 0.000075
# static_data[1, 2] <- (P0 * exp(0.000075 * 168))
# static_data[1, 3] <- (P0 * exp(0.000075 * 720))
# static_data[2, 1] <- 0.00015
# static_data[2, 2] <- (P0 * exp(0.00015 * 168))
# static_data[2, 3] <- (P0 * exp(0.00015 * 720))

# print(static_data)
# print(glue::glue("IL is {IL} at expected price {expected_price} 𝝈 = {sigma} 𝝁 = {mu} T = {T}"))
