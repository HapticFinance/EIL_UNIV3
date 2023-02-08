#!/usr/bin/env Rscript
path = getwd()
source(paste(path, "/install.r", sep=""))
source(paste(path, "/gbm.r", sep=""))
source(paste(path, "/eil.r", sep=""))
source(paste(path, "/univ3.r", sep=""))

# set.seed(33.358) 
range_factor <- NA
x <- NA
y <- NA

# Vector of IL values
IL_v <- c()
# Vector of price changes
priceChanges <- c()
# Create a vector of portfolio values
portfolio <- c()
IV_vector <- c()
price_vector <- c()
daily_premium <- 0.3
cum_premium <- 0
loss_vs_rebalancing <- 0
lvr_vector <- c()
real_volume_vector <- c()
volatilities_vector <- c()
daily_volatility_vector <- c()
intervals_in_range <- 0

loan <- 4000 * 0.25

run_sim <- function(p, a, b, x, y, t, mu, sigma) {
    
    n_paths <- 5
    S <- gbm_loop(n_paths, t, mu, sigma, p)

    liquidated <- FALSE
    sp <- sqrt(p)
    sa <- sqrt(a)
    sb <- sqrt(b)

    range_factor <- sqrt(b / a)

    # calculate the initial liquidity
    L <- get_liquidity(x, y, sp, sa, sb)
    
    IV <- 0
    price <- 0

    IV_vector <- c(n)
    price_vector <- c(n)
    uni_v3_fees_vector <- c(n)

    V <- (x * p) + y
    phi <- 0.003 # 0.3% fee

    for (r in 1:nrow(S)) {
        for (x in 1:ncol(S)) {
            daily_volatility <- var(S[1:r, x])

            if (r == 1) {
                P1 <- p
                x1 <- x
                y1 <- y
            } else {
                P1 <- S[r, x]
                x1 <- calculate_x(L, sp1, sa, sb)
                y1 <- calculate_y(L, sp1, sa, sb)
            }

            sp1 <- sqrt(P1)
            one_month <- 30/365
            cum_premium <- cum_premium + daily_premium

            # Impermanent loss calculation
            # Value of initial position at the end of the simulation
            V0 = (x * p) + (y * 1)  

            # Actual value of position at the end of the simulation
            V1 = (x1 * S[r, x]) + (y1 * 1) 

            # if (V1 < loan * 1.5) {
            #     liquidated <- TRUE
            #     # break
            # }

            # IL as percentage loss
            if ((P1 >= a && P1 <= b)) {
                IL = V1 - V0
                intervals_in_range <- intervals_in_range + 1
            } else if (P1 < a || P1 > b) {
                IL = V0 - ((x * P1) + y)
            # special case
            } else if (P1 == a && P1 == b  ) {
                IL = 0
                intervals_in_range <- intervals_in_range + 1
            }

            if (IL < 0) {
                IL = IL * -1
            }

            IL = (IL / V0) * 100

            # Loss-vs-rebalancing calculation
            daily_volatility <- sqrt(daily_volatility) / 100
            loss_vs_rebalancing <- (daily_volatility^2 / 8)
            lvr_vector <- c(lvr_vector, loss_vs_rebalancing)

            real_volume_vector <- c(real_volume_vector, loss_vs_rebalancing / 30)
            daily_volatility_vector <- c(daily_volatility_vector, daily_volatility)
    

            IL_v <- c(IL_v, IL)   

        }
    
   }

    alpha <- calc_alpha(mu, sigma, intervals_in_range)
    # fees <- calc_expected_total_fees_x(V, p, a, b, mu, 0.003, intervals_in_range, alpha) #/ n_paths

    return(
        list(
            IL_v, 
            priceChanges, 
            IV_vector, 
            price_vector, 
            0, #fees, 
            cum_premium, 
            lvr_vector, 
            daily_volatility_vector, 
            real_volume_vector, 
            S,
            intervals_in_range / n_paths / 10
        )
    )
}

# run_sim(1000, 833.33, 1200, 2, 2000, 1680, 0.000075, 0.001)

# p = 2000
# a = 1800
# b = 1860
# x = 2
# y = 4000

# in_range <- (a <= p && b >= p)

# if (a > p && !in_range) {
#   # single asset liquidity
#   y = 0
# } else if (a < p && !in_range) {
#   # single asset liquidity
#   x = 0
# } else if (in_range) {
#   # dual asset liquidity
#   sp = p ** 0.5
#   sa = a ** 0.5
#   sb = b ** 0.5
#   L = get_liquidity_0(x, sp, sb)
#   y = calculate_y(L, sp, sa, sb)
# } 
