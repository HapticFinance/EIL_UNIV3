#!/usr/bin/env Rscript
path = getwd()
source(paste(path, "/install.r", sep=""))
source(paste(path, "/gbm.r", sep=""))
source(paste(path, "/eil.r", sep=""))
source(paste(path, "/univ3.r", sep=""))

range_factor <- NA
x <- NA
y <- NA

# Vector of IL values
IL_v <- c()

loss_vs_rebalancing <- 0
intervals_in_range <- 0

lvr_vector <- c()
real_volume_vector <- c()
daily_volatility_vector <- c()


run_sim <- function(P, Pa, Pb, x, y, t, mu, sigma) {
    
    phi <- 0.003 # 0.3% fee
    dt <- 0.1
    n <- t/dt
    n_paths <- 10
    S <- gbm_loop(n_paths, t, mu, sigma, P)

    liquidated <- FALSE
    sp <- sqrt(P)
    sa <- sqrt(Pa)
    sb <- sqrt(Pb)

    range_factor <- sqrt(Pb / Pa)

    # calculate the initial liquidity
    L <- get_liquidity(x, y, sp, sa, sb)
    
    V <- (x * P) + y

    for (r in 1:nrow(S)) {
        for (x in 1:ncol(S)) {
            daily_volatility <- var(S[1:r, x])

            if (r == 1) {
                P1 <- P
                x1 <- x
                y1 <- y
            } else {
                P1 <- S[r, x]
                x1 <- calculate_x(L, sp1, sa, sb)
                y1 <- calculate_y(L, sp1, sa, sb)
            }

            sp1 <- sqrt(P1)

            # Impermanent loss calculation
            # Value of initial position at the end of the simulation
            V0 = (x * P) + (y * 1)  

            # Actual value of position at the end of the simulation
            V1 = (x1 * S[r, x]) + (y1 * 1) 

            # IL as percentage loss
            if ((P1 >= Pa && P1 <= Pb)) {
                IL = V1 - V0
                intervals_in_range <- intervals_in_range + 1
            } else if (P1 < Pa || P1 > Pb) {
                IL = V0 - ((x * P1) + y)
            # special case
            } else if (P1 == Pa && P1 == Pb  ) {
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

    alpha <- 0 #calc_alpha(mu, sigma, intervals_in_range)
    # fees <- calc_expected_total_fees_x(V, P, Pa, Pb, mu, 0.003, intervals_in_range, alpha) #/ n_paths

    return(
        list(
            IL_v, 
            lvr_vector, 
            daily_volatility_vector, 
            real_volume_vector, 
            S,
            intervals_in_range / n_paths / 10
        )
    )
}

# run_sim(1000, 833.33, 1200, 2, 2000, 168, 0.000075, 0.001)

