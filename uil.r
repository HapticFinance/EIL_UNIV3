#!/usr/bin/env Rscript
path = getwd()
source(paste(path, "/install.r", sep=""))
source(paste(path, "/config.r", sep=""))
source(paste(path, "/static.r", sep=""))
# set.seed(1)

compute_UIL <- function(P0, Pa, Pb, Sa, Sb, sigma, t) {

    Db <- (log(P0 / Pb) - (sigma^2 * t / 2)) / (sigma * sqrt(t))
    Da <- (log(P0 / Pa) - (sigma^2 * t / 2)) / (sigma * sqrt(t))

    Qb <- (log(P0 / Sb) - (sigma^2 * t / 2)) / (sigma * sqrt(t))
    Qa <- (log(P0 / Sa) - (sigma^2 * t / 2)) / (sigma * sqrt(t))
    
    UIL_R <- 2 * sqrt(P0) * exp(-sigma^2 * t / 8) * (pnorm(Da + sigma * sqrt(t) / 2) - pnorm(Db + sigma * sqrt(t) / 2)) - sqrt(Pa) * pnorm(Da) + sqrt(Pb) * pnorm(Db) - (1 / sqrt(Pa)) * P0 * pnorm(Da + sigma * sqrt(t)) + (1 / sqrt(Pb)) * P0 * pnorm(Db + sigma * sqrt(t))
    UIL_L <- 2 * sqrt(P0) * exp(-sigma^2 * t / 8) * (-pnorm(-Qa - sigma * sqrt(t) / 2) + pnorm(-Qb - sigma * sqrt(t) / 2)) + sqrt(Sa) * pnorm(-Qa) - sqrt(Sb) * pnorm(-Qb) + (1 / sqrt(Sa)) * P0 * pnorm(-Qa - sigma * sqrt(t)) - (1 / sqrt(Sb)) * P0 * pnorm(-Qb - sigma * sqrt(t))

    return(list(UIL_R = UIL_R, UIL_L = UIL_L))
}


calc_uil_integrals <- function(P0, Pa, Pb, sigma, t, flag) {
    
    psi <- function(x, T, sigma, flag) {
        option_price <- bs_option(P0, x, sigma, 0, T, flag) 
        return(x^(-(3/2)) * option_price)
    }

    integral <- integrate(
        psi,
        # Lower integration bound
        lower = Pa, 
        # Upper integration bound
        upper = Pb, 
        subdivisions = 5000,
        abs.tol = 1e-5, 
        rel.tol = 1e-7,  
        stop.on.error = F,
        T = t ,
        sigma = sigma,
        flag = flag
    )$value;

    coefficient <- (-0.5)
    return(coefficient * integral)
}

compute_row_data_uil <- function(
    P0, 
    Pa, 
    Pb, 
    Sa,
    Sb, 
    sigma, 
    t
) {

    UIL <- compute_UIL(P0, Pa, Pb, Sa, Sb, sigma, t)
    UIL_R <- UIL$UIL_R
    UIL_L <- UIL$UIL_L

    uil_r <- calc_uil_integrals(
        P0,
        Pa,
        Pb,
        sigma,
        t,
        "Call"
    )

    uil_l <- calc_uil_integrals(
        P0,
        Sa,
        Sb,
        sigma,
        t,
        "Put"
    )

    results = matrix(nrow = 3, ncol= 2)

    results[1, 1] = uil_r
    results[1, 2] = uil_l
    results[2, 1] = UIL_R
    results[2, 2] = UIL_L
    results[3, 1] = ifelse((-1 * uil_r) > 0, glue::glue("[BUY PUT - K@{Pa}]"), NA)
    results[3, 2] = ifelse((-1 * uil_l) > 0, glue::glue("[BUY CALL - K@{Pb}]"), NA)

    return(results)
}

calc_chunk_uil <- function(ranges_list, sigma, t, prices) {

    results_mat <- matrix(nrow = length(ranges_list), ncol = 5) 

    for (i in 1:length(ranges_list)) {

        active_range <- ranges_list[[i]]
        
        Pa <- active_range[1]
        Pb <- active_range[2]

        Sa <- Pa * 1.25
        Sb <- Pb * 1.25

        range_factor <- sqrt(Pb / Pa)
        capital_efficiency <- sqrt(range_factor) / (sqrt(range_factor) - 1)
        results <- compute_row_data_uil(S0, Pa, Pb, Sa, Sb, sigma, t)

        prices <- prices$gbms
        uil_r <- results[1, 1]
        uil_l <- results[1, 2]
        UIL_R <- results[2, 1]
        UIL_L <- results[2, 2]
        hedge_r <- results[3, 1]
        hedge_l <- results[3, 2]

        # print(glue::glue("last price {prices[length(prices)]}"))

        IL_abs_left <- calc_abs_il(S0, prices[nrow(prices), ncol(prices)], Pa, Pb)$IL
        IL_abs_right <- calc_abs_il(S0, prices[nrow(prices), ncol(prices)], Pa, Pb)$IL
        
        results_mat[i, 1] <- formatC(sigma, format = "f", digits = 3)
        results_mat[i, 2] <- formatC(uil_r, format = "f", digits = 5)
        results_mat[i, 3] <- formatC(uil_l, format = "f", digits = 5)
        results_mat[i, 4] <- formatC(UIL_R, format = "f", digits = 5)
        results_mat[i, 5] <- formatC(UIL_L, format = "f", digits = 5)
        # results_mat[i, 5] <- formatC(IL_abs_left, format = "f", digits = 5)
        # results_mat[i, 6] <- formatC(IL_abs_right, format = "f", digits = 5)

        # results_mat[i, 5] <- hedge_r
        # results_mat[i, 6] <- hedge_l

    }

    return(results_mat)
}

run_uil_calc <- function(ranges, sigma, t) {

    prices <- price_at_t_heston(n_paths = n_sim, P0 = 1000, mu = 0.1, sigma = 0.2, T = 1, v0 = 0.04, kappa = 1.5, theta = 0.04, sigma_v = 0.3, rho = -0.5)

    # tail(prices$gbms[, ncol(prices$gbms)])

    ranges <- ranges[3]
    first_chunk <- calc_chunk_uil(ranges,  sigma, t, prices)
    second_chunk <- calc_chunk_uil(ranges, sigma * 5, t, prices)

    res <- rbind(first_chunk, second_chunk)

    df <- data.frame(res)
    colnames(df) <- c("sigma", "$UIL^{R}$", "$UIL^{L}$", "$UIL^{R}$", "$UIL^{L}$")

    return(df)
}

# run_uil_calc(
#     ranges,
#     sigma, 
#     T
# )
 