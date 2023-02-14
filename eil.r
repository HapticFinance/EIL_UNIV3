#!/usr/bin/env Rscript
path = getwd()
source(paste(path, "/config.r", sep=""))
source(paste(path, "/install.r", sep=""))
source(paste(path, "/static.r", sep=""))

psi_below_pa <- function(x, t, P, Pa, Pb, mu, sigma) {

    first_term_num <- (exp(x) * (sqrt(P/Pa) - 1) - 1 + sqrt(Pa/P))
    first_term_den <- (exp(x) * (1 - sqrt(P/Pb)) + 1 - sqrt(Pa/P))
    second_term <- (x - (mu - (sigma^2/2)) * t)^2 / (2 * t * (sigma^2))
    
    exponentiated <- exp(-second_term)
    res <- (first_term_num / first_term_den) * exponentiated

    return(res) 
}

psi_in_range <- function(x, t, P, Pa, Pb, mu, sigma) {

    first_term_num <- (2 * sqrt(exp(x)) - 1 - exp(x))
    first_term_den <- ((1 + exp(x) - sqrt(Pa/P) - exp(x) * sqrt(P/Pb)))
    second_term <- (x - (mu - (sigma^2/2)) * t)^2 / (2 * t * (sigma^2))

    exponentiated <- exp(-second_term)
    res <- (first_term_num / first_term_den) * exponentiated

    return(res) 
}

psi_above_pb <- function(x, t, P, Pa, Pb, mu, sigma) {

    first_term_num <- (sqrt(Pb/P) - (exp(x) * (1 - sqrt(P/Pb))) -1)
    first_term_den <- (exp(x) * (1 - sqrt(P/Pb)) + 1 - sqrt(Pa/P))
    second_term <- (x - (mu - (sigma^2/2)) * t)^2 / (2 * t * (sigma^2))
    exponentiated <- exp(-second_term)
    res <- (first_term_num / first_term_den) * exponentiated

    return(res) 
}

calc_eil_integrals <- function(f, P, Pa, Pb, mu, sigma, t) {

    exec <- f(0, 0, 0, 0, 0, 0, 0)
    func_name <- match.call()[2]

    if (func_name == "psi_below_pa()" ) {
        lower_bound <- -Inf 
        upper_bound <- log(Pa / P)
    } else if (func_name == "psi_in_range()" ) {
        lower_bound <- log(Pa / P)
        upper_bound <- log(Pb / P)
    } else if (func_name == "psi_above_pb()" ) {
        lower_bound <- log(Pb / P)
        upper_bound <- Inf
    }

    integral <- integrate(
        f,
        # Lower integration bound
        lower_bound, 
        # Upper integration bound
        upper_bound, 
        subdivisions = 5000,
        abs.tol = 1e-5, 
        rel.tol = 1e-7,  
        stop.on.error = F,
        t = t, 
        P = P, 
        Pa = Pa,    
        Pb = Pb,
        mu = mu,
        sigma = sigma
    )$value;

    coefficient <- (1 / (sigma * sqrt(2 * pi * t)))
    return(coefficient * integral)
}


calc_time_ITM_integrals <- function(P, Pa, Pb, mu, sigma, T) {

    lower_bound <- log(Pa / P)
    upper_bound <- log(Pb / P)

    f <- function(x, y, mu, sigma) {
        t <- y
        first_term <- -(x - (mu - (sigma^2 / 2)) * t)^2
        second_term <- (2 * t * (sigma^2))
        exp(first_term / second_term) / sqrt(t)
    }

    integral <- dblquad(
        f, 
        log(Pa/P), 
        log(Pb/P), 
        0, 
        T,
        tol=1e-5,
        subdivs=5000, 
        mu=mu, 
        sigma=sigma
    )

    result <- (1 / (sigma * (sqrt(2 * pi)))) * integral[1]
    
    return(result)
}


calc_liquidity <- function(V, P, Pa, Pb) {

    res <- V / ((2 * sqrt(P)) - (P / sqrt(Pb)) - sqrt(Pa))

    return(res)
}


calc_eil <- function(P, Pa, Pb, mu, sigma, t) {

    integral_below_pa <- calc_eil_integrals(psi_below_pa, P, Pa, Pb, mu, sigma, t)
    integral_in_range <- calc_eil_integrals(psi_in_range, P, Pa, Pb, mu, sigma, t)
    integral_above_pb <- calc_eil_integrals(psi_above_pb, P, Pa, Pb, mu, sigma, t)
    expected_IL <- integral_below_pa + integral_in_range + integral_above_pb

    return(expected_IL)
}


calc_capital_efficiency_simplified <- function(r) {

    CE <- round(sqrt(r) / (sqrt(r) - 1))

    return(CE)
}

get_simulated_last_price_avg <- function(simulated_prices) {
    
    last_prices <- c()
    avg_last_price <- 0

    for (i in 1:nrow(simulated_prices)) {
        last_prices <- c(
            last_prices, 
            simulated_prices[i, ncol(simulated_prices)]
        )
    }
    
    avg_last_price <- mean(last_prices)    

    return(avg_last_price)
}

compute_row_data <- function(
    V, 
    P, 
    Pa, 
    Pb, 
    mu, 
    sigma, 
    t, 
    results
) {

    simulated_prices <- results$gbms
    avg_last_price <- get_simulated_last_price_avg(simulated_prices)
    datum <- avg_last_price
    
    time_ITM <- calc_time_ITM_integrals(P, Pa, Pb, mu, sigma, t)
    EIL <- calc_eil(P, Pa, Pb, mu, sigma, t)
    data <- calc_abs_il(P, datum, Pa, Pb)
    IL <- data$IL

    return(list(EIL, time_ITM, IL)) 
}

calc_chunk <- function(ranges_list, mu, sigma, t) {

    mat_res <- matrix(ncol = length(ranges), nrow = length(ranges)) 
    P <- S0

    ranges <- ranges_list
    res <- price_at_t_heston(n_sim, 1000, mu, sigma, t, v0, kappa, theta, sigma_v, rho)

    for (i in 1:length(ranges_list)) {

        active_range <- ranges[[i]]
        
        Pa <- active_range[1]
        Pb <- active_range[2]

        range_factor <- sqrt(Pb / Pa)
        capital_efficiency <- sqrt(range_factor) / (sqrt(range_factor) - 1)
        results <- compute_row_data(V, P, Pa, Pb, mu, sigma, t, res)

        EIL <- results[[1]]
        time_ITM <- results[[2]]
        IL <- results[[3]]

        mat_res[i, 1] <- formatC(mu, format = "e", digits = 2)
        mat_res[i, 2] <- formatC(range_factor, digits = 3, format = "f")
        mat_res[i, 3] <- formatC(capital_efficiency, digits = 0, format = "f")
        mat_res[i, 4] <- formatC(time_ITM, digits = 2, format = "f")
        mat_res[i, 5] <- ifelse((-1 * EIL) < EIL_THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL, format = "f", digits = 4))
        mat_res[i, 6] <- ifelse((-1 * IL) < EIL_THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(IL, format = "f", digits = 4))
   
    }

    return(mat_res)
}

run_calc <- function(mu, sigma, t) {

    tryCatch({
        
        first_chunk <- calc_chunk(ranges, mu, sigma, t)
        second_chunk <- calc_chunk(ranges, mu, sigma * 2, t)

        combined_by_col_1st_chunk <- cbind(first_chunk, second_chunk[, 4:ncol(second_chunk)])

        third_chunk <- calc_chunk(ranges, mu * 2, sigma, t)
        fourth_chunk <- calc_chunk(ranges, mu * 2, sigma * 2, t)

        combined_by_col_2nd_chunk <- cbind(third_chunk, fourth_chunk[, 4:ncol(fourth_chunk)])
        combined <- rbind(combined_by_col_1st_chunk, combined_by_col_2nd_chunk)

        return(combined)
    }, error = function(e) {

        if (length(e) > 1) {
            print(e)
            return(NA)
        }

    })
}
 
# run_calc(0.000075, 0.001, 168)