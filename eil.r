#!/usr/bin/env Rscript
path = getwd()
source(paste(path, "/install.r", sep=""))

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
        lower_bound, # Lower integration bound
        upper_bound, # Upper integration bound
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
        exp(-(x - (mu - (sigma^2/2)) * t)^2 / (2 * t * (sigma^2))) / sqrt(t)
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

calc_eil <- function(P, Pa, Pb, mu, sigma, t) {

    integral_below_pa <- calc_eil_integrals(psi_below_pa, P, Pa, Pb, mu, sigma, t)
    integral_in_range <- calc_eil_integrals(psi_in_range, P, Pa, Pb, mu, sigma, t)
    integral_above_pb <- calc_eil_integrals(psi_above_pb, P, Pa, Pb, mu, sigma, t)
    expected_IL <- integral_below_pa + integral_in_range + integral_above_pb

    return(expected_IL)
}

calc_expected_total_fees_x <- function(V, P, Pa, Pb, mu, phi, t, dt, alpha) {

    CE <- calc_capital_efficiency_simplified(sqrt(Pb / Pa))
    fees_x <- phi / (1 - phi) * ((V * CE) / (4 * P)) * ((1 - exp((-mu * t) / 2)) / (1 - exp((-mu * dt) / 2))) * alpha

    return(fees_x)
}

calc_expected_total_fees_y <- function(V, P, Pa, Pb, mu, phi, t, dt, beta) {

    CE <- calc_capital_efficiency_simplified(sqrt(Pb / Pa))
    fees_x <- phi / (1 - phi) * ((V * CE) / 4) * ((1 - exp((mu * t) / (2))) / (1 - exp((mu * dt) / 2))) * beta

    return(fees_x)
}

calc_alpha <- function(mu, sigma, dt) {

    first_term <- exp((1 / 8) * (3 * sigma^2 - 4 * mu) * dt)
    second_term <- (erf(((sigma^2 - mu) * sqrt(dt)) / sqrt(2) * sigma) + 1) 
    third_term <- -(1 / 2) * erfc(((mu - (sigma^2 / 2)) * sqrt(dt)) / (sqrt(2) * sigma))
    res <- first_term * second_term - third_term

    return(res)
}

calc_beta <- function(mu, sigma, dt) {
    
    first_term <- exp((1 / 8) * (4 * mu - sigma^2) * dt)
    second_term <- (erf((mu * sqrt(dt)) / sqrt(2) * sigma) + 1) 
    third_term <- -erf(((mu - (sigma^2 / 2)) * sqrt(dt)) / (sqrt(2) * sigma)) - 1
    res <- first_term * second_term - third_term

    return(res)
}

calc_capital_efficiency_simplified <- function(r) {

    CE <- round(sqrt(r) / (sqrt(r) - 1))

    return(CE)
}


compute_row_data <- function(V, P, Pa, Pb, mu, sigma, t) {

    EIL <- calc_eil(P, Pa, Pb, mu, sigma, t)
    time_ITM <- calc_time_ITM_integrals(P, Pa, Pb, mu, sigma, t)
    alpha <- calc_alpha(mu, sigma, time_ITM)
    fees_x <- calc_expected_total_fees_x(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, alpha)
    beta <- calc_beta(mu, sigma, time_ITM)
    fees_y <- calc_expected_total_fees_y(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, beta)

    return(list(EIL, time_ITM, fees_x,  fees_y))
}

calc_chunk <- function(ranges_list, mu, t) {

        mat_res <- matrix(ncol = 10, nrow = 6) 
        
        EIL_THRESHOLD <- 0.0001

        V <- 5000
        P <- 1000

        ranges <- ranges_list

        for (i in 1:length(ranges_list)) {

            active_range <- ranges[[i]]

            Pa <- active_range[1]
            Pb <- active_range[2]
            range_factor <- sqrt(Pb / Pa)
            
            sigma <- 0.001
            results <- compute_row_data(V, P, Pa, Pb, mu, sigma, t)

            EIL <- results[[1]]
            time_ITM <- results[[2]]
            fees_x <- results[[3]]
            fees_y <- results[[4]]

            mat_res[i, 1] <- formatC(mu, format = "e", digits = 2)
            mat_res[i, 2] <- formatC(range_factor, digits = 3, format = "f")
            mat_res[i, 3] <- formatC(time_ITM, digits = 2, format = "f")
            mat_res[i, 4] <- ifelse((-1 * EIL) < EIL_THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL, format = "f", digits = 5))
            mat_res[i, 5] <- formatC(fees_x, digits = 4, format = "f")
            mat_res[i, 6] <- format(fees_y, nsmall = 4, format = "f")

            sigma <- sigma * 2
            results <- compute_row_data(V, P, Pa, Pb, mu, sigma, t)
            
            EIL <- results[[1]]
            time_ITM <- results[[2]]
            fees_x <- results[[3]]
            fees_y <- results[[4]]

            mat_res[i, 7] <- formatC(time_ITM, digits = 2, format = "f")
            mat_res[i, 8] <- ifelse((-1 * EIL) < EIL_THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL , format = "f", digits = 5))
            mat_res[i, 9] <- formatC(fees_x, digits = 4, format = "f")
            mat_res[i, 10] <-format(fees_y, nsmall = 4, format = "f")

        }

        return(mat_res)
}

run_calc <- function(mu, t) {

    ranges_list <- list(
        c(999, 1001),
        c(990, 1010),
        c(909, 1100),
        c(833.33, 1200),
        c(500, 2000),
        c(200, 5000)
    )

    tryCatch({
        
        first_chunk <- calc_chunk(ranges_list, mu, t)
        second_chunk <- calc_chunk(ranges_list, mu * 2, t)

        combined <- rbind(first_chunk, second_chunk)

        prmatrix(combined)

        return(combined)
    }, error = function(e) {

        if (length(e) > 1) {
            print(e)
            return(NA)
        }

    })
}
 