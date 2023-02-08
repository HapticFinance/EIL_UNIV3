#!/usr/bin/env Rscript
path = getwd()
source(paste(path, "/install.r", sep=""))


psi_below_pa <- function(x, t, P, Pa, Pb, mu, sigma) {

    first_term_num <- (exp(x) * (sqrt(P/Pa) - 1) - 1 + sqrt(Pa/P))
    first_term_den <- (exp(x) * (1 - sqrt(P/Pb)) + 1 - sqrt(Pa/P))
    second_term <- ((x - (mu - ((sigma^2)/2) * t))^2) / (2 * t * (sigma^2))
    exponentiated <- exp(-second_term)
    res <- (first_term_num / first_term_den) * exponentiated

    return(res) 
}

psi_in_range <- function(x, t, P, Pa, Pb, mu, sigma) {

    first_term_num <- (2 * sqrt(exp(x)) - 1 - exp(x))
    first_term_den <- ((1 + exp(x) - sqrt(Pa/P) - exp(x) * sqrt(P/Pb)))
    second_term <- ((x - (mu - ((sigma^2)/2) * t))^2) / (2 * t * (sigma^2))
    exponentiated <- exp(-second_term)
    res <- (first_term_num / first_term_den) * exponentiated

    return(res) 
}

psi_above_pb <- function(x, t, P, Pa, Pb, mu, sigma) {

    first_term_num <- (sqrt(Pb/P) - (exp(x) * (1 - sqrt(P/Pb))) -1)
    first_term_den <- (exp(x) * (1 - sqrt(P/Pb)) + 1 - sqrt(Pa/P))
    second_term <- ((x - (mu - ((sigma^2)/2) * t))^2) / (2 * t * (sigma^2))
    exponentiated <- exp(-second_term)
    res <- (first_term_num / first_term_den) * exponentiated

    return(res) 
}

calc_eil_integrals <- function(f, P, Pa, Pb, mu, sigma, t) {

    exec <- f(0, 0, 0, 0, 0, 0, 0)
    func_name <- match.call()[2]

    if (func_name == "psi_below_pa()") {
        lower_bound <- -Inf 
        upper_bound <- log(Pa / P)
    } else if (func_name == "psi_in_range()") {
        lower_bound <- log(Pa / P)
        upper_bound <- log(Pb / P)
    } else if (func_name == "psi_above_pb()") {
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
        stop.on.error = T,
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


calc_time_ITM_integrals <- function(P, Pa, Pb, mu, sigma, t) {

    lower_bound <- log(Pa / P)
    upper_bound <- log(Pb / P)

    InnerFunc = function(x, mu, sigma, t) { 
        second_term <- ((x - (mu - ((sigma^2) / 2) * t))^2) / (2 * t * (sigma^2))
        exponentiated <- exp(-second_term) 
        res <- exponentiated / sqrt(t)
    }

    InnerIntegral = Vectorize(
        function(y) { 
            integrate(
                InnerFunc, 
                lower_bound, 
                upper_bound,
                subdivisions = 5000,
                abs.tol = 1e-5, 
                rel.tol = 1e-7,  
                stop.on.error = F,
                mu = mu,
                sigma = sigma,
                t = t
            )$value
    })

    res <- integrate(InnerIntegral, 0, t)$value
    coefficient <- (1 / (sigma * sqrt(2 * pi)))

    return(coefficient * res)
}

calc_eil <- function(P, Pa, Pb, mu, sigma, t) {
    print(glue::glue("mu is {mu} and sigma is {sigma} and t is {t}"))
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
    # res <- exp(first_term * second_term - third_term)
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
    # print(glue::glue("CE = {CE}"))
    return(CE)
}


calc_chunk <- function(mu, t) {

        mat_res <- matrix(ncol = 10, nrow = 6) 

        V <- 5000
        P <- 1000
        Pa <- 999
        Pb <- 1001
        range_factor <- sqrt(Pb / Pa)
        THRESHOLD <- 0.0001
        sigma <- 0.0001
 
        EIL <- calc_eil(P, Pa, Pb, mu, sigma, t)
        time_ITM <- calc_time_ITM_integrals(P, Pa, Pb, mu, sigma, t)
        alpha <- calc_alpha(mu, sigma, time_ITM)
        fees_x <- calc_expected_total_fees_x(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, alpha)
        beta <- calc_beta(mu, sigma, time_ITM)
        fees_y <- calc_expected_total_fees_y(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, beta)
 
    
        mat_res[1, 1] <- formatC(mu, format = "e", digits = 2)
        mat_res[1, 2] <- formatC(range_factor, digits = 3, format = "f")
        mat_res[1, 3] <- formatC(time_ITM, digits = 2, format = "f")
        mat_res[1, 4] <- ifelse((-1 * EIL) < THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL, format = "f", digits = 5))
        mat_res[1, 5] <- formatC(fees_x, digits = 4, format = "f")
        mat_res[1, 6] <- format(fees_y, nsmall = 4, format = "f")

        Pa <- 990
        Pb <- 1010
        range_factor <- sqrt(Pb / Pa)

        EIL <- calc_eil(P, Pa, Pb, mu, sigma, t)
        time_ITM <- calc_time_ITM_integrals(P, Pa, Pb, mu, sigma, t)
        alpha <- calc_alpha(mu, sigma, time_ITM)
        fees_x <- calc_expected_total_fees_x(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, alpha)
        beta <- calc_beta(mu, sigma, time_ITM)
        fees_y <- calc_expected_total_fees_y(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, beta)

        mat_res[2, 1] <- formatC(mu, format = "e", digits = 2)
        mat_res[2, 2] <- formatC(range_factor, digits = 3, format = "f")
        mat_res[2, 3] <- formatC(time_ITM, digits = 2, format = "f")
        mat_res[2, 4] <- ifelse((-1 * EIL) < THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL, format = "f", digits = 5))
        mat_res[2, 5] <- formatC(fees_x, digits = 4, format = "f")
        mat_res[2, 6] <- format(fees_y, nsmall = 4, format = "f")

        Pa <- 909
        Pb <- 1100
        range_factor <- sqrt(Pb / Pa)

        EIL <- calc_eil(P, Pa, Pb, mu, sigma, t)
        time_ITM <- calc_time_ITM_integrals(P, Pa, Pb, mu, sigma, t)
        alpha <- calc_alpha(mu, sigma, time_ITM)
        fees_x <- calc_expected_total_fees_x(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, alpha)
        beta <- calc_beta(mu, sigma, time_ITM)
        fees_y <- calc_expected_total_fees_y(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, beta)
        
        mat_res[3, 1] <- formatC(mu, format = "e", digits = 2)
        mat_res[3, 2] <- formatC(range_factor, digits = 3, format = "f")
        mat_res[3, 3] <- formatC(time_ITM, digits = 2, format = "f")
        mat_res[3, 4] <- ifelse((-1 * EIL) < THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL, format = "f", digits = 5))
        mat_res[3, 5] <- formatC(fees_x, digits = 4, format = "f")
        mat_res[3, 6] <- format(fees_y, nsmall = 4, format = "f")


        Pa <- 833.33
        Pb <- 1200
        range_factor <- sqrt(Pb / Pa)

        EIL <- calc_eil(P, Pa, Pb, mu, sigma, t)
        time_ITM <- calc_time_ITM_integrals(P, Pa, Pb, mu, sigma, t)
        alpha <- calc_alpha(mu, sigma, time_ITM)
        fees_x <- calc_expected_total_fees_x(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, alpha)
        beta <- calc_beta(mu, sigma, time_ITM)
        fees_y <- calc_expected_total_fees_y(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, beta)
        
        mat_res[4, 1] <- formatC(mu, format = "e", digits = 2)
        mat_res[4, 2] <- formatC(range_factor, digits = 3, format = "f")
        mat_res[4, 3] <- formatC(time_ITM, digits = 2, format = "f")
        mat_res[4, 4] <- ifelse((-1 * EIL) < THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL, format = "f", digits = 5))
        mat_res[4, 5] <- formatC(fees_x, digits = 4, format = "f")
        mat_res[4, 6] <- format(fees_y, nsmall = 4, format = "f")


        Pa <- 500
        Pb <- 2000
        range_factor <- sqrt(Pb / Pa)

        EIL <- calc_eil(P, Pa, Pb, mu, sigma, t)
        time_ITM <- calc_time_ITM_integrals(P, Pa, Pb, mu, sigma, t)
        alpha <- calc_alpha(mu, sigma, time_ITM)
        fees_x <- calc_expected_total_fees_x(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, alpha)
        beta <- calc_beta(mu, sigma, time_ITM)
        fees_y <- calc_expected_total_fees_y(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, beta)
        
        mat_res[5, 1] <- formatC(mu, format = "e", digits = 2)
        mat_res[5, 2] <- formatC(range_factor, digits = 3, format = "f")
        mat_res[5, 3] <- formatC(time_ITM, digits = 2, format = "f")
        mat_res[5, 4] <- ifelse((-1 * EIL) < THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL, format = "f", digits = 5))
        mat_res[5, 5] <- formatC(fees_x, digits = 4, format = "f")
        mat_res[5, 6] <- format(fees_y, nsmall = 4, format = "f")


        Pa <- 200
        Pb <- 5000
        range_factor <- sqrt(Pb / Pa)

        EIL <- calc_eil(P, Pa, Pb, mu, sigma, t)
        time_ITM <- calc_time_ITM_integrals(P, Pa, Pb, mu, sigma, t)
        alpha <- calc_alpha(mu, sigma, time_ITM)
        fees_x <- calc_expected_total_fees_x(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, alpha)
        beta <- calc_beta(mu, sigma, time_ITM)
        fees_y <- calc_expected_total_fees_y(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, beta)
        
        # print(glue::glue("EIL {prettyNum(EIL,big.mark=',')} time ITM {prettyNum(time_ITM,big.mark=',')} hours"))


        mat_res[6, 1] <- formatC(mu, format = "e", digits = 2)
        mat_res[6, 2] <- formatC(range_factor, digits = 3, format = "f")
        mat_res[6, 3] <- formatC(time_ITM, digits = 2, format = "f")
        mat_res[6, 4] <- ifelse((-1 * EIL) < THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL, format = "f", digits = 5))
        mat_res[6, 5] <- formatC(fees_x, digits = 4, format = "f")
        mat_res[6, 6] <- format(fees_y, nsmall = 4, format = "f")

        # # sigma = 0.002

        sigma <- 0.002
        Pa <- 999
        Pb <- 1001
        range_factor <- sqrt(Pb / Pa)

        EIL <- calc_eil(P, Pa, Pb, mu, sigma, t)
        time_ITM <- calc_time_ITM_integrals(P, Pa, Pb, mu, sigma, t)
        alpha <- calc_alpha(mu, sigma, time_ITM)        
        fees_x <- calc_expected_total_fees_x(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, alpha)
        beta <- calc_beta(mu, sigma, time_ITM)
        fees_y <- calc_expected_total_fees_y(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, beta)
        
        mat_res[1, 7] <- formatC(time_ITM, digits = 2, format = "f")
        mat_res[1, 8] <- ifelse((-1 * EIL) < THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL , format = "f", digits = 5))
        mat_res[1, 9] <- formatC(fees_x, digits = 4, format = "f")
        mat_res[1, 10] <-format(fees_y, nsmall = 4, format = "f")

        Pa <- 990
        Pb <- 1010
        range_factor <- sqrt(Pb / Pa)

        EIL <- calc_eil(P, Pa, Pb, mu, sigma, t)
        time_ITM <- calc_time_ITM_integrals(P, Pa, Pb, mu, sigma, t)
        alpha <- calc_alpha(mu, sigma, time_ITM)
        fees_x <- calc_expected_total_fees_x(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, alpha)
        beta <- calc_beta(mu, sigma, time_ITM)
        fees_y <- calc_expected_total_fees_y(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, beta)
        
        mat_res[2, 7] <- formatC(time_ITM, digits = 2, format = "f")
        mat_res[2, 8] <- ifelse((-1 * EIL) < THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL, format = "f", digits = 5))
        mat_res[2, 9] <- formatC(fees_x, digits = 4, format = "f")
        mat_res[2, 10] <-format(fees_y, nsmall = 4, format = "f")

        Pa <- 909
        Pb <- 1100
        range_factor <- sqrt(Pb / Pa)

        EIL <- calc_eil(P, Pa, Pb, mu, sigma, t)
        time_ITM <- calc_time_ITM_integrals(P, Pa, Pb, mu, sigma, t)
        alpha <- calc_alpha(mu, sigma, time_ITM)
        fees_x <- calc_expected_total_fees_x(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, alpha)
        beta <- calc_beta(mu, sigma, time_ITM)
        fees_y <- calc_expected_total_fees_y(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, beta)
        
        mat_res[3, 7] <- formatC(time_ITM, digits = 2, format = "f")
        mat_res[3, 8] <- ifelse((-1 * EIL) < THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL, format = "f", digits = 5))
        mat_res[3, 9] <- formatC(fees_x, digits = 4, format = "f")
        mat_res[3, 10] <-format(fees_y, nsmall = 4, format = "f")

        Pa <- 833.33
        Pb <- 1200
        range_factor <- sqrt(Pb / Pa)

        EIL <- calc_eil(P, Pa, Pb, mu, sigma, t)
        time_ITM <- calc_time_ITM_integrals(P, Pa, Pb, mu, sigma, t)
        alpha <- calc_alpha(mu, sigma, time_ITM)
        fees_x <- calc_expected_total_fees_x(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, alpha)
        beta <- calc_beta(mu, sigma, time_ITM)
        fees_y <- calc_expected_total_fees_y(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, beta)
        
        mat_res[4, 7] <- formatC(time_ITM, digits = 2, format = "f")
        mat_res[4, 8] <- ifelse((-1 * EIL) < THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL, format = "f", digits = 5))
        mat_res[4, 9] <- formatC(fees_x, digits = 4, format = "f")
        mat_res[4, 10] <-format(fees_y, nsmall = 4, format = "f")

        Pa <- 500
        Pb <- 2000
        range_factor <- sqrt(Pb / Pa)

        EIL <- calc_eil(P, Pa, Pb, mu, sigma, t)
        time_ITM <- calc_time_ITM_integrals(P, Pa, Pb, mu, sigma, t)
        alpha <- calc_alpha(mu, sigma, time_ITM)        
        fees_x <- calc_expected_total_fees_x(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, alpha)
        beta <- calc_beta(mu, sigma, time_ITM)
        fees_y <- calc_expected_total_fees_y(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, beta)
        
        mat_res[5, 7] <- formatC(time_ITM, digits = 2, format = "f")
        mat_res[5, 8] <- ifelse((-1 * EIL) < THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL, format = "f",digits = 5))
        mat_res[5, 9] <- formatC(fees_x, digits = 4, format = "f")
        mat_res[5, 10] <-format(fees_y, nsmall = 4, format = "f")

        Pa <- 200
        Pb <- 5000
        range_factor <- sqrt(Pb / Pa)

        EIL <- calc_eil(P, Pa, Pb, mu, sigma, t)
        time_ITM <- calc_time_ITM_integrals(P, Pa, Pb, mu, sigma, t)
        alpha <- calc_alpha(mu, sigma, time_ITM)        
        fees_x <- calc_expected_total_fees_x(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, alpha)
        beta <- calc_beta(mu, sigma, time_ITM)
        fees_y <- calc_expected_total_fees_y(V, P, Pa, Pb, mu, 0.003, time_ITM, t * 10, beta)
        
        mat_res[6, 7] <- formatC(time_ITM, digits = 2, format = "f")
        mat_res[6, 8] <- ifelse((-1 * EIL) < THRESHOLD, formatC(EIL, format = "e", digits = 2) , formatC(EIL, format = "f", digits = 5))
        mat_res[6, 9] <- formatC(fees_x, digits = 4, format = "f")
        mat_res[6, 10] <-format(fees_y, nsmall = 4, format = "f")

        return(mat_res)
}

run_calc <- function(mu, t) {

    tryCatch({
        
        first_chunk <- calc_chunk(mu, t)
        second_chunk <- calc_chunk(mu * 2, t)

        prmatrix(first_chunk)
        prmatrix(second_chunk)

    }, error = function(e) {
        if (length(e) > 1) {
            print(e)
            return(NA)
        }
    })
}

run_calc(0.000075, 168)
# run_calc()