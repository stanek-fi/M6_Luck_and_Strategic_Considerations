simulate_returns_BEKK_internal <- function(N, mu, sigma, alpha = 0, beta = 0, df = Inf, burnin = 1000){
    if(df == Inf){
        sigma_scale <- 1
    }else{
        sigma_scale <- df/(df-2)
    }
    C <- sigma*(1-alpha-beta)
    returns <- matrix(NA, nrow = N+burnin, ncol = length(mu))
    H_past <- C
    r_past <- mu

    for(n in seq_len(N+burnin)){
        H <- C + alpha * (r_past-mu) %*% t(r_past-mu) + beta * H_past
        r <- as.vector(rmvt(1, sigma = H/sigma_scale, df = df, delta = mu))
        returns[n, ] <- r
        r_past <- r
        H_past <- H
    }
    returns <- returns[-seq_len(burnin), ]
    return(returns)
}


simulate_returns_BEKK <- function(rounds_num, days_per_round, mu, sigma, lambda = 0, alpha = 0, beta = 0, df = Inf, burnin = 1000){
    if(sum(lambda)>1){
        stop("invalid lambda vector")
    }
    N <- rounds_num*days_per_round

    returns_predictable <- lapply(lambda, function(lam) {mvrnorm(
        N,
        mu = lam * mu,
        Sigma = sqrt(lam)^2 * sigma
    )})
    returns_unpredictable <- simulate_returns_BEKK_internal(
        N,
        mu = (1 - sum(lambda)) * mu,
        sigma = sqrt(1 - sum(lambda))^2 * sigma,
        alpha = alpha,
        beta = beta,
        df = df,
        burnin = burnin
    )
    returns_total <- Reduce("+", returns_predictable) + returns_unpredictable

    returns_list <- lapply(seq_len(rounds_num), function(x) {
        list(
            returns_total = returns_total[(1 + (x - 1) * days_per_round):(x * days_per_round ),],
            returns_predictable = lapply(returns_predictable, function(obj) {
                obj[(1 + (x - 1) * days_per_round):(x * days_per_round ),]
            }),
            returns_unpredictable = returns_unpredictable[(1 + (x - 1) * days_per_round):(x * days_per_round ),]
        )
    })

    return(returns_list)
}