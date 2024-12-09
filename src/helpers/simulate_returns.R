simulate_returns <- function(days_per_round, mu, sigma, lambda = 0) {
    if(sum(lambda)>1){
        stop("invalid lambda vector")
    }
    returns_predictable <- lapply(lambda, function(lam) {mvrnorm(
        n = days_per_round,
        mu = lam * mu,
        Sigma = sqrt(lam)^2 * sigma
    )})
    returns_unpredictable <- mvrnorm(
            n = days_per_round,
            mu = (1 - sum(lambda)) * mu,
            Sigma = sqrt(1 - sum(lambda))^2 * sigma
    )
    # returns_total <- returns_predictable + returns_unpredictable
    returns_total <- Reduce("+", returns_predictable) + returns_unpredictable
    list(
        returns_total = returns_total,
        returns_predictable = returns_predictable,
        returns_unpredictable = returns_unpredictable
    )
}