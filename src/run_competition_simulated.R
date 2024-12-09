rm(list = ls())
library(yaml)
library(data.table)
library(MASS)
library(doParallel)

source(file.path("src", "helpers", "position_strategic.R"))
source(file.path("src", "helpers", "position_baseline.R"))
source(file.path("src", "helpers", "simulate_returns.R"))
source(file.path("src", "helpers", "compute_round.R"))
source(file.path("src", "helpers", "position_tangency.R"))
source(file.path("src", "helpers", "invert_double_constant.R"))

strategy_q_1 <- readRDS(file.path("models", "strategy_q_1.RDS"))
strategy_q_20 <- readRDS(file.path("models", "strategy_q_20.RDS"))
par <- yaml.load_file("par.yaml")
coefficients <- yaml.load_file(file.path("outputs", "metrics", "coefficients.yaml"))
mu <- rep(par$returns_mean, par$returns_num)
sigma <- coefficients$returns_var * diag(par$returns_num) + coefficients$returns_cov * (matrix(1, par$returns_num, par$returns_num) - diag(par$returns_num))

returns_lambda <- unlist(par$returns_lambda)
qs <- c(1,20)

# -- simulating competition ----------------------------------------------------

participant_types_measured <- c(
    list(list(type="baseline", j=1, name = "baseline")),
    lapply(seq_along(returns_lambda), function(x) list(
        type="tangency", 
        j=x, 
        name = paste0("tangency ($\\lambda$ = ", format(returns_lambda[x], scientific=F ),")")
    )),
    lapply(seq_along(qs), function(x) list(
        type="rank optimization", 
        j=x,
        name = paste0("rank opt. (q = ", qs[x],")")
    ))
)
participant_types_measured_names <- sapply(participant_types_measured, function(x) {x$name})
participant_types_other <- rep(list(list(type="baseline")), c(par$participants_num - 1))
participant_types <- c(participant_types_measured, participant_types_other)

numCores <- detectCores() - 1 # * multi-thread
cl <- makeCluster(numCores)
registerDoParallel(cl)
clusterEvalQ(cl, library(MASS))
clusterEvalQ(cl, library(data.table))

# set.seed(par$seed)
# res <- lapply(1:par$rep_simulate_competition, function(r) { # * single-thread
res <- foreach(r = 1:par$rep_simulate_competition) %dopar% { # * multi-thread

    set.seed(r)
    states <- rep(0, length(participant_types))

    returns <- lapply(seq_len(par$rounds_num), function(m) {
        simulate_returns(
            days_per_round = par$days_per_round,
            mu = mu,
            sigma = sigma,
            lambda = returns_lambda
        )
    })
    positions <- lapply(seq_len(par$rounds_num), function(m) {
        matrix(NA, nrow = length(participant_types), ncol = par$returns_num)
    })

    for (rounds_remaining in rev(seq_len(par$rounds_num))) {

        m <- par$rounds_num - rounds_remaining + 1

        for (i in seq_len(nrow(positions[[m]]))) {
            switch(participant_types[[i]]$type,
                "baseline" = {
                    positions[[m]][i, ] <- position_baseline(
                        returns_num = par$returns_num,
                        participants_num = 1,
                        num_shorted = coefficients$num_shorted,
                        num_zeros = coefficients$num_zeros
                    )
                },
                "tangency" = {
                    positions[[m]][i, ] <- position_tangency(
                        mu = mu,
                        sigma = sigma,
                        returns_predictable = returns[[m]]$returns_predictable[[participant_types[[i]]$j]],
                        lambda = returns_lambda[participant_types[[i]]$j]
                    )
                },
                "rank optimization" = {
                    strategy <- switch(as.character(qs[participant_types[[i]]$j]), 
                        "1" = {strategy_q_1},
                        "20" = {strategy_q_20}
                    )
                    positions[[m]][i, ] <- position_strategic(
                        returns_num = par$returns_num,
                        participants_num = 1,
                        state_own = states[i],
                        state_other = states[-seq_along(participant_types_measured)],
                        rounds_remaining = rounds_remaining,
                        strategy = strategy,
                        q = qs[participant_types[[i]]$j]
                    )
                }
            )
        }
        states <- compute_round(
            returns = lapply(returns[1:m], function(x) {x$returns_total}),
            positions = positions[1:m],
            states = NULL
        )
    }
    states_measured <- states[seq_along(participant_types_measured)]
    states_other <- states[-seq_along(participant_types_measured)]
    participant_beta <- apply(do.call(rbind,
        lapply(seq_len(par$rounds_num), function(m) apply(positions[[m]][seq_along(participant_types_measured),], 1, function(y) {
            sum(ifelse(y>0,abs(y),0))/sum(abs(y))
        }
    ))),2,mean)

    list(
        participant_state = states_measured,
        participant_rank = sapply(states_measured, function(x) {
            rank(-c(x, states_other))[1]
        }),
        participant_beta = participant_beta
    )
}
# ) # * single-thread
message(paste0("run_competition_simulated:", 100, "%", " time:", Sys.time()))

participant_state <- do.call(rbind,lapply(res, function(x) x$participant_state))
dimnames(participant_state) <- list(NULL,participant_types_measured_names)

participant_rank <- do.call(rbind,lapply(res, function(x) x$participant_rank))
dimnames(participant_rank) <- list(NULL,participant_types_measured_names)

participant_beta <- do.call(rbind,lapply(res, function(x) x$participant_beta))
dimnames(participant_beta) <- list(NULL,participant_types_measured_names)

# -- saving results ------------------------------------------------------------

dir.create(file.path("models"), showWarnings = FALSE)
saveRDS(participant_rank, file.path("models", "participant_rank_simulated.RDS"))
saveRDS(participant_state, file.path("models", "participant_state_simulated.RDS"))
saveRDS(participant_beta, file.path("models", "participant_beta_simulated.RDS"))

print(cbind(
    mean_IR = apply(participant_state,2,mean),
    prob_q_1 = apply(participant_rank,2,function(x) mean(x<=1)),
    prob_q_20 = apply(participant_rank,2,function(x) mean(x<=20))
))
