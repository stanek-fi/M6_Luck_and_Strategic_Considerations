rm(list = ls())
library(yaml)
library(data.table)
library(MASS)
library(doParallel)
library(stringr)
library(mvtnorm)

source(file.path("src", "helpers", "simulate_returns_BEKK.R"))
source(file.path("src", "helpers", "position_baseline.R"))
source(file.path("src", "helpers", "WYY.R"))

par <- yaml.load_file("par.yaml")
coefficients <- yaml.load_file(file.path("outputs", "metrics", "coefficients.yaml"))

mu <- rep(par$returns_mean, par$returns_num)
sigma <- coefficients$returns_var * diag(par$returns_num) + coefficients$returns_cov * (matrix(1, par$returns_num, par$returns_num) - diag(par$returns_num))

participants_nums <- c(5, 50, 163)
rounds_nums <- c(1)*12
R <- 1000

sensitivity_parameters <- data.table(
    alpha = c(0, 0.10, 0.10),
    beta = c(0, 0.85, 0.85),
    df = c(6, Inf, 6)
)

s <- 1

for(s in seq_len(nrow(sensitivity_parameters))){
    alpha <- sensitivity_parameters[s, alpha]
    beta <- sensitivity_parameters[s, beta]
    df <- sensitivity_parameters[s, df]

    numCores <- detectCores() - 1
    cl <- makeCluster(numCores)
    registerDoParallel(cl)
    clusterEvalQ(cl, library(MASS))
    clusterEvalQ(cl, library(mvtnorm))

    par_combinations <- CJ(participants_num = participants_nums, rounds_num =  rounds_nums)
    res <- vector("list", nrow(par_combinations))

    for(i in seq_along(res)){
        participants_num <- par_combinations[i, participants_num]
        rounds_num <- par_combinations[i, rounds_num]
        print(paste0("participants_num: ", participants_num, " rounds_num: ", rounds_num, " time: ", Sys.time()))

        temp <- foreach(r = 1:R) %dopar% {
            set.seed(r)

            returns_list <- simulate_returns_BEKK(rounds_num, par$days_per_round, mu, sigma, lambda = 0, alpha = alpha, beta = beta, df = df, burnin = 10)
            returns_list <- lapply(returns_list, function(obj) obj$returns_total)

            positions_list <- lapply(seq_len(rounds_num), function(round) {
                position_baseline(
                    returns_num = par$returns_num,
                    participants_num = participants_num,
                    num_shorted = coefficients$num_shorted,
                    num_zeros = coefficients$num_zeros
                )
            })
            list(
                "WYY" = WYY(returns_list, positions_list, critical_value = "analytic", covariance_estimator = "HAC"),
                "WB-WYY" = WYY(returns_list, positions_list, critical_value = "wild_bootstrap", R = 1000, covariance_estimator = "HAC")
            )
        }
        tempdt <- rbindlist(temp)
        tempdt[,rounds_num:=rounds_num]
        tempdt[,participants_num:=participants_num]
        res[[i]] <- tempdt
    }
    stopCluster(cl)
    res <- do.call(rbind, res)

    dir.create(file.path("models"), showWarnings = FALSE)
    saveRDS(res, file.path("models", paste0("test_IR_level_sensitivity_",s,".RDS")))
}




