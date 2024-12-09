compute_WYY_stat <- function(X1,covariance_estimator){
    K <- ncol(X1)
    N <- nrow(X1)
    X2 <- X1^2
    X <- cbind(X1,X2)
    X_mean <- apply(X,2,mean)
    X1_mean <- X_mean[1:K]
    X2_mean <- X_mean[(K+1):(K*2)]

    if(covariance_estimator == "IID"){
        sigma_raw <- cov(X)
    }else if(covariance_estimator=="HAC"){
        m <- floor(N^(1/4))
        Xdm <- apply(X,2,function(y){y-mean(y)})
        lrmats <- lapply(seq_len(m), function(j) {
            temp <- (1/N)*t(Xdm[1:(N - j),]) %*% Xdm[(1+j):N,]
            (1-j/(m+1))*(temp+t(temp))
        })
        sigma_raw <- (1 / N) * t(Xdm) %*% Xdm + Reduce("+", lrmats)
    }

    D <- cbind(
        diag(X2_mean/(X2_mean - X1_mean^2)^(3/2)),
        diag(-X1_mean/(2*(X2_mean - X1_mean^2)^(3/2)))
    )
    sigma <- D%*%sigma_raw%*%t(D)
    SR <- X1_mean/sqrt(X2_mean - X1_mean^2)

    Q <- matrix(0,nrow = K-1, ncol = K)
    Q[cbind(1:(K-1),1:(K-1))]=1
    Q[cbind(1:(K-1),1+1:(K-1))]=-1

    ts <- N*t(Q%*%SR)%*%solve(Q%*%sigma%*%t(Q))%*%(Q%*%SR)
    return(as.vector(ts))
}

WYY <- function(returns_list, positions_list, critical_value = "analytic", covariance_estimator="HAC", R=1000, block_length = NULL){

    X1 <- do.call(rbind, lapply(seq_along(returns_list), function(m) {
        returns_list[[m]] %*% t(positions_list[[m]])
    }))
    K <- ncol(X1)
    returns_lengths <- sapply(returns_list, function(x) nrow(x))

    ts <- compute_WYY_stat(X1, covariance_estimator = covariance_estimator)

    if (critical_value == "analytic"){

        p <- 1 - pchisq(ts, df = K - 1)

    } else if (critical_value == "wild_bootstrap") {

        ts_BS <- sapply(1:R, function(r) {

            sign_resamples <- do.call(cbind,lapply(seq_len(K), function(x) {rep(sample(c(-1, 1), length(returns_lengths), replace = T), times = returns_lengths)}))
            X1b <- X1*sign_resamples

            compute_WYY_stat(X1b, covariance_estimator = covariance_estimator)
        })
        p <- mean(ts<ts_BS)

    } else if (critical_value == "block_bootstrap"){
        temp <- tsboot(X1, compute_WYY_stat, R = R, sim="fixed", l=block_length, covariance_estimator = covariance_estimator)
        p <- mean(ts<temp$t)
    }

    return(p)
}