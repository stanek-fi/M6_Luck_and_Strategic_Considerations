compute_round <- function(returns, positions, states = NULL) {
    if(is.matrix(returns) & is.matrix(positions)){
        returns <- list(returns)
        positions <- list(positions)
    } 
    if (is.null(states)) {
        states <- rep(0, nrow(positions[[1]]))
    }
    ret <- do.call(rbind,lapply(seq_along(returns), function(i) {
        log(1 + returns[[i]] %*% t(positions[[i]]))
    }))
    sret <- apply(ret, 2, sum)
    sdp <- apply(ret, 2, sd)
    states <- states + sret / sdp
    return(states)
}


