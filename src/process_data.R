rm(list = ls())
library(data.table)
library(stringr)
library(yaml)
library(stringr)

par <- yaml.load_file("par.yaml")

# -- processing data -----------------------------------------------------------

prices <- as.data.table(read.csv(file.path("data", "raw", "prices", "assets_M6.csv")))
prices[, date := as.Date(date, "%Y/%m/%d")]
prices <- dcast(prices, date ~ symbol, value.var = "price")
setnafill(prices, type = "locf", cols = colnames(prices)[-1])
returns <- prices[, .(date, .SD / shift(.SD) - 1), .SDcols = colnames(prices)[-1]]

intervals <- list(
    start = seq(as.Date("2022-03-07"), by = 28, length.out = 12),
    end = seq(as.Date("2022-04-03"), by = 28, length.out = 12)
)
returns_list <- lapply(seq_along(intervals$start), function(i) {
    temp <- returns[date >= intervals$start[i] & date <= intervals$end[i], .SD, .SDcols = colnames(returns)[-1]]
    temp <- as.matrix(temp)
    temp[is.na(temp)] <- 0
    temp
})

leaderboard <- do.call(rbind, lapply(1:par$rounds_num_fixed, function(i) {
    temp <- as.data.table(
        read.csv(
            file.path("data", "raw", "leaderboard", paste0(i, ".csv")),
            header = FALSE,
            sep = ";",
            col.names = c("rank", "name", "rank_avg", "rps", "rank_rps", "ir", "rank_ir")
        )
    )
    temp[, id := word(name)]
    temp[, month := i]
    temp
}))
leaderboard[, eligible := .N == par$rounds_num_fixed, id]

positions <- fread(file.path("data", "raw", "submissions", "submissions.csv"))
positions <- positions[Evaluation!="Trial run"]
positions[,Symbol := ifelse(Symbol=="BF-B", "BF.B", Symbol)] # * unifying names of assets 
positions[, month := as.numeric(str_sub(as.character(Evaluation), 1, -14))]
eligible <- positions[IsActive == 1, .N / 100, Team][V1 == 12, Team]
positions_list <- lapply(1:12, function(i) {
    temp <- positions[month==i & Team %in% eligible]
    temp <- dcast(temp, Team~Symbol, value.var = "Decision")
    temp_matrix <- as.matrix(temp[,-1])
    rownames(temp_matrix) <- temp[,Team]
    temp_matrix
})

eligible <- positions[IsActive == 1, .N / 100, Team][, Team]
positions_all_list <- lapply(1:12, function(i) {
    temp <- positions[month==i & Team %in% eligible]
    temp <- dcast(temp, Team~Symbol, value.var = "Decision")
    temp_matrix <- as.matrix(temp[,-1])
    rownames(temp_matrix) <- temp[,Team]
    temp_matrix
})

# -- saving results -----------------------------------------------------------

dir.create(file.path("data", "processed"), showWarnings = FALSE)
saveRDS(returns, file.path("data", "processed", "returns.RDS"))
saveRDS(returns_list, file.path("data", "processed", "returns_list.RDS"))
saveRDS(leaderboard, file.path("data", "processed", "leaderboard.RDS"))
saveRDS(positions_list, file.path("data", "processed", "positions_list.RDS"))
saveRDS(positions_all_list, file.path("data", "processed", "positions_all_list.RDS"))
