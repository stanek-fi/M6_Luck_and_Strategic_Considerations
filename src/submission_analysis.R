rm(list = ls())
library(yaml)
library(data.table)
library(MASS)
library(profvis)
library(sandwich)
library(ggplot2)
library(latex2exp)
library(moments)
library(scales)

source(file.path("src", "helpers", "compute_round.R"))

returns_list <- readRDS(file.path("data", "processed", "returns_list.RDS"))
positions_list <- readRDS(file.path("data", "processed", "positions_all_list.RDS"))


temp <- do.call(rbind, returns_list)
assets_skew <- apply(temp, 2, skewness)

leaderboard <- do.call(rbind,lapply(rownames(positions_list[[12]]), function(team) {
    positions <- lapply(positions_list,function(x) {
        if(team %in% rownames(x)){
            x[team,,drop=F]
        }else{
            matrix(NA, nrow=1, ncol=ncol(x))
        }
    })
    data.table(
        team = team,
        l_m = 1:12,
        l_q = rep(1:4,each=3),
        IR_m = sapply(1:12, function(m) compute_round(returns_list[m], positions[m])),
        IR_q = do.call(c, lapply(1:4, function(q) sapply(1:3, function(m) compute_round(returns_list[3 * (q - 1) + 1:m], positions[3 * (q - 1) + 1:m])))),
        IR_g = sapply(1:12, function(m) compute_round(returns_list[1:m], positions[1:m]))
    )
}))

na_rank <- function(x) {ifelse(is.na(x),NA, rank(x))}
leaderboard[, rank_m := na_rank(-IR_m), l_m]
leaderboard <- leaderboard[, .SD[, .(team, IR_m, IR_g, IR_q, rank_m, rank_q = na_rank(-IR_q)), l_m], l_q]
leaderboard[,rank_g := na_rank(-IR_g),l_m]
leaderboard[,l_m_lead := l_m+1]
leaderboard[,rank_end_g := .SD[l_m==max(l_m),rank_g],team]
leaderboard[,rank_end_q := .SD[l_m==max(l_m),rank_q],.(team,l_q)]

positions_statistics <- do.call(rbind,lapply(seq_along(positions_list), function(m){
    temp <- t(apply(positions_list[[m]], 1, function(y) {
        c(
            long_ratio = sum(ifelse(y>0,abs(y),0))/sum(abs(y)),
            skew_exposure = sum(y/sum(abs(y))*assets_skew),
            p_m=m
        )
    }))
    temp <- as.data.table(temp)
    temp$team <- rownames(positions_list[[m]])
    temp
}))

d <- merge(leaderboard, positions_statistics, by.x=c("team", "l_m"),by.y=c("team", "p_m"))
d <- d[order(team, l_m)]
d[,abs_rank_diff := abs(c(NA,diff(rank_g))),team]

# -- outputs -------------------------------------------------------------------

temp <- d[,.(value = mean(long_ratio), position = "long"),l_m]
temp <- rbind(temp, temp[, .(l_m, value = 1 - value, position = "short")])
temp[, m := as.factor(l_m)]
temp[, position := factor(position, levels = c("short", "long"), labels = c(TeX("$\\beta^{-}_{m,\\cdot}$"), TeX("$\\beta^{+}_{m,\\cdot}$")))]
temp[, label:= as.character(format(round(value,2),2))]
temp[, label_y:= cumsum(value)-value/2,l_m]
long <- ggplot(temp, aes(x=m,y=value, fill=position))+
    geom_bar(position = "stack", stat = "identity", alpha=0.8)+
    geom_text(aes(y=label_y,label = label), vjust = 0)+
    scale_fill_discrete(labels = parse_format())+
    theme(legend.title = element_blank(),legend.position = "bottom")+
    ylab("")
dir.create(file.path("outputs", "plots"), showWarnings = FALSE)
ggsave(file.path("outputs", "plots", "submission_analysis_long.png"), long, width = 6.6, height = 6)

temp <- d
temp[,m:=as.factor(l_m)]
long_rank_diff <- ggplot(na.omit(d), aes(x=long_ratio, y=abs_rank_diff, colour=m, fill=m))+
    geom_point(alpha=0.3)+
    geom_smooth(alpha = 0.2,method = 'loess', formula = 'y ~ x')+
    ylab(TeX("$|rank_{T_{1}:T_{m-1},k} - rank_{T_{1}:T_{m},k}|$"))+
    xlab(TeX("$\\beta^{+}_{m,k}$"))
dir.create(file.path("outputs", "plots"), showWarnings = FALSE)
ggsave(file.path("outputs", "plots", "submission_analysis_long_rank_diff.png"), long_rank_diff, width = 6.6, height = 6)

temp_g <- d[,.(mean_long_ratio = mean(long_ratio), IR=last(IR_g)),.(rank_end_g,team)]
temp_g[,rank := rank_end_g]
temp_g[,period := "global"]
temp_q <- d[,.(mean_long_ratio = mean(long_ratio), IR=last(IR_q)),.(rank_end_q,l_q,team)]
temp_q[,rank := rank_end_q]
temp_q[, period := paste0("Q",l_q)]
temp <- rbind(temp_g[, .(mean_long_ratio, rank, period, IR, team)], temp_q[, .(mean_long_ratio, rank, period, IR, team)])
temp[, period := factor(period, levels = c("Q1", "Q2", "Q3", "Q4", "global"))]
temp <- na.omit(temp)
temp[,threshold:=quantile(mean_long_ratio,0.5),period]
temp[,above_threshold := (mean_long_ratio > threshold),period]
temp[rank<=5][order(period,rank)]

rank_long <- ggplot(temp, aes(x=rank, y=mean_long_ratio))+
    geom_point(alpha=0.3)+
    geom_smooth(method = 'loess', formula = 'y ~ x')+
    facet_grid(period~.)+
    xlab(TeX("$rank_{\\cdot,k}$"))+
    ylab(TeX("$\\bar{\\beta}^{+}_{\\cdot,k}$"))
dir.create(file.path("outputs", "plots"), showWarnings = FALSE)
ggsave(file.path("outputs", "plots", "submission_analysis_rank_long.png"), rank_long, width = 6.6, height = 14)

median_prob <- temp[,.(p5=mean(rank<=5,na.rm=T), p10=mean(rank<=10,na.rm=T), p20=mean(rank<=20,na.rm=T)),.(period,above_threshold)]
median_prob <- dcast(median_prob, period~above_threshold, value.var = c("p5", "p10"))
median_prob <- merge(temp[team == "32cdcc24",.(period, IR, threshold)],median_prob, by="period")
print(median_prob)
dir.create(file.path("outputs", "tables"), showWarnings = FALSE)
write.csv(median_prob, file.path("outputs", "tables", "submission_analysis_median_prob.csv"), row.names = FALSE)

temp_g <- d[,.(mean_skew_exposure = mean(skew_exposure), IR=last(IR_g)),.(rank_end_g,team)]
temp_g[,rank := rank_end_g]
temp_g[,period := "global"]
temp_q <- d[,.(mean_skew_exposure = mean(skew_exposure), IR=last(IR_q)),.(rank_end_q,l_q,team)]
temp_q[,rank := rank_end_q]
temp_q[, period := paste0("Q",l_q)]
temp <- rbind(temp_g[, .(mean_skew_exposure, rank, period, IR, team)], temp_q[, .(mean_skew_exposure, rank, period, IR, team)])
temp[, period := factor(period, levels = c("Q1", "Q2", "Q3", "Q4", "global"))]
temp <- na.omit(temp)

rank_skew <- ggplot(temp, aes(x=rank, y=mean_skew_exposure))+
    geom_point(alpha=0.3)+
    geom_smooth(method = 'loess', formula = 'y ~ x')+
    facet_grid(period~.)+
    xlab(TeX("$rank_{\\cdot,k}$"))+
    ylab(TeX("$\\bar{\\gamma}_{\\cdot,k}$"))
dir.create(file.path("outputs", "plots"), showWarnings = FALSE)
ggsave(file.path("outputs", "plots", "submission_analysis_rank_skew.png"), rank_skew, width = 6.6, height = 14)