library(data.table)
library(ggplot2)
library(scales)
library(stringr)
library(latex2exp)

participant_rank <- readRDS(file.path("models", "participant_rank_simulated.RDS"))
participant_state <- readRDS(file.path("models", "participant_state_simulated.RDS"))
participant_beta <- readRDS(file.path("models", "participant_beta_simulated.RDS"))

# -- rank histogram ------------------------------------------------------------

participant_rank_dt <- as.data.table(participant_rank)
participant_rank_long <- melt(participant_rank_dt, measure.vars = colnames(participant_rank_dt), variable.name = "portfolio", value.name = "rank")
participant_state_dt <- as.data.table(participant_state)
participant_state_long <- melt(participant_state_dt, measure.vars = colnames(participant_state_dt), variable.name = "portfolio", value.name = "state")
participant_beta_dt <- as.data.table(participant_beta)
participant_beta_long <- melt(participant_beta_dt, measure.vars = colnames(participant_beta_dt), variable.name = "portfolio", value.name = "beta")
participant_rank_long_subset <- participant_rank_long[str_detect(portfolio, c("baseline")) | str_detect(portfolio, c("rank")) | str_detect(portfolio, c("0003"))]

temp <- participant_rank_long_subset
orig_levels <- levels(participant_rank_long_subset$portfolio)
orig_levels[!str_detect(orig_levels,"lambda")] <- str_replace_all(orig_levels[!str_detect(orig_levels,"lambda")]," ", "\\\\,")
levels(temp$portfolio) <- TeX(orig_levels)
rank_histogram <- ggplot(temp, aes(rank, fill = portfolio)) +
    geom_histogram(aes(y = after_stat(count / sum(count[group == 1]))), binwidth = 1, alpha = .5, position = "identity") +
    ylab("frequency") +
    scale_fill_manual(values = hue_pal()(4), labels = parse_format())+
    theme(legend.position="bottom", legend.title=element_blank())
dir.create(file.path("outputs", "plots"), showWarnings = FALSE)
ggsave(file.path("outputs", "plots", "rank_histogram_simulated.png"), rank_histogram, width = 6.6, height = 6)

# -- state table ---------------------------------------------------------------

rank_table <- participant_rank_long[, .(top_1 = mean(rank == 1), top_5 = mean(rank <= 5), top_10 = mean(rank <= 10), top_20 = mean(rank <= 20)), .(portfolio)]
state_table <- participant_state_long[, .(IR = mean(state)), portfolio]
beta_table <- participant_beta_long[, .(beta = mean(beta)), portfolio]
state_table <- merge(merge(state_table, beta_table, by = "portfolio"), rank_table, by = "portfolio")
print(state_table)

dir.create(file.path("outputs", "tables"), showWarnings = FALSE)
temp <- state_table[str_detect(portfolio, c("baseline")) | str_detect(portfolio, c("tangency"))]
temp$beta <- NULL
write.csv(temp, file.path("outputs", "tables", "state_table_simulated_tangency.csv"), row.names = FALSE)

temp <- state_table[str_detect(portfolio, c("baseline")) | str_detect(portfolio, c("rank")) | str_detect(portfolio, c("0003")) | str_detect(portfolio, c("= 0\\)"))]
write.csv(temp, file.path("outputs", "tables", "state_table_simulated_rank_optimization.csv"), row.names = FALSE)
