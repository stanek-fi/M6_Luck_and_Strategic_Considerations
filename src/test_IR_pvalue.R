rm(list = ls())
library(yaml)
library(data.table)
library(MASS)

source(file.path("src", "helpers", "WYY.R"))

returns_list <- readRDS(file.path("data", "processed", "returns_list.RDS"))
positions_list <- readRDS(file.path("data", "processed", "positions_list.RDS"))

is_dummy_submission <- do.call(cbind,lapply(positions_list, function(x) {
    apply(x,1,function(y) {all(y==0.01)})
}))
dummy_teams <- apply(is_dummy_submission,1,all)
dummy_teams <- names(dummy_teams)[dummy_teams]
dummy_teams <- dummy_teams[dummy_teams != "32cdcc24"] # * excluding the official M6 Dummy
print(paste0("Excluding ", length(dummy_teams), " teams whose submissions are identical to the M6 dummy"))
positions_list <- lapply(positions_list, function(x) {
    x[!(rownames(x)%in%dummy_teams),]
})

set.seed(1)
pvalue <- WYY(returns_list, positions_list, critical_value = "wild_bootstrap", R=1000, covariance_estimator = "HAC")

pvalue_table <- data.table(
    test="WYY (w.b.)",
    pvalue=pvalue
)
print(pvalue_table)
dir.create(file.path("outputs", "tables"), showWarnings = FALSE)
write.csv(pvalue_table, file.path("outputs", "tables", "test_IR_pvalue_table.csv"), row.names = FALSE)
