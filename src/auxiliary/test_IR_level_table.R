rm(list = ls())
library(data.table)
library(ggplot2)
library(stringr)

res <- readRDS(file.path("models", paste0("test_IR_level.RDS")))

mres <- melt(res,id.vars=c("participants_num", "rounds_num"), variable.name = "test")
amres <- mres[, .(rr010 = mean(value < 0.10), rr005 = mean(value < 0.05), rr001 = mean(value < 0.01)), .(participants_num, rounds_num, test)]
amres <- melt(amres, id.vars=c("participants_num", "rounds_num", "test"), variable.name = "level")
amres[,level_num := as.numeric(str_sub(level, 3))/100]
amres[,years:=rounds_num/12]

level_table <- dcast(amres, level_num + years ~ participants_num + test, value.var = "value")
level_table$level_num <- do.call(c, lapply(unique(level_table$level_num), function(x) {
    c(x, rep("", length(unique(level_table$years)) - 1))
}))

print(level_table)
dir.create(file.path("outputs", "tables"), showWarnings = FALSE)
write.csv(level_table, file.path("outputs", "tables", "test_IR_level_table.csv"), row.names = FALSE)


tab_asy <- dcast(amres[years==1& test=="WYY"], level_num + years ~ participants_num + test, value.var = "value")
tab_asy$years <- NULL
tab_wb <- dcast(amres[years==1& test=="WB-WYY"], level_num + years ~ participants_num + test, value.var = "value")
tab_wb$years <- NULL
write.csv(tab_asy, file.path("outputs", "tables", "test_IR_level_table_asy.csv"), row.names = FALSE)
write.csv(tab_wb, file.path("outputs", "tables", "test_IR_level_table_wb.csv"), row.names = FALSE)
