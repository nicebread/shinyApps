library(googlesheets)
library(dplyr)

RPP_handler <- register_ss("https://docs.google.com/spreadsheet/ccc?key=0ApoirNUlNviedHJLTVo4V3hpUlpjQnhIUzdRMHh0SEE&usp=sharing")

RPP <- get_via_cf(RPP_handler, ws = 1) 

RPP2 <- reshape_cf(RPP %>% filter(!row %in% c(1, 3)))

# 17 = p.value original; 34 = p.value replication

colnames(RPP2)[17] <- "p_original"
colnames(RPP2)[34] <- "p_replication"

#save(RPP2, file="RPP_data.RData")
load(file="RPP_data.RData")

# full plot
plot(NA, xlab="Original p value", ylab="Replication p value", pch=20, xlim=c(0, 1), ylim=c(0, 1))
polygon(c(0,.05, .05, 0), y = c(0, 0, .05, .05), density = -1, col = "lightblue", border=0)
lines(c(0, 1), c(.05, .05), lty="dotted", col="red")
lines(c(.05, .05), c(0, 1), lty="dotted", col="red")
points(RPP2[, c(17, 34)], pch=20)

# zoomed plot
plot(NA, xlab="Original p value", ylab="Replication p value", pch=20, xlim=c(0, .06), ylim=c(0, 1))
polygon(c(0,.05, .05, 0), y = c(0, 0, .05, .05), density = -1, col = "lightblue", border=0)
lines(c(0, 1), c(.05, .05), lty="dotted", col="red")
lines(c(.05, .05), c(0, 1), lty="dotted", col="red")
points(RPP2[, c(17, 34)], pch=20)

round(addmargins(prop.table(table(RPP2$p_original <= .05, RPP2$p_replication <= .05, dnn=list("Original significant", "Replication significant"))))*100, 1)

addmargins(table(RPP2$p_original <= .05, RPP2$p_replication <= .05, dnn=list("Original p value", "Replication p value")))