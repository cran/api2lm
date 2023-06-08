set.seed(10)
library(KingCountyHouses)
data(home_prices)
home_prices$waterfront <- factor(home_prices$waterfront,
                                 labels = c("no", "yes"))

# select subset
keep1 <- c(12778, 15871, 4025, 7253, 21051)
keep2 <- sample(which(home_prices$waterfront == "yes"), 10)
remain <- setdiff(seq_len(nrow(home_prices)),
                  c(keep1, keep2))
keep3 <- unlist(tapply(remain, home_prices$condition[remain], sample, size = 5))
remain <- setdiff(remain, keep3)
keep <- c(keep1, keep2, keep3)
keep4 <- sample(remain, 216 - length(keep))
keep <- c(keep, keep4)
keep <- sample(keep)
home_sales <- as.data.frame(home_prices[keep, c(2:8, 10)])
row.names(home_sales) <- seq_len(nrow(home_sales))
save(home_sales, file = "home_sales.rda", compress = TRUE)


