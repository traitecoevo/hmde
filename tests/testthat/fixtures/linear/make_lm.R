lm_data <- data.frame(X = Loblolly$age,
                      Y = Loblolly$height,
                      N = nrow(Loblolly))

saveRDS(lm_data, file = "lm_data.rds")
