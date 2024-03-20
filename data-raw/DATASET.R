## code to prepare `DATASET` dataset goes here

true_single <- seq(from=1, by=2.5, length.out=7)
set.seed(2024)
y_single <- true_single + true_single*rnorm(n=7, 0, 0.02) + rnorm(n=7, 0, 0.2)


true_multi <- c(seq(from=1, by=2.5, length.out=7),
                seq(from=2, by=2, length.out=7))
set.seed(2024)
y_multi <- true_multi + true_multi*rnorm(n=14, 0, 0.02) + rnorm(n=14, 0, 0.2)

usethis::use_data(y_single, y_multi, overwrite = TRUE, internal = TRUE)
