set.seed(1)
mydf <- data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100))

save(mydf, file = "saveddf.RData")

unlink("saveddf.RData")
