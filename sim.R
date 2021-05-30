set.seed(123)

sc1 = function(){
  x1 = rpois(100, lambda = 1)
  # x1 = rnorm(100, mean = 1.1, sd = 1)
  y1 = rnorm(100, mean = 2, sd = 2)
  scenario1 = c(x1, y1)
  return(t.test(x = x1, y = y1, paired = FALSE, var.equal = FALSE)$statistic)
}

sc2 = function(){
  x2 = rnorm(100, mean = 10, sd = 1)
  y2 = rnorm(100, mean = 11, sd = 2)
  scenario2 = c(x2, y2)
  return(t.test(x = x2, y = y2, paired = FALSE, var.equal = FALSE)$statistic)
}

collection1 = replicate(1000, expr = sc1())
hist(collection1)
# mean(collection1 <= 0.05)
collection2 = replicate(1000, expr = sc2())
hist(collection2)
# mean(collection2 <= 0.05)
plot(sort(abs(collection1)), sort(abs(collection2)))
abline(a = 0, b = 1, col = "red")
