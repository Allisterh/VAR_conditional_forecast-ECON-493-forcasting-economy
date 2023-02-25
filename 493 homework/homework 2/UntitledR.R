data <- data.frame(X = c(1, 2, 3, 4, 5, 6, 7), Y = c(12, 10, 8, 6, 4, 2, 0))
model1 <- lm(X ~ Y , data = data)
summary(model1)

wu <-function(x) {-3*x + 40}
curve(wu)
