iris
View(iris)
?par

plot(iris$Sepal.Length, iris$Sepal.Width,
     type = "p",
     main = "iris data",
     xlab = "Sepal Length",
     ylab = "Sepal Width",
     pch = 20,
     col = "red")

f1 <- function(n){exp(n)}
f1(2)
?plot

plot(f1,1,5,
     type = "l",
     lty = 2,
     lwd = 2,
     col = "blue")

x <- seq(1,5,by=0.01)
y <- x^3

lines(x,y,lwd=2)

legend("topleft", legend = c("exp()", "x^3"),
       col = c("blue","black"),
       lty = c(2,1),
       lwd = 2)

legend("top", legend = c("exp()", "x^3"),
       fill = c("blue","black"))

set.seed(1997)
x <- runif(100,0,10)

hist(x)
hist(x, breaks = seq(0,10,0.5), col = "green")

hist(x,plot=FALSE)$counts






