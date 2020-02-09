library(rmutil)

par(mfrow = c(1, 2))

Ox <- seq(-100, 100, 1)
plot(Ox, dlaplace(Ox, 10, 7),
     main="LaPlace Distribution - density",
     ylab="dlaplace(x)",
     type="l",
     col="blue")

plot(Ox, plaplace(Ox, 10, 7),
     main="LaPlace Distribution - cumulative distribution",
     ylab="dlaplace(x)",
     type="l",
     col="blue")