# Paris J. Huth: Gruppe 1
# Q inich Pakal Figueroa Coc: Gruppe 5
# Introduction to computation physics ex. 3

# 4th Order Runge-Kutta Method
# Define the RK4 step function
rk4_step <- function(y0, x0, f, h, ...) {
    k1 <- h * f(y0, x0, ...)
    k2 <- h * f(y0 + k1/2., x0 + h/2., ...)
    k3 <- h * f(y0 + k2/2., x0 + h/2., ...)
    k4 <- h * f(y0 + k3, x0 + h, ...)
    xp1 <- x0 + h
    yp1 <- y0 + 1./6.*(k1 + 2.*k2 + 2.*k3 + k4)
    return(list(yp1, xp1))
}

# Define the RK4 function
rk4 <- function(y0, x0, f, h, n, ...) {
    yn <- matrix(0, n+1, length(y0))
    xn <- numeric(n+1)
    yn[1,] <- y0
    xn[1] <- x0
    for (i in 1:n) {
        y_out <- rk4_step(y0 = yn[i,], x0 = xn[i], f = f, h = h, ...)
        yn[i+1,] <- y_out[[1]]
        xn[i+1] <- y_out[[2]]
    }
    return(list(yn, xn))
}

# Define the exponential function
exponential <- function(x, t, r) {
    return(-r * x)
}

# Set the initial conditions and parameters
y0 <- c(1)
x0 <- 0
r <- 1

# Loop over different step sizes and plot the results
par(mfrow=c(1,1))
for (i in c(0.01, 0.1, 0.5, 1, 2)) {
    result <- rk4(y0 = y0, x0 = x0, f = exponential, h = i, n = floor(10/i), r = r)
    plot(result[[2]], result[[1]], type = "l", log = "y", ylim = c(1e-6, 10), xlab = "x", ylab = "y")
}
x <- seq(0, 10, length.out = 100)
lines(x, exp(-r * x), col = "red")

#-----------------------------------------------------------------------------------------

# Three body problem

m1 <- 1
m2 <- 1
m3 <- 1
G <- 1

y0 <- c(0.97000436, -0.24308753, -0.46620368, -0.43236573, 0, 0, 0.93240737, 0.86473146, -0.97000436, 0.24308753, -0.46620368, -0.43236573)
t0 <- 0

h <- function(x1, x2, y1, y2, z1, z2, m1, m2, m3) {
    x <- -1 * G * (m2 * (x1 - y1) / ((x1 - y1)^2 + (x2 - y2)^2)^(3/2) + m3 * (x1 - z1) / ((x1 - z1)^2 + (x2 - z2)^2)^(3/2))
    y <- -1 * G * (m2 * (x2 - y2) / ((x1 - y1)^2 + (x2 - y2)^2)^(3/2) + m3 * (x2 - z2) / ((x1 - z1)^2 + (x2 - z2)^2)^(3/2))
    return(c(x, y))
}

f <- function(x, t) {
    result <- numeric(12)
    # Körper 1
    result[1] <- x[3] # x1'
    result[2] <- x[4] # x2'
    # x1''
    result[3] <- h(x[1], x[2], x[5], x[6], x[9], x[10], m1, m2, m3)[1]
    # x2''
    result[4] <- h(x[1], x[2], x[5], x[6], x[9], x[10], m1, m2, m3)[2]
    # Körper 2
    result[5] <- x[7]
    result[6] <- x[8]
    result[7] <- h(x[5], x[6], x[1], x[2], x[9], x[10], m2, m1, m3)[1]
    result[8] <- h(x[5], x[6], x[1], x[2], x[9], x[10], m2, m1, m3)[2]
    # Körper 3
    result[9] <- x[11]
    result[10] <- x[12]
    result[11] <- h(x[9], x[10], x[1], x[2], x[5], x[6], m3, m1, m2)[1]
    result[12] <- h(x[9], x[10], x[1], x[2], x[5], x[6], m3, m1, m2)[2]
    return(result)
}
# seperate plots
par(mfrow=c(2,2))
result <- rk4(y0, t0, f, 0.001, 5000)
print(result[[1]])
# plot the trajectories of the first mass
plot(result[[1]][,1], result[[1]][,2], type="l", xlab="x", ylab="y", main="Trajectories of the three masses")
# plot the trajectories of the second mass
plot(result[[1]][,5], result[[1]][,6], col="red")
# plot the trajectories of the third mass
plot(result[[1]][,9], result[[1]][,10], col="blue")

# combined plot
par(mfrow=c(1,1))
plot(result[[1]][,1], result[[1]][,2], type="l", xlab="x", ylab="y", main="Trajectories of the three masses")
# plot the trajectories of the second mass
lines(result[[1]][,5], result[[1]][,6], col="red")
# plot the trajectories of the third mass
lines(result[[1]][,9], result[[1]][,10], col="blue")

# b

m1 <- 5
m2 <- 4
m3 <- 3
M <- m1+m2+m3

x1 <- c(0,0)
x2 <- c(0,3)
x3 <- c(4,0)
o <- (x1*m1+x2*m2+x3*m3)/M

x1 <- x1 - o
x2 <- x2 - o
x3 <- x3 - o


y0 <- c(x1,0,0,x2,0,0,x3,0,0)
result <- rk4(y0, t0, f, 0.009999, 10000)

cat("o = ", o, "\n")
cat("y0 = ", y0, "\n")

plot(result[[1]][,1],result[[1]][,2], type="l", xlab="x", ylab="y", col="blue", main="Three Body Problem", xlim=c(-5,5), ylim=c(-5,5))
points(result[[1]][,5],result[[1]][,6], type="l", xlab="x", ylab="y", col="red")
points(result[[1]][,9],result[[1]][,10], type="l", xlab="x", ylab="y", col="green")
legend("topleft", c("m1", "m2", "m3"), col=c("blue", "red", "green"), lty=1, cex=0.8)

