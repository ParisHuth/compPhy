# Paris J. Huth: Gruppe 1
# Q inich Pakal Figueroa Coc: Gruppe 5
# Introduction to computation physics ex. 3
# 4th Order Runge-Kutta Method
rk4s <- function(y,x,f,h){
    k1 <- h*f(y,x)
    k2 <- h*f(y+k1/2,x+h/2)
    k3 <- h*f(y+k2/2,x+h/2)
    k4 <- h*f(y+k3,x+h)
    
    xn1 <- x+h
    yn1 <- y+h/6*(k1+2*k2+2*k3+k4)
    list(yn1,xn1)
}

rk4 <- function(y,x,f,h,n){
    yn <- matrix(0, nrow = n+1, ncol = length(y))
    yn[1,] <- y
    xn <- numeric(n+1)
    xn[1] <- x
    for (i in 2:(n+1)) {
        temp <- rk4s(yn[n-1,],xn[n-1],f,h)
        yn[n,] <- temp[[1]]
        xn[n] <- temp[[2]]
        
    }
    list(yn,xn)
}
r <- 1
y0 <- c(1)
x0 <- 0
exponential <- function(x,t){
    return(-1*x)
}
for (i in c(0.01,0.1,0.5,1,2)) {
    result <- rk4(y0,x0,exponential,i,as.numeric(10/i))
    
}


# Define the values of i to iterate over
i_values <- c(0.01, 0.1, 0.5, 1, 2)

# Initialize the plot
plot(NA, xlim = c(x0, 10), ylim = c(0, 5), xlab = "x", ylab = "y")

# Loop over the values of i and plot the results
for (i in i_values) {
    result <- rk4(y0, x0, exponential, i, as.integer(10/i))
    lines(result[[2]], result[[1]], type = "l", lty = 1, col = "black", lwd = 1, 
          main = paste0("RK4 with i = ", i))
}

# Add a legend and set the y-axis to a log scale
legend("topleft", legend = as.character(i_values), lty = 1, col = "black", lwd = 1, 
       title = "i values", cex = 0.8)
axis(2, log = "y")

#-----------------------------------------------------------------------------------------

# Three body problem
