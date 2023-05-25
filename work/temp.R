is.tridia <- function(A) {
    n <- nrow(A)

    for (i in 1:(n-1)) {
        if (A[i+1, i] == 0 || A[i, i+1] == 0){
            return(FALSE)}
    }
    
    return(TRUE)
}

tridiagaus <- function(A, b) {
    n <- length(b)
    
    # Check if A is a tridiagonal matrix
    if (!is.tridia(A))
        stop("Matrix A is not tridiagonal.")
    
    # Forward elimination
    for (i in 2:n) {
        m <- A[i, i-1] / A[i-1, i-1]
        A[i, i] <- A[i, i] - m * A[i-1, i]
        b[i] <- b[i] - m * b[i-1]
    }
    
    # Back substitution
    x <- numeric(n)
    x[n] <- b[n] / A[n, n]
    
    for (i in (n-1):1) {
        x[i] <- (b[i] - A[i, i+1] * x[i+1]) / A[i, i]
    }
    
    return(x)
}




# Example usage
A <- matrix(c(2, -1, 0, -1, 2, -1, 0, -1, 2), nrow = 3, ncol = 3)
b <- c(1, 2, 3)

solution <- tridiagaus(A, b)
print(solution)

# b

is.uppertri <- function(A) {
    n <- nrow(A)
    
    for (i in 2:n) {
        if (any(A[i, 1:(i-1)] != 0))
            return(FALSE)
    }
    
    return(TRUE)
}

backsub <- function(A, b) {
    n <- length(b)
    
    # Check if A is an upper triangular matrix
    if (!is.uppertri(A))
        stop("Matrix A is not upper triangular.")
    
    x <- numeric(n)
    x[n] <- b[n] / A[n, n]
    
    for (i in (n-1):1) {
        x[i] <- (b[i] - sum(A[i, (i+1):n] * x[(i+1):n])) / A[i, i]
    }
    
    return(x)
}



# Example usage
A <- matrix(c(2, 0, 0, -1, 2, 0, 0, -1, 2), nrow = 3, ncol = 3)
b <- c(1, 2, 3)

solution <- backsub(A, b)
print(solution)

# c

tridiagSolver <- function(a, b, c, y) {
    n <- length(y)
    x <- numeric(n)
    
    # Check if the lengths of input vectors are valid
    if (length(a) != (n-1) || length(b) != n || length(c) != (n-1)) {
        stop("Invalid input vectors.")
    }
    
    # Forward elimination
    for (i in 2:n) {
        m <- a[i-1] / b[i-1]
        b[i] <- b[i] - m * c[i-1]
        y[i] <- y[i] - m * y[i-1]
    }
    
    # Backward substitution
    x[n] <- y[n] / b[n]
    
    for (i in (n-1):1) {
        x[i] <- (y[i] - c[i] * x[i+1]) / b[i]
    }
    
    return(x)
}


# d

N <- 10
a <- rep(-1, N-1)
b <- rep(3/2, N)
c <- rep(-1, N-1)
y <- rep(1/10, N)

solution <- tridiagSolver(a, b, c, y)
print(solution)

# e 

# Original matrix equation:
# (3/2)x[i] - x[i-1] - x[i+1] = 1/10 for i = 1 to N

solution <- tridiagSolver(a, b, c, y)

# Calculate the deviation from the original right-hand side
deviation <- b * solution - c(rep(0, N-1), solution[-N]) - a * solution[-1] - y
print(deviation)


