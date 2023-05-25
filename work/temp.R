tridiagonalGaussianElimination <- function(A, b) {
    n <- nrow(A)
    
    # Check if the dimensions of matrix A and vector b are valid
    if (ncol(A) != n || length(b) != n) {
        stop("Invalid input dimensions.")
    }
    
    # Forward elimination
    for (i in 2:n) {
        m <- A[i, i-1] / A[i-1, i-1]
        A[i, i] <- A[i, i] - m * A[i-1, i]
        b[i] <- b[i] - m * b[i-1]
    }
    
    return(list(A = A, b = b))
}

# Example usage
A <- matrix(c(2, -1, 0, -1, 2, -1, 0, -1, 2), nrow = 3, ncol = 3)
b <- c(1, 2, 3)

result <- tridiagonalGaussianElimination(A, b)
A_updated <- result$A
b_updated <- result$b

# Print the updated matrix A and vector b
print(A_updated)
print(b_updated)
A
