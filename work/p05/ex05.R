M <- matrix(1:9,ncol=3)
M[3] <- 0
M[7] <- 0
M
gaus <- function(M) {
    if(length(M)/ncol(M)!=ncol(M)){
        stop('matrix has to be square')
    }
    j <- ncol(M)
    
    # # to output together with inverted matrix include the following
    # dia <- diag(ncol(M))
    # M <- cbind(M,dia)
    for (i in 1:j) {
        M[i,] <- M[i,] / M[i,i]
        
        for (n in 1:(i - 1)) {
            if(i-1>0){
            M[n,] <- M[n,] - (M[n,i] / M[i,i]) * M[i,]
        }}

        for (n in j:(i + 1)) {
            if(i>=j){
                break
            }
            if (n <= j) {
                temp <- (M[n,i] / M[i,i]) * M[i,]
                M[n,] <- M[n,]-temp
                }
        }
    }
    
    return(M)
}

gaus(M)
M

backward_substitution <- function(U, b) {
    n <- nrow(U)  # Dimensions of U
    
    x <- rep(0, n)  # Initialize x with zeros
    
    x[n] <- b[n] / U[n, n]  # Last element of x
    
    for (i in (n - 1):1) {
        sum <- 0
        
        for (j in (i + 1):n) {
            sum <- sum + U[i, j] * x[j]
        }
        
        x[i] <- (b[i] - sum) / U[i, i]
    }
    
    return(x)
}

