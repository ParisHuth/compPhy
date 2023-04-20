# Paris J. Huth: Gruppe 1
# Q inich Pakal Figueroa Coc: Gruppe 5


# Numerical Integration
## a

# defining the integrand as a function
inte <- function(x,a,n){
    x^n/(x+a)
}

# assigning variables
a <- 5
x <- seq(0,1,length.out=100)
n <- c(1,5,10,20,30,50)

# computing the integrand
yn <- lapply(n, function(i) inte(x,a,i))
y <- sapply(yn, function(i) sum(i))

# plotting every configuration
plot(x,yn[[1]],type='l',ylab='y')
colors <- rainbow(length(n))
for (i in seq_along(n)){
    lines(x,yn[[i]],col=colors[i])
}
legend('topleft',legend=n, col = colors,lty=1)

## b

iter <- function(a, n0, y0, n1){
    # strictly going from m->n doesnt need a case by case
    m <- min(n0,n1)
    n <- max(n0,n1)
    
    # error handling
    # check for trivial case
    if (n == m){
        warning('Boundaries match! Returning initial y value.')
        return(y0)
    } else if (m == 0){
        warning('A boundary was set to 0! This would have resulted in an infite',
                ' result. Replacing boundary with 1.')
        m <- 1
    }
    
    
    
    # initialising
    y <- y0
    # iterating
    for (i in m:n){
        y <- (1/i - a*y)
    }
    
    
    # returning finished result
    return(y)
}

## c

iter(a=5, n0=0, y0=log((1+5)/5),n1=0)
iter(a=5, n0=50, y0=log((1+5)/5),n1=30)
# with increasing n the factor 1/n gets progressively smaller, while not equal to
# zero, meaning that each subsequent iteration increases the absolute value.
# convergence is dependent on the choice of the a
