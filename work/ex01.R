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
y <- lapply(n, function(i) inte(x,a,i))

# plotting every configuration
plot(x,y[[1]],type='l',ylab='y')
colors <- rainbow(length(n))
for (i in seq_along(n)){
    lines(x,y[[i]],col=colors[i])
}
legend('topleft',legend=n, col = colors,lty=1)

## b