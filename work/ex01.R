# Paris J. Huth: Gruppe 1
# Q inich Pakal Figueroa Coc: Gruppe 5

inte <- function(x,a,n){
    x^n/(x+a)
}
a <- 5
x <- seq(0,1,length.out=100)
n <- c(1,5,10,20,30,50)
y <- lapply(n, function(i) inte(x,a,i))

plot(x,y[[1]],type='l',ylab='y')
colors <- rainbow(length(n))
for (i in seq_along(n)){
    lines(x,y[[i]],col=colors[i])
}
legend('topleft',legend=n, col = colors,lty=1)
