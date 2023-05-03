# Paris J. Huth: Gruppe 1
# Q inich Pakal Figueroa Coc: Gruppe 5
# Introduction to computation physics ex. 2

# Implementing euler scheme
euler <- function(s0, w0, h, Tr){
    for (i in 1:3){
        stopifnot('Only works with data in up to three dimensions.'=length(s0)<=3)
        stopifnot('Only works with data in up to three dimensions.'=length(w0)<=3)
        if (length(s0) < 3){
            s0 <- c(s0, 0)
        }
        if (length(w0) < 3){
            w0 <- c(w0, 0)
        }
        if (length(s0) >= 3 && length(w0) >= 3){
            break
        }
    }
    
    N <- Tr/h
    temp <- numeric(N)
    s <- data.frame(temp,temp,temp)
    w <- s
    s[1,] <- s0
    w[1,] <- w0
    
    for (i in 2:N){
        s[i,] <- s[i-1,]+w[i-1,]*h
        w[i,] <- w[i-1,]-(s[i-1,]/(sum(s[i-1,]^2)^(3/2)))*h
    }
    
    return(s)
}

s0 <- c(1,0,0)
w0 <- c(0,1,0)
s4 <- euler(s0,w0,1,10)
plot(s4[,1],s4[,2],col='green', type='l',ylim=c(-2.4,4.3))

s1 <- euler(s0,w0,0.001,10)
lines(s1[,1],s1[,2])

s2 <- euler(s0,w0,0.01,10)
lines(s2[,1],s2[,2],col='red')

s3 <- euler(s0,w0,0.1,10)
lines(s3[,1],s3[,2],col='blue')


 #different velocity
s0 <- c(1,0,0)
w0 <- c(0,0.6,0)
s4 <- euler(s0,w0,1,10)
plot(s4[,1],s4[,2],col='green',
    type='l',ylim=c(-9.95,1.43),
            xlim=c(-10.08,4.1))

s1 <- euler(s0,w0,0.001,10)
lines(s1[,1],s1[,2])

s2 <- euler(s0,w0,0.01,10)
lines(s2[,1],s2[,2],col='red')

s3 <- euler(s0,w0,0.1,10)
lines(s3[,1],s3[,2],col='blue')

s0 <- c(1,0,0)
w0 <- c(0,0.2,0)
s4 <- euler(s0,w0,1,10)
plot(s4[,1],s4[,2],col='green',
     type='l',ylim=c(-60.2,0.42),
     xlim=c(-12.31,21.3))

s1 <- euler(s0,w0,0.001,10)
lines(s1[,1],s1[,2])

s2 <- euler(s0,w0,0.01,10)
lines(s2[,1],s2[,2],col='red')

s3 <- euler(s0,w0,0.1,10)
lines(s3[,1],s3[,2],col='blue')

