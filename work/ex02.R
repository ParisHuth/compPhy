# Paris J. Huth: Gruppe 1
# Q inich Pakal Figueroa Coc: Gruppe 5
# Introduction to computation physics ex. 2

# # initial values
# G <- 1
# M1 <- M2 <- 1
# r <- 1
# v <- 1
# t <- 1
# ep <- 1
# 
# # characteristic scale
# R <- r
# V <- sqrt(2/R)
# T <- R/V
# e <- (r*v^2-(r*v)*v)/(G*(M1+M2))-r/sqrt(sum(r^2))

# tutorial stuff
# tried to translate into r may need some serious troubeshooting
e_tot <- function(S,W){
    W2 <- sum(W*W)
    S2 <- sum(S*S)
    return(0.5*W2-1/sqrt(S2))
}

rl_vec <- function(S,W){
    W2 <- sum(W*W)
    S2 <- sum(S*S)
    SW <- sum(S*W)
    return(S*W2 -SW*W-S*1/sqrt(S2))
}

T_orbits <- function(S,W){
    a <- -1/e_tot(S,W)
    return(pi*a*sqrt(a/2))
}

retrieve_trajectories <- function(m1,m2,S,r0){
    Xi1 <- m2/(m2+m1)*S*r0*2
    Xi2 <- -m1/(m2+m1)*S*r0*2
    return(c(Xi1,Xi2))
}

accel <- function(sn){
    s_sq <- sum(sn*sn)
    s <- sqrt(s_sq)
    s3 <- s_sq*s
    return(-sn/s3)
}

twoEuler <- function(s0,w0,del_tau,n){
    S_out <- matrix(nrow = n,ncol=length(s0))
    W_out <- matrix(nrow = n,ncol=length(w0))
    S_out[1,] <- s0
    W_out[1,] <- w0
    S_np1 <- s0
    W_np1 <- w0
    for (n in 1:(n-1)){
        S_n <- S_np1
        W_n <- W_np1
        S_np1 <- S_n+W_n*del_tau
        W_np1 <- W_n+del_tau*accel(S_n)
        S_out[n+1,] <- S_np1
        W_out[n+1,] <- W_np1
    }
    return(list(S_out,W_out))
}


### next ex

m1 <- 1
m2 <- 1
r0 <- 1

s0 <- c(1,0,0)
w0 <- c(0,1,0)
delta_tau <- 0.01
T_orb <- T_orbits(s0,w0) ## TODO check for vector compadibility
T_orb

temp <- twoEuler(s0,w0,delta_tau,T_orb/delta_tau)
S_fE <- temp[[1]]
W_fE <- temp[[2]]

temp <- retrieve_trajectories(m1,m2,S_fE,r0)
X1 <- temp[1]
X2 <- temp[2]
X1
X2
