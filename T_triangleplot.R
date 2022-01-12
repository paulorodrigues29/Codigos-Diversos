source("T_simulacao_Exp.R")
alpha <- 5.35559
init <- 2
N <- 10
paths <- 2
lwd <- 1.5
transp <- 0.75
colA <- rgb(0.3,0.3,0.3,transp)
colB <- rgb(30/255,144/255,255/255)
run <- simulacao(init,init,init,init,init,init,alpha,N,5,plt=FALSE)
plot(run$b.x, run$b.y, type="l", xlim=c(0,1), ylim=c(0,sqrt(3)/2),axes=F, xlab="", ylab="", col=colA, lwd=lwd)
lines(run$a.x, run$a.y, col=colB, lwd=lwd);
segments(0,0,1/2,sqrt(3)/2, lwd=.75)
segments(1,0,1/2,sqrt(3)/2, lwd=.75)
segments(0,0,1,0, lwd=.75)
if (paths > 1) {
    for (n in 1:(paths-1)) {
        run <- simulacao(init,init,init,init,init,init,alpha,N,n+1,plt=FALSE);
        lines(run$b.x, run$b.y, col=colA, lwd=lwd);
        lines(run$a.x, run$a.y, col=colB, lwd=lwd);
    }
}
# equilibria
#----------------------------------------
e1 <- c(0,1/3,2/3)
x <- 0.5*(2*e1[2]+e1[3])
y <- sqrt(3)/2*e1[3]
points(x,y, pch=21, bg="white", cex=1.25)
#----------------------------------------
e2 <- c(0,2/3,1/3)
x <- 0.5*(2*e2[2]+e2[3])
y <- sqrt(3)/2*e2[3]
points(x,y, pch=21, bg="white", cex=1.25)
#----------------------------------------
e3 <- c(1/3,1/3,1/3)
x <- 0.5*(2*e3[2]+e3[3])
y <- sqrt(3)/2*e3[3]
points(x,y, pch=21, bg="white", cex=1.25)
#----------------------------------------
e4 <- c(1/3,2/3,0)
x <- 0.5*(2*e4[2]+e4[3])
y <- sqrt(3)/2*e4[3]
points(x,y, pch=21, bg="white", cex=1.25)
#----------------------------------------
e5 <- c(2/3,1/3,0)
x <- 0.5*(2*e5[2]+e5[3])
y <- sqrt(3)/2*e5[3]
points(x,y, pch=21, bg="white", cex=1.25)
#----------------------------------------
e6 <- c(1/3,0,2/3)
x <- 0.5*(2*e6[2]+e6[3])
y <- sqrt(3)/2*e6[3]
points(x,y, pch=21, bg="white", cex=1.25)
#----------------------------------------
e7 <- c(2/3,0,1/3)
x <- 0.5*(2*e7[2]+e7[3])
y <- sqrt(3)/2*e7[3]
points(x,y, pch=21, bg="white", cex=1.25)

