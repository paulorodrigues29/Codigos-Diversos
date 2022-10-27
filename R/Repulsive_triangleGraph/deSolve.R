# -- 3 vertex complete graph --
## reinforcement strength
alpha <- 20
parameters <- c(alpha)
## steps
times <- seq(0, 100, by = 0.06)
## the model
trglRepODE <- function(t, state, parameters) {
    with(as.list(c(state, parameters)),{
        dx1<- -x1 + 1/3*(exp(-alpha*y1)/(exp(-alpha*y1)+exp(-alpha*y2))
            + exp(-alpha*y1)/(exp(-alpha*y1)+exp(-alpha*y3)))
        dx2<- -x2 + 1/3*(exp(-alpha*y2)/(exp(-alpha*y2)+exp(-alpha*y1))
            + exp(-alpha*y2)/(exp(-alpha*y2)+exp(-alpha*y3)))
        dx3<- -x3 + 1/3*(exp(-alpha*y3)/(exp(-alpha*y3)+exp(-alpha*y1))
            + exp(-alpha*y3)/(exp(-alpha*y3)+exp(-alpha*y2)))
        dy1<- -y1 + 1/3*(exp(-alpha*x1)/(exp(-alpha*x1)+exp(-alpha*x2))
            + exp(-alpha*x1)/(exp(-alpha*x1)+exp(-alpha*x3)))
        dy2<- -y2 + 1/3*(exp(-alpha*x2)/(exp(-alpha*x2)+exp(-alpha*x1))
            + exp(-alpha*x2)/(exp(-alpha*x2)+exp(-alpha*x3)))
        dy3<- -y3 + 1/3*(exp(-alpha*x3)/(exp(-alpha*x3)+exp(-alpha*x1))
            + exp(-alpha*x3)/(exp(-alpha*x3)+exp(-alpha*x2)))        
        list(c(dx1, dx2, dx3, dy1, dy2, dy3))
    })
}

addpathX <- function(state=c(x1=1/3,x2=1/3,x3=1/3,
                            y1=1/3,y2=1/3,y3=1/3),
                    col = rgb(0.7,0.7,0.7),
                    lwd=2,
                    arr.length=0.35,
                    ars=108,
                    are=109) {
    # solve the ODE
    out <- ode(y = state, times = times, func = trglRepODE,
               parms = parameters)
    # transform to ternary coordinates
    x.x <- 0.5*(2*out[,"x2"]+out[,"x3"])
    x.y <- (sqrt(3)/2)*out[,"x3"]
    y.x <- 0.5*(2*out[,"y2"]+out[,"y3"])
    y.y <- (sqrt(3)/2)*out[,"y3"]
    # plot
    lines(x.x, x.y,col=col, lwd=lwd)
    Arrows(x.x[ars], x.y[ars], x.x[are], x.y[are], col=col,
           type = "curved", arr.length = arr.length, segment = FALSE)
}

addpathY <- function(state=c(x1=1/3,x2=1/3,x3=1/3,
                            y1=1/3,y2=1/3,y3=1/3),
                    col = rgb(0.7,0.7,0.7),
                    lwd=2,
                    arr.length=0.35,
                    ars=8,
                    are=9) {
    # solve the ODE
    out <- ode(y = state, times = times, func = trglRepODE,
               parms = parameters)
    # transform to ternary coordinates
    x.x <- 0.5*(2*out[,"x2"]+out[,"x3"])
    x.y <- (sqrt(3)/2)*out[,"x3"]
    y.x <- 0.5*(2*out[,"y2"]+out[,"y3"])
    y.y <- (sqrt(3)/2)*out[,"y3"]
    # plot
    lines(y.x, y.y,col=col, lwd=lwd)
    Arrows(y.x[ars], y.y[ars], y.x[are], y.y[are], col=col,
           type = "curved", arr.length = arr.length, segment = FALSE)
}



library(deSolve)
library(shape)
##par(mfrow=c(1,2), mar = c(1, 1, 1, 1))
plot(0, 0, xlim=c(0,1), ylim=c(0,sqrt(3)/2),axes=F,xlab="",ylab="",cex=0) 

##-------------------------- N-W ----------------------------------------
addpathX(state=c(x1=0.4,x2=0.3-0.02,x3=0.3+0.02,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0.35,x2=0.3,x3=0.35,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0.3+0.02,x2=0.3,x3=0.4-0.02,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0.45,x2=0,x3=0.55,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0.55,x2=0,x3=0.45,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
##-------------------------- N-E  ---------------------------------------
addpathX(state=c(x1=0.3,x2=0.35,x3=0.35,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0.3-0.02,x2=0.4,x3=0.3+0.02,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0.3,x2=0.3+0.02,x3=0.4-0.02,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0,x2=0.55,x3=0.45,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
##--------------------------  S  ----------------------------------------
addpathX(state=c(x1=0.35,x2=0.35,x3=0.3,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0.4,x2=0.3+0.02,x3=0.3-0.02,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0.3+0.02,x2=0.4,x3=0.3-0.02,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0.55,x2=0.45,x3=0.0,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0.45,x2=0.55,x3=0.0,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0.87,x2=0.13,x3=0.0,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0.87,x2=0.0,x3=0.13,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
##------------------ coming from SE vertex ------------------------------
addpathX(state=c(x1=0.05,x2=0.95,x3=0,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0,x2=0.95,x3=0.05,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0.13,x2=0.87,x3=0.0,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0.0,x2=0.87,x3=0.13,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
##------------------ coming from SW vertex ------------------------------
addpathX(state=c(x1=0.95,x2=0.05,x3=0,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0.95,x2=0,x3=0.05,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
##------------------ coming from N vertex -------------------------------
addpathX(state=c(x1=0,x2=0.05,x3=0.95,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0.05,x2=0,x3=0.95,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0.0,x2=0.13,x3=0.87,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0.13,x2=0.0,x3=0.87,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0,x2=0.45,x3=0.55,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
## draw the border of the simplex
segments(0,0,1/2,sqrt(3)/2, lwd=1.2)
segments(1,0,1/2,sqrt(3)/2, lwd=1.2)
segments(0,0,1,0, lwd=1.2)
###------------------ add simulations -----------------------------------
seed1 <- 951
seed2 <- -1
seed3 <- 26 # 79
source("simulacao_Exp_v2.R")
s <- simulacao(13,13,13,13,13,13,alpha,i=10000,seed=seed1,plt=FALSE) # 5:13
lines(s$b.x,s$b.y, lwd=2.5, col="black") 
s <- simulacao(13,13,13,13,13,13,alpha,i=10000,seed=seed2,plt=FALSE)
lines(s$b.x,s$b.y, lwd=2.5, col="blue")
s <- simulacao(13,13,13,13,13,13,alpha,i=10000,seed=seed3,plt=FALSE) # 3
lines(s$b.x,s$b.y, lwd=2.5, col="red")

## equilibria
library(rootSolve)
eta <- function(x) 1/(1+exp((x*alpha)/3.0)) + 1/(1+exp((2.0*x*alpha)/3.0)) + x - 1
All <- uniroot.all(eta, c(-1, 1))
print(All)
# All == 1 will be the center, otherwhise we get the other points
if (length(All) == 1) {
    x3 <- 1/3-All/3;  # see eqn. (3.2)
    x2 <- 1/3;
    x <- 0.5*(2*x2+x3)
    y <- sqrt(3)/2*x3
    points(x,y, pch=21, bg="yellow", cex=1.1, lwd=1.3)
} else {
    for (i in c(1,2,3)) {
        x3 <- 1/3-All[i]/3; # see eqn. (3.2)
        x2 <- 1/3;
        x <- 0.5*(2*x2+x3)
        y <- sqrt(3)/2*x3
        aa <- 0.5*(2*x3+x2)
        bb <- sqrt(3)/2*x2
        x3 <- 1/3+All[i]/3
	x2 <- 1/3-All[i]/3;
	aaa <- 0.5*(2*x2+x3)
	bbb <- sqrt(3)/2*x3
        points(x,y, pch=21, bg="yellow", cex=1.1, lwd=1.3)
        points(aa,bb, pch=21, bg="yellow", cex=1.1, lwd=1.3)
        points(aaa,bbb, pch=21, bg="yellow", cex=1.1, lwd=1.3)
    }
}


rm(list=ls())
