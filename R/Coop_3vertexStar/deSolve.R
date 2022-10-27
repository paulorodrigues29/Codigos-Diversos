## -- 3 vertex star graph ---
## reinforcement strength 
alpha <- 12 # try 8, 9, ...
parameters <- c(alpha) # 2, 2.7, 5, 15, 
## steps
times <- seq(0, 30, by = 0.1)
## the model: the ODE is defined via the
## field that arises in the three vertex star graph
## this particular version corresponds to the repulsive
## case.
trglRepODE <- function(t, state, parameters) {
    with(as.list(c(state, parameters)),{
        dx1<- -x1 + 1/2*exp(alpha*y1)/(exp(alpha*y1)+exp(alpha*y2))
        dx2<- -x2 + 1/2*(exp(alpha*y2)/(exp(alpha*y2)+exp(alpha*y1))
            + exp(alpha*y2)/(exp(alpha*y2)+exp(alpha*y3)))
        dx3<- -x3 + 1/2*exp(alpha*y3)/(exp(alpha*y3)+exp(alpha*y2))
        dy1<- -y1 + 1/2*exp(alpha*x1)/(exp(alpha*x1)+exp(alpha*x2))
        dy2<- -y2 + 1/2*(exp(alpha*x2)/(exp(alpha*x2)+exp(alpha*x1))
            + exp(alpha*x2)/(exp(alpha*x2)+exp(alpha*x3)))
        dy3<- -y3 + 1/2*exp(alpha*x3)/(exp(alpha*x3)+exp(alpha*x2))
        list(c(dx1, dx2, dx3, dy1, dy2, dy3))
    })
}

addpathX <- function(state=c(x1=1/3,x2=1/3,x3=1/3,
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
    lines(x.x, x.y,col=col, lwd=lwd)
    Arrows(x.x[ars], x.y[ars], x.x[are], x.y[are], col=col,
           type = "curved", arr.length = arr.length, segment = FALSE)
}

addpathY <- function(state=c(x1=1/3,x2=1/3,x3=1/3,
                             y1=1/3,y2=1/3,y3=1/3),
                    col = rgb(0.7,0.7,0.7),
                    lwd=2,
                    arr.length=0.25,
                    ars=25,
                    are=26) {
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
#addpathX(state=c(x1=0,x2=0.2,x3=0.8,y1=1/3,y2=1/3,y3=1/3), col="blue")
addpathX(state=c(x1=0,x2=0.3,x3=0.7,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0,x2=0.45,x3=0.55,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0,x2=0.6,x3=0.4,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0,x2=0.8,x3=0.2,y1=1/3,y2=1/3,y3=1/3))
##-------------------------- N-E  ---------------------------------------
#addpathX(state=c(x1=0.8,x2=0.2,x3=0,y1=1/3,y2=1/3,y3=1/3),col="red")
addpathX(state=c(x1=0.7,x2=0.3,x3=0,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0.55,x2=0.45,x3=0,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0.4,x2=0.6,x3=0,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0.2,x2=0.8,x3=0,y1=1/3,y2=1/3,y3=1/3))
addpathX(state=c(x1=0.0,x2=1,x3=0,y1=1/3,y2=1/3,y3=1/3))
##------------------ coming from SW vertex ------------------------------
addpathX(state=c(x1=0.87,x2=0.13,x3=0,y1=1/3,y2=1/3,y3=1/3),ars=2,are=3)
addpathX(state=c(x1=0.9,x2=0.0,x3=0.1,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0.7,x2=0.0,x3=0.3,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0.5,x2=0.0,x3=0.5,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0.3,x2=0.0,x3=0.7,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
##------------------ coming from N vertex -------------------------------
addpathX(state=c(x1=0.0,x2=0.13,x3=0.87,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
addpathX(state=c(x1=0.1,x2=0,x3=0.9,y1=1/3,y2=1/3,y3=1/3),ars=1,are=2)
##------------------ add simulations -----------------------------------
source("simulacaolinha_Exp.R")
#s <- simulacao(20,13,1,25,25,25,alpha,i=5000,seed=6,plt=FALSE)
s <- simulacao(20,10,1,25,25,25,alpha,i=5000,seed=77,plt=FALSE)
#s <- simulacao(4,4,4,4,4,4,alpha,i=5000,seed=77,plt=FALSE)
lines(s$b.x,s$b.y, lwd=2.75, col="black")
# used to be seed 22 bellow
s <- simulacao(1,14,20,25,25,25,alpha,i=5000,seed=23,plt=FALSE)
#s <- simulacao(4,4,4,4,4,4,alpha,i=5000,seed=23,plt=FALSE)
lines(s$b.x,s$b.y, lwd=2.75, col="blue")
s <- simulacao(21,1,10,25,25,25,alpha,i=5000,seed=21,plt=FALSE) # 20,1,5
lines(s$b.x,s$b.y, lwd=2.5, col="red")

# draw the border of the simplex
segments(0,0,1/2,sqrt(3)/2, lwd=1.7)
segments(1,0,1/2,sqrt(3)/2, lwd=1.7)
segments(0,0,1,0, lwd=1.7)

##---------- calculos para os equilibrios ------------------------------
## os equilibrios sao determinados pela solucao a equacao (3.18)
## da dissertacao; isto fornece o valor de x, logo as coordenadas dos
## possiveis equilibrios sao ((x, 1-2x, x), (x, 1-2x, x)) i.e. tipo
## (d2).

library(rootSolve)
eq318 <- function (x) 1/(2+2*exp(alpha*(1-3*x))) - x;
All <- uniroot.all(eq318, c(0, 1/2))
if (length(All) == 1) {
    x3 <- All;
    x2 <- 1 - 2*All;
    x <- 0.5*(2*x2+x3)
    y <- sqrt(3)/2*x3
    points(x,y, pch=21, bg="yellow", cex=1.1, lwd=1.3)
} else {
    for (i in c(1,2,3)) {
        x3 <- All[i];
        x2 <- 1 - 2*All[i];
        x <- 0.5*(2*x2+x3)
        y <- sqrt(3)/2*x3
        points(x,y, pch=21, bg="yellow", cex=1.1, lwd=1.3)
    }
}

rm(list=ls())
