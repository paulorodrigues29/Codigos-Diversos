# -- 3 vertex complete graph --
## reinforcement strength
alpha <- 9.8
parameters <- c(alpha)
## steps
times <- seq(0, 150, by = 0.1)
## the model
trglRepODE <- function(t, state, parameters) {
    with(as.list(c(state, parameters)),{
        dx1<- -x1 + 1/3*(exp(alpha*y1)/(exp(alpha*y1)+exp(alpha*y2))
            + exp(alpha*y1)/(exp(alpha*y1)+exp(alpha*y3)))
        dx2<- -x2 + 1/3*(exp(-alpha*y2)/(exp(-alpha*y2)+exp(-alpha*y1))
            + exp(-alpha*y2)/(exp(-alpha*y2)+exp(-alpha*y3)))
        dx3<- -x3 + 1/3*(exp(-alpha*y3)/(exp(-alpha*y3)+exp(-alpha*y1))
            + exp(-alpha*y3)/(exp(-alpha*y3)+exp(-alpha*y2)))
        dy1<- -y1 + 1/3*(exp(alpha*x1)/(exp(alpha*x1)+exp(alpha*x2))
            + exp(alpha*x1)/(exp(alpha*x1)+exp(alpha*x3)))
        dy2<- -y2 + 1/3*(exp(-alpha*x2)/(exp(-alpha*x2)+exp(-alpha*x1))
            + exp(-alpha*x2)/(exp(-alpha*x2)+exp(-alpha*x3)))
        dy3<- -y3 + 1/3*(exp(alpha*x3)/(exp(alpha*x3)+exp(alpha*x1))
            + exp(alpha*x3)/(exp(alpha*x3)+exp(alpha*x2)))        
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

plot(0, 0, xlim=c(0,1), ylim=c(0,sqrt(3)/2),axes=F,xlab="",ylab="",cex=0) 

##------------------------------------------------------------------
addpathX(state=c(x1=1/3,x2=1/3-0.001,x3=1/3+0.001,y1=1/3,y2=1/3,y3=1/3),ars=364,are=365,col="black", lwd=2)
addpathX(state=c(x1=0.2,x2=0.8,x3=0.0,y1=1/3,y2=1/3,y3=1/3),ars=10,are=11, lwd=2)
addpathX(state=c(x1=0.3,x2=0.0,x3=0.7,y1=1/3,y2=1/3,y3=1/3),ars=10,are=11, lwd=2)

#addpathX(state=c(x1=0.0,x2=0.1,x3=0.6,y1=0,y2=1,y3=0),ars=10,are=11)

## draw the border of the simplex
segments(0,0,1/2,sqrt(3)/2, lwd=1.2)
segments(1,0,1/2,sqrt(3)/2, lwd=1.2)
segments(0,0,1,0, lwd=1.2)

rm(list=ls())
