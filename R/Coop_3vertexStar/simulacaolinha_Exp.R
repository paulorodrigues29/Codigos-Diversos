simulacao = function(bv1=1,bv2=1,bv3=1,av1=1,av2=1,av3=1,alfa=1,i=500,seed=1,plt=TRUE, plty="l", transp=1, lwd=1){
  #criando vetor do tempo para graficar
  t = 1
  tempo = 1:i
  set.seed(seed)
  #criando vetor das proporções em cada vértice para graficar
  vb1 = rep(0, length(tempo))
  vb2 = rep(0, length(tempo))
  vb3 = rep(0, length(tempo))
  va1 = rep(0, length(tempo))
  va2 = rep(0, length(tempo))
  va3 = rep(0, length(tempo))
  
  while(t<=i){
      ##variáveis auxiliares das bolas adicionadas em cada rodada
      auxb1 = 0
      auxb2 = 0
      auxb3 = 0
      auxa1 = 0
      auxa2 = 0
      auxa3 = 0
      ##definindo as proporções
      vb1[t] = bv1/(bv1+bv2+bv3)
      vb2[t] = bv2/(bv1+bv2+bv3)
      vb3[t] = bv3/(bv1+bv2+bv3)
      va1[t] = av1/(av1+av2+av3)
      va2[t] = av2/(av1+av2+av3)
      va3[t] = av3/(av1+av2+av3)
      ##Para o elo 1-2    
      ##Probabilidade das brancas
      pb1 = exp(alfa*va1[t])/(exp(alfa*va1[t])+exp(alfa*va2[t]))
      pb2 = 1-pb1
      ##Probabilidade das azuis
      pa1 = exp(alfa*vb1[t])/(exp(alfa*vb1[t])+exp(alfa*vb2[t]))
      pa2 = 1-pa1
      ##gerando um valor aleatório entre 0 e 1 para as bolas brancas
      valor = runif(1,0,1)
      ##Verificando para qual vértice as bolas brancas irao
      if(valor <= pb1){
          auxb1 = auxb1 + 1
      }
      if(valor > pb1){
          auxb2  = auxb2 + 1
      }
      ##gerando um valor aleatório entre 0 e 1 para as bolas brancas
      valor = runif(1,0,1)
      ##Verificando para qual vértice as bolas azuis irao
      if(valor <= pa1){
          auxa1 = auxa1 + 1
      }
      if(valor > pa1){
          auxa2 = auxa2 + 1
      }
      ##Para o elo 2-3
      ##Probabilidade das brancas
      pb2 = exp(alfa*va2[t])/(exp(alfa*va2[t])+exp(alfa*va3[t]))
      pb3 = 1-pb2
      ##Probabilidade das azuis
      pa2 = exp(alfa*vb2[t])/(exp(alfa*vb2[t])+exp(alfa*vb3[t]))
      pa3 = 1-pa2    
      ##gerando um valor aleatório entre 0 e 1 para as bolas brancas
      valor = runif(1,0,1)
      ##Verificando para qual vértice as bolas brancas irao
      if(valor <= pb2){
          auxb2 = auxb2 + 1
      }
      if(valor > pb2){
          auxb3 = auxb3 + 1
      }
      ##gerando um valor aleatório entre 0 e 1 para as bolas brancas
      valor = runif(1,0,1)
      ##Verificando para qual vértice as bolas azuis irao
      if(valor <= pa2){
          auxa2 = auxa2 + 1
      }
      if(valor > pa2){
          auxa3 = auxa3 + 1
      }
      
      ##
      bv1 = bv1 + auxb1
      bv2 = bv2 + auxb2
      bv3 = bv3 + auxb3
      av1 = av1 + auxa1
      av2 = av2 + auxa2
      av3 = av3 + auxa3 
      t = t+1
  }
    propb1 = bv1/(bv1+bv2+bv3)
    propb2 = bv2/(bv1+bv2+bv3)
    propb3 = bv3/(bv1+bv2+bv3)
    propa1 = av1/(av1+av2+av3)
    propa2 = av2/(av1+av2+av3)
    propa3 = av3/(av1+av2+av3)
    ## ternary coordinates
    b.x <- c()
    b.y <- c()
    a.x <- c()
    a.y <- c()
    k <- sqrt(3)/2
    for (n in 1:length(vb2)) {  
        x <- 0.5*(2*vb2[n]+vb3[n])
        b.x <- append(b.x, x)
        y <- k*vb3[n]
        b.y <- append(b.y, y)
        x <- 0.5*(2*va2[n]+va3[n])
        a.x <- append(a.x, x)
        y <- k*va3[n]
        a.y <- append(a.y, y)
    }

    if (plt !=0) {
        if (plty != "t") {
            par(mfrow = c(1,2))
            plot(0,1/3, xlim=c(1,i), ylim= c(0,1), type = "l", xlab="n",
                 ylab="X(n)")
            abline(h=c(1/4,1/2), lty=2)  
            title("brancas",xlab="n", ylab="X(n)")
            lines(vb1, col = "red", lwd=2)
            lines(vb2, col = "blue", lwd=2)
            lines(vb3, col = "black", lwd=2)
            legend("topright", legend=c("v 1", "v 2", "v 3"),
                   col=c("red", "blue", "black"), lty=1:1.2, cex=0.8)
            ## bolas azuis 
            plot(0,1/3, xlim=c(1,i), ylim= c(0,1), type = "l", xlab="n",
                 ylab="X(n)")
            abline(h=c(1/4,1/2), lty=2)  
            title("azuis",xlab="n",
                  ylab="X(n)")
            lines(va1, col = "red", lwd=2)
            lines(va2, col = "blue", lwd=2)
            lines(va3, col = "black", lwd=2)
            legend("topright", legend=c("v 1", "v 2", "v 3"),
                   col=c("red", "blue", "black"), lty=1:1.2, cex=0.8)
        }
        if (plty != "l") {
            colA <- rgb(0.3,0.3,0.3,transp)
            colB <- rgb(30/255,144/255,255/255)
            par(mfrow=c(1,1))
            plot(b.x, b.y, type="l", xlim=c(0,1), ylim=c(0,sqrt(3)/2),
                 axes=F, xlab="", ylab="", col=colA, lwd=lwd)
            lines(a.x, a.y, col=colB, lwd=lwd);
            segments(0,0,1/2,sqrt(3)/2, lwd=.75)
            segments(1,0,1/2,sqrt(3)/2, lwd=.75)
            segments(0,0,1,0, lwd=.75)
            ## equilibria
            ##----------------------------------------
            e1 <- c(0,1/3,2/3)
            x <- 0.5*(2*e1[2]+e1[3])
            y <- sqrt(3)/2*e1[3]
            points(x,y, pch=21, bg="white", cex=1.25)
            ##----------------------------------------
            e2 <- c(0,2/3,1/3)
            x <- 0.5*(2*e2[2]+e2[3])
            y <- sqrt(3)/2*e2[3]
            points(x,y, pch=21, bg="white", cex=1.25)
            ##----------------------------------------
            e3 <- c(1/3,1/3,1/3)
            x <- 0.5*(2*e3[2]+e3[3])
            y <- sqrt(3)/2*e3[3]
            points(x,y, pch=21, bg="white", cex=1.25)
            ##----------------------------------------
            e4 <- c(1/3,2/3,0)
            x <- 0.5*(2*e4[2]+e4[3])
            y <- sqrt(3)/2*e4[3]
            points(x,y, pch=21, bg="white", cex=1.25)
            ##----------------------------------------
            e5 <- c(2/3,1/3,0)
            x <- 0.5*(2*e5[2]+e5[3])
            y <- sqrt(3)/2*e5[3]
            points(x,y, pch=21, bg="white", cex=1.25)
            ##----------------------------------------
            e6 <- c(1/3,0,2/3)
            x <- 0.5*(2*e6[2]+e6[3])
            y <- sqrt(3)/2*e6[3]
            points(x,y, pch=21, bg="white", cex=1.25)
            ##----------------------------------------
            e7 <- c(2/3,0,1/3)
            x <- 0.5*(2*e7[2]+e7[3])
            y <- sqrt(3)/2*e7[3]
            points(x,y, pch=21, bg="white", cex=1.25)
        }
    } else {
        return(data.frame(b.x,b.y,a.x,a.y))
    }
}

