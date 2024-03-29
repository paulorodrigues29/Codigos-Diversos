simulacao = function(bv1=1,bv2=1,bv3=1,av1=1,av2=1,av3=1,alfa=1,i=500,seed=1,plt=TRUE, plty="l", transp=1, lwd=1){
  #criando vetor do tempo para graficar
  t = 1
  tempo = 1:i
  set.seed(seed)
  #criando vetor das propor��es em cada v�rtice para graficar
  vb1 = rep(0, length(tempo))
  vb2 = rep(0, length(tempo))
  vb3 = rep(0, length(tempo))
  va1 = rep(0, length(tempo))
  va2 = rep(0, length(tempo))
  va3 = rep(0, length(tempo))
  
  while(t<=i){
      ##vari�veis auxiliares das bolas adicionadas em cada rodada
      auxb1 = 0
      auxb2 = 0
      auxb3 = 0
      auxa1 = 0
      auxa2 = 0
      auxa3 = 0
      ##definindo as propor��es
      vb1[t] = bv1/(bv1+bv2+bv3)
      vb2[t] = bv2/(bv1+bv2+bv3)
      vb3[t] = bv3/(bv1+bv2+bv3)
      va1[t] = av1/(av1+av2+av3)
      va2[t] = av2/(av1+av2+av3)
      va3[t] = av3/(av1+av2+av3)
      ##Para o elo 1-2    
      ##Probabilidade das brancas
      pb1 = exp(-alfa*va1[t])/(exp(-alfa*va1[t])+exp(-alfa*va2[t]))
      pb2 = 1-pb1
      ##Probabilidade das azuis
      pa1 = exp(-alfa*vb1[t])/(exp(-alfa*vb1[t])+exp(-alfa*vb2[t]))
      pa2 = 1-pa1
      ##gerando um valor aleat�rio entre 0 e 1 para as bolas brancas
      valor = runif(1,0,1)
      ##Verificando para qual v�rtice as bolas brancas irao
      if(valor <= pb1){
          auxb1 = auxb1 + 1
      }
      if(valor > pb1){
          auxb2  = auxb2 + 1
      }
      ##gerando um valor aleat�rio entre 0 e 1 para as bolas brancas
      valor = runif(1,0,1)
      ##Verificando para qual v�rtice as bolas azuis irao
      if(valor <= pa1){
          auxa1 = auxa1 + 1
      }
      if(valor > pa1){
          auxa2 = auxa2 + 1
      }
      ##Para o elo 1-3
      ##Probabilidade das brancas
      pb2 = exp(-alfa*va2[t])/(exp(-alfa*va2[t])+exp(-alfa*va3[t]))
      pb3 = 1-pb2
      ##Probabilidade das azuis
      pa2 = exp(-alfa*vb2[t])/(exp(-alfa*vb2[t])+exp(-alfa*vb3[t]))
      pa3 = 1-pa2    
      ##gerando um valor aleat�rio entre 0 e 1 para as bolas brancas
      valor = runif(1,0,1)
      ##Verificando para qual v�rtice as bolas brancas irao
      if(valor <= pb2){
          auxb2 = auxb2 + 1
      }
      if(valor > pb2){
          auxb3 = auxb3 + 1
      }
      ##gerando um valor aleat�rio entre 0 e 1 para as bolas brancas
      valor = runif(1,0,1)
      ##Verificando para qual v�rtice as bolas azuis irao
      if(valor <= pa2){
          auxa2 = auxa2 + 1
      }
      if(valor > pa2){
          auxa3 = auxa3 + 1
      }
      ##Para o elo 1-3
      ##Probabilidade das brancas
      pb1 = exp(-alfa*va1[t])/(exp(-alfa*va1[t])+exp(-alfa*va3[t]))
      pb3 = 1-pb1
      ##Probabilidade das azuis
      pa1 = exp(-alfa*vb1[t])/(exp(-alfa*vb1[t])+exp(-alfa*vb3[t]))
      pa3 = 1-pa1
      ##gerando um valor aleat�rio entre 0 e 1 para as bolas brancas
      valor = runif(1,0,1)
      ##Verificando para qual v�rtice as bolas brancas irao
      if(valor <= pb1){
          auxb1 = auxb1 + 1
      }
      if(valor > pb1){
          auxb3 = auxb3 + 1
      }
      ##gerando um valor aleat�rio entre 0 e 1 para as bolas brancas
      valor = runif(1,0,1)
      ##Verificando para qual v�rtice as bolas azuis irao
      if(valor <= pa1){
          auxa1 = auxa1 + 1
      }
      if(valor > pa1){
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
        ## grafico dos caminhos & tempo:
        if (plty != "t") {
            par(mfrow = c(1,2))
            plot(0,1/3, xlim=c(1,i), ylim= c(0,2/3+0.01), type = "l", xlab="n",
                 ylab="", xaxt="n", yaxt="n")
            axis(1, at = c(0, i/2, i))
            axis(2, at = c(0, 1/3, 2/3), labels = c("0", "1/3", "2/3"))
            abline(h=c(1/3, 2/3), lty=2)  
            title("brancas",xlab="n", ylab="x(n)")
            lines(vb1, col = rgb(165,15,21,max=255), lwd=2)
            lines(vb2, col = rgb(239,59,44,max=255), lwd=2)
            lines(vb3, col = rgb(252,187,161,max=255), lwd=2)
            ##legend("topright", legend=c("v 1", "v 2", "v 3"),
            ##       col=c(rgb(165,15,21,max=255), rgb(239,59,44,max=255),
            ##             rgb(252,187,161,max=255)), lty=1:1.2, cex=0.8)
            ## bolas azuis 
            plot(0,1/3, xlim=c(1,i), ylim= c(0,2/3+0.01), type = "l", xlab="n",
                 ylab="", xaxt="n", yaxt="n")
            axis(1, at = c(0, i/2, i))
            axis(2, at = c(0, 1/3, 2/3), labels = c("0", "1/3", "2/3")) 
            abline(h=c(2/3,1/3), lty=2)  
            title("azuis",xlab="n",
                  ylab="y(n)")
            lines(va1, col = rgb(165,15,21,max=255), lwd=2)
            lines(va2, col = rgb(239,59,44,max=255), lwd=2)
            lines(va3, col = rgb(252,187,161,max=255), lwd=2)
            ##legend("center", legend=c("v 1", "v 2", "v 3"),
            ##       col=c(rgb(165,15,21,max=255), rgb(239,59,44,max=255),
            ##             rgb(252,187,161,max=255)), lty=1:1.2, cex=0.8)
        }
        ## grafico dos caminhos no espaco de fase (mesmo 2-implex para ambas cores)
        if (plty != "l") {
            colA <- "black"
            colB <- "#d73027"
            par(mfrow=c(1,1))
            plot(b.x, b.y, type="l", xlim=c(0,1), ylim=c(0,sqrt(3)/2),
                 axes=F, xlab="", ylab="", col=colA, lwd=1.75)
            lines(a.x, a.y, col=colB, lwd=1.75);
            segments(0,0,1/2,sqrt(3)/2, lwd=1)
            segments(1,0,1/2,sqrt(3)/2, lwd=1)
            segments(0,0,1,0, lwd=1)
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

