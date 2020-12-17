identite <- function(coeff,hat,s){ #test : si la différence est inférieure à un seuil fixé, on considère hat_i bien estimé
  diff <- abs(coeff - hat) #différence en valeur absolue des matrices
  compt <- 0
  seuil <- s
  for(i in 1:dim(diff)[1]){
    if(diff[i]<=seuil){
      compt = compt +1
    }
  }
  return(compt)
}

simulation <- function(sd,coef,tirage,s){

  eps <- matrix(rnorm(tirage, 0,sd)) #matrice des n epsilons

  #initialisation des valeurs X1,...,Xn
  X <- matrix(0, ncol=length(coef), nrow=tirage)

  for(i in 1:length(coef)){
    X[,i] <- runif(tirage)
  }

  #initialisation des valeurs Y
  Y <- (X %*% coef) + eps

  #estimation des coefficients
  hat <- solve(t(X)%*%X)%*%t(X)%*%Y

  #estimation du pourcentage de coefficients considérés comme bien estimés
  pourcentage <- identite(coef,hat,s)/length(coef) #diff,coef et hat ont le même nombre de lignes

  #estimation du pourcentage de coefficients dont l'ordre a été conservé
  ordre <- mean(order(hat) == order(coef))

  return(list(pourcent=pourcentage,order=ordre))
}


resultat_simu <- function(simulations){
  #simulations
  M <- data.frame(matrix(unlist(simulations), ncol = 2, byrow = T))
  colnames(M) <- c("pourcent", "order")
  print(M)

  pourcentage_moyen <-mean(M$pourcent)
  #pourcentage_moyen
  ordre_moyen <- mean(M$order)

  plot(M$pourcent, type="p", col="blue",xlab="tirage",ylab="pourcentage",main="Pourcentage de paramètres bien estimées en fonction des tirages",ylim=c(0,1.2))
  par(new=TRUE)
  abline(h=pourcentage_moyen, col="blue")
  legend("topleft","moyenne",col="blue",lwd=1)


  plot(M$order, type="p", col="red",xlab="tirage",ylab="pourcentage",main="Pourcentage de paramètres bien ordonnés en fonction des tirages",ylim=c(0,1.2))
  par(new=TRUE)
  abline(h=ordre_moyen, col="red")
  legend("topleft","moyenne",col="red",lwd=1)
}

histog_simu <- function(sd,coef,tirage){

  eps <- matrix(rnorm(tirage, 0,sd)) #matrice des n epsilons

  #initialisation des valeurs X1,...,Xn
  X <- matrix(0, ncol=length(coef), nrow=tirage)

  for(i in 1:length(coef)){
    X[,i] <- runif(tirage)
  }

  #initialisation des valeurs Y
  Y <- (X %*% coef) + eps

  #estimation des coefficients
  hat <- solve(t(X)%*%X)%*%t(X)%*%Y

  return(data.frame(hat))
}
