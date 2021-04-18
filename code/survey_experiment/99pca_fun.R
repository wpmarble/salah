library(psych)
center <- function(x) (x - mean(x))/sd(x)

center_05sd <- function(x) (x - mean(x))/(2*sd(x))

PCA_extract <- function(vars, cor_type = "poly"){
  
  if(cor_type == "poly"){
    W <- polychoric(vars)$rho
  } else if(cor_type == "mix"){
    W <- mixedCor(vars)$rho
  } else if(cor_type == "pear"){
    W <- cor(vars)
  }
  
  pca <- principal(r = W, nfactors = ncol(W), rotate = "none")
  var <- apply(as.matrix(vars), 2, center)
  out <- center(var %*% pca$loadings[,1])
  
  return(list(pca, out))
}

