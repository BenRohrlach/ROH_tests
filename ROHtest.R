pacman::p_load(tidyverse,
               Exact)

rohTest <- function(x,y,verbose=T){
  N1 <- length(x)
  N2 <- length(y)
  n1 <- sum(x>0)
  n2 <- sum(y>0)
  
  p01 <- (N1-n1)/N1
  p02 <- (N2-n2)/N2
  
  xp <- x[x>0]
  yp <- y[y>0]
  
  l1 <- 1/mean(xp)
  l2 <- 1/mean(yp)
  
  p0.test <- matrix(c(n1,N1-n1,n2,N2-n2),
                    nrow=2,
                    byrow=T) %>%
  Exact::exact.test(to.plot=F,
                    conf.int=T)
  
  p0.lwr <- p0.test$conf.int[1]
  p0.upr <- p0.test$conf.int[2]
  p0.p <- p0.test$p.value
  
  p1 <- sum(x!=0)/length(x)
  p2 <- sum(y!=0)/length(y)
  M1 <- p1*(1/l1)
  M2 <- p2*(1/l2)
  
  M.s1 <- sqrt((2*p1-p1^2)/(length(x)*l1^2))
  M.s2 <- sqrt((2*p2-p2^2)/(length(y)*l2^2))
  
  M.Z <- (M1-M2)/sqrt(M.s1^2+M.s2^2)
  M.p <- 2*pnorm(-abs(M.Z))
  
  if(verbose){
    # return results
    # P0
    cat(sprintf('Test for non-zero ROH (n1=%i, n2=%i):\n',n1,n2))
    cat(sprintf('Est (P0) group 1: %.4f\n',p01))
    cat(sprintf('Est (P0) group 2: %.4f\n',p02))
    cat(sprintf('95%% CI for odds ratio: (%.4f,%.4f)\n',p0.lwr,p0.upr))
    if(p0.p<0.0001){
      cat(sprintf('P0 p-value: %.4e\n\n',p0.p))
    }else{
      cat(sprintf('P0 p-value: %.4f\n\n',p0.p))
    }
    
    # Overall mean
    cat(sprintf('Test for mean ROH (n1=%i, n2=%i):\n',n1,n2))
    cat(sprintf('Est (ROH) group 1: %.4f\n',M1))
    cat(sprintf('Est (ROH) group 2: %.4f\n',M2))
    cat(sprintf('Z-score (ROH): %.2f\n',M.Z))
    if(M.p<0.0001){
      cat(sprintf('ROH p-value: %.4e\n\n',M.p))
    }else{
      cat(sprintf('ROH p-value: %.4f\n\n',M.p))
    }
  }
  
  return(list(p0=c(p01,p02),
              pvals=c(p0.p,M.p)))
}
