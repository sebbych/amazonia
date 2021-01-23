#modified just to get the weights

###################################################################################
# Compute Weighted OLS regression parameters for the Improved Doubly-Robust DID estimator with Panel Data


wols.br.panel2 <- function(deltaY, D, int.cov, pscore, i.weights){
  #-----------------------------------------------------------------------------
  
  i.weights <- as.vector(i.weights * pscore/(1 - pscore))
  
  #Run weighted OLS
  beta.cal <- stats::coef(stats::lm(deltaY ~ -1 + int.cov,
                                    subset = D==0,
                                    weights = i.weights))
  
  #get fitted values
  out.delta <-  as.numeric(tcrossprod(beta.cal, int.cov))
  
  # return fitted values
  return(list(wvoc = int.cov,
              ww = i.weights))
  
}
