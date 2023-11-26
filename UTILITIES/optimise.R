# Goal optimise portfolio

optim_foo <- function(Type = "mv", mu, Sigma, LB, UB, printmsg = TRUE){
  
  Safe_Optim <- purrr::safely(RiskPortfolios::optimalPortfolio)
  
  Opt_W <- 
    Safe_Optim(mu = mu, Sigma = Sigma, 
               control = list(type = Type, constraint = 'user', 
                              LB = rep(LB, ncol(Sigma)), 
                              UB = rep(UB, ncol(Sigma))))
  
  if( is.null(Opt_W$error)){
    
    optimw <- 
      tibble(Tickers = colnames(Sigma), weights = Opt_W$result) %>% 
      # Take note:
      rename(!!Type := weights)
    
    if(printmsg)   optimw <- optimw %>% mutate(Result = glue::glue("Converged: {Type}"))
    
  } else {
    
    optimw <- tibble(Tickers = colnames(Sigma), weights = 1/ncol(Sigma)) %>% 
      # Take note:
      rename(!!Type := weights)
    
    
    if(printmsg)   optimw <- optimw %>% mutate(Result = glue::glue("Failed to Converge: {Type}"))
    
  }
  optimw
}