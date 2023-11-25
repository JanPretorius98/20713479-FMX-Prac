fee_converter <- function(stock_returns, annual_fee) {
  # Function to convert annual fee to monthly compounded fee
  monthly_fee_converter <- function(annual_fee) {
    (1 + annual_fee)^(1/12) - 1
  }
  
  # Calculate the monthly compounded fee
  monthly_fee = monthly_fee_converter(annual_fee)
  
  # Adjusting returns for the monthly compounded fee
  adjusted_returns = stock_returns - monthly_fee
  
  return(adjusted_returns)
}
