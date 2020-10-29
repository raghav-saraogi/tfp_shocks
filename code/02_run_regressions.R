# The function below computes the theta0 coefficients (coefficient for shock at t)
# for each dependent variable for all h periods. It takes as inputs a time series
# dataset with all variables required to run the regressions, including the shocks
# the type of shock and the number of horizons H.


# Compute Local Projections -----------------------------------------------

impulse_response <- function(data, shocks = 'ford', H = 20) {

# Sort data by date
data <- data %>% 
  arrange(date)

# Select which series of shocks to use
if (shocks == "ford") {
  
  data <- data %>% 
    mutate(shocks = ford_tfp)
  
} else {
  
  data <- data %>% 
    mutate(shocks = dtfp_util)
}

# Loop through each horizon h and run regressions for each dependent variable z of interest  
  for (h in 1:H) {

# z = GDP -----------------------------------------------------------------

  # Create dependent variable h periods in the future
  reg_data <- data %>% 
    mutate(y_h = lead(log_gdp_pc, order_by = date, n = h))

  # Run OLS regression
  test_fit  <- lm(
    formula = y_h ~ shocks + lag(shocks, n = 1) + lag(shocks, n = 2) + 
      lag(log_gdp_pc, n = 1) + 
      lag(log_productivity_pc, n = 1) +
      lag(log_stock_price_pc, n = 1),
    data = reg_data
  )
  
  # Get Newey-West adjusted standard errors 
  fit_df_h <- coeftest(test_fit, vcov = NeweyWest(test_fit, lag = h)) %>% 
    tidy() %>% 
    filter(term == "shocks") %>% 
    mutate(horizon = h,
           response = 'log real GDP per capita')
  
  # Compute confident intervals
  ci <- coefci(test_fit, vcov = NeweyWest(test_fit, lag = h), level = CI) %>% 
    tidy() %>% 
    rename(term = 1, low = 2, high = 3) %>% 
    filter(term == "shocks")
  
  fit_df_h <- fit_df_h %>% left_join(ci, by = "term")
  
  if (exists("final_df")) {

    final_df <- final_df %>% 
      bind_rows(fit_df_h)
        
  } else {
    
    final_df <- fit_df_h
    
  }

# z = Productivity --------------------------------------------------------

  # Create dependent variable h periods in the future
  reg_data <- data %>% 
    mutate(y_h = lead(log_productivity_pc, order_by = date, n = h))
  
  # Run OLS regression
  test_fit  <- lm(
    formula = y_h ~ shocks + lag(shocks, n = 1) + lag(shocks, n = 2) + 
      lag(log_gdp_pc, n = 1) + 
      lag(log_productivity_pc, n = 1) +
      lag(log_stock_price_pc, n = 1),
    data = reg_data
  )
  
  # Get Newey-West adjusted standard errors 
  fit_df_h <- coeftest(test_fit, vcov = NeweyWest(test_fit, lag = h)) %>% 
    tidy() %>% 
    filter(term == "shocks") %>% 
    mutate(horizon = h,
           response = 'log labor productivity')
  
  # Compute confident intervals
  ci <- coefci(test_fit, vcov = NeweyWest(test_fit, lag = h), level = CI) %>% 
    tidy() %>% 
    rename(term = 1, low = 2, high = 3) %>% 
    filter(term == "shocks")
  
  fit_df_h <- fit_df_h %>% left_join(ci, by = "term")
  
  final_df <- final_df %>% bind_rows(fit_df_h)
  
  
  # z = Stock Prices --------------------------------------------------------
  
  # Create dependent variable h periods in the future
  reg_data <- data %>% 
    mutate(y_h = lead(log_stock_price_pc, order_by = date, n = h))
  
  # Run OLS regression
  test_fit  <- lm(
    formula = y_h ~ shocks + lag(shocks, n = 1) + lag(shocks, n = 2) + 
      lag(log_gdp_pc, n = 1) + 
      lag(log_productivity_pc, n = 1) +
      lag(log_stock_price_pc, n = 1),
    data = reg_data
  )
  
  # Get Newey-West adjusted standard errors 
  fit_df_h <- coeftest(test_fit, vcov = NeweyWest(test_fit, lag = h),
                      ) %>% 
    tidy() %>% 
    filter(term == "shocks") %>% 
    mutate(horizon = h,
           response = 'log real stock prices per capita')
  
  # Compute confident intervals
  ci <- coefci(test_fit, vcov = NeweyWest(test_fit, lag = h), level = CI) %>% 
    tidy() %>% 
    rename(term = 1, low = 2, high = 3) %>% 
    filter(term == "shocks")
  
  fit_df_h <- fit_df_h %>% left_join(ci, by = "term")
  
  final_df <- final_df %>% bind_rows(fit_df_h)
  

# Consumption -------------------------------------------------------------

  # Create dependent variable h periods in the future
  reg_data <- data %>% 
    mutate(y_h = lead(log_consumption_pc, order_by = date, n = h))
  
  # Run OLS regression
  test_fit  <- lm(
    formula = y_h ~ shocks + lag(shocks, n = 1) + lag(shocks, n = 2) + 
      lag(log_gdp_pc, n = 1) + 
      lag(log_productivity_pc, n = 1) +
      lag(log_stock_price_pc, n = 1) + 
      lag(log_consumption_pc, n = 1),
    data = reg_data
  )
  
  # Get Newey-West adjusted standard errors 
  fit_df_h <- coeftest(test_fit, vcov = NeweyWest(test_fit, lag = h)) %>% 
    tidy() %>% 
    filter(term == "shocks") %>% 
    mutate(horizon = h,
           response = 'log real consumption per capita')
  
  # Compute confident intervals
  ci <- coefci(test_fit, vcov = NeweyWest(test_fit, lag = h), level = CI) %>% 
    tidy() %>% 
    rename(term = 1, low = 2, high = 3) %>% 
    filter(term == "shocks")
  
  fit_df_h <- fit_df_h %>% left_join(ci, by = "term")
  
  final_df <- final_df %>% bind_rows(fit_df_h)
  

# z = Investment ----------------------------------------------------------

  # Create dependent variable h periods in the future
  reg_data <- data %>% 
    mutate(y_h = lead(log_investment_pc, order_by = date, n = h))
  
  # Run OLS regression
  test_fit  <- lm(
    formula = y_h ~ shocks + lag(shocks, n = 1) + lag(shocks, n = 2) + 
      lag(log_gdp_pc, n = 1) + 
      lag(log_productivity_pc, n = 1) +
      lag(log_stock_price_pc, n = 1) + 
      lag(log_investment_pc, n = 1),
    data = reg_data
  )
  
  # Get Newey-West adjusted standard errors 
  fit_df_h <- coeftest(test_fit, vcov = NeweyWest(test_fit, lag = h)) %>% 
    tidy() %>% 
    filter(term == "shocks") %>% 
    mutate(horizon = h,
           response = 'log nonresidential investment per capita')
  
  # Compute confident intervals
  ci <- coefci(test_fit, vcov = NeweyWest(test_fit, lag = h), level = CI) %>% 
    tidy() %>% 
    rename(term = 1, low = 2, high = 3) %>% 
    filter(term == "shocks")
  
  fit_df_h <- fit_df_h %>% left_join(ci, by = "term")
  
  final_df <- final_df %>% bind_rows(fit_df_h)
  
  }

  
  final_df <- final_df %>% 
    mutate(shocks = shocks)
  
  return(final_df)
  
}
