

# Load FORD TFP Shock data
ford_shocks <- read_csv("./input/ford_tfp.csv")

# Clean ford TFP shock data to remove observations with missing values
ford_shocks <- ford_shocks %>% 
  filter(!is.na(ford_tfp))

# Format dates
ford_shocks <- ford_shocks %>% 
  mutate(day = 1,
         qtr_string = substr(as.character(quarter), 6, 7),
         year = as.numeric(substr(as.character(quarter), 1, 4)),
         month = case_when(
           qtr_string == '' ~ 1,
           qtr_string == '25' ~ 4,
           qtr_string == '5' ~ 7,
           qtr_string == '75' ~ 10,
         ),
         date = make_date(year, month, day)) %>% 
  select(date, ford_tfp)

# Load fernald TFP shock data
fernald_shocks <- read_excel("./input/quarterly_tfp.xlsx",
                            sheet = "quarterly",
                            skip  = 1)

# Select only dtfp_util variable from fernald shock file
fernald_shocks <- fernald_shocks %>% 
  select(date, dtfp_util)

fernald_shocks <- fernald_shocks %>% 
  mutate(day = 1,
         qtr_string = substr(as.character(date), 7, 7),
         year = as.numeric(substr(as.character(date), 1, 4)),
         month = case_when(
           qtr_string == '1' ~ 1,
           qtr_string == '2' ~ 4,
           qtr_string == '3' ~ 7,
           qtr_string == '4' ~ 10,
         ),
         date = make_date(year, month, day)) %>% 
  select(date, dtfp_util)

# Filter out missing dates
fernald_shocks <- fernald_shocks %>% 
  filter(!is.na(date))


# Load Shiller stock price data
stock_prices <- read_excel("./input/ie_data.xls",
                           sheet = "Data",
                           skip = 7)

# Select S&P Comp. P stock price variable and rename variable to stock_price 
stock_prices <- stock_prices %>%
  select(Date, stock_price = P)

# Formate dates
stock_prices <- stock_prices %>% 
  mutate(day = 1,
         month_string = if_else(substr(as.character(Date), 6, 7) == '01',
                                1, if_else(substr(as.character(Date), 6, 7) == '1',
                                           10, as.numeric(substr(as.character(Date), 6, 7)))),
         year = as.numeric(substr(as.character(Date), 1, 4)),
         qtr_month = case_when(
           month_string <= 3 ~ 1,
           month_string <= 6 & month_string > 3 ~ 4,
           month_string <= 9 & month_string > 6 ~ 7,
           month_string >= 10 ~ 10,
         ),
         date = make_date(year, qtr_month, day)) %>% 
  select(date, stock_price)

# Convert to quarterly values by takings means
stock_prices <- stock_prices %>% 
  group_by(date) %>% 
  summarize(stock_price = mean(stock_price, na.rm = T)) %>% 
  ungroup()


# Download Fred Data ------------------------------------------------------

# Set FRED API Key
fredr_set_key(FRED_KEY)

# load population data
population <- fredr_series_observations(
  series_id = FRED_POPULATION,
  frequency = 'q'
) %>% 
  select(-series_id) %>% 
  rename(population = value)

gdp <- fredr_series_observations(
  series_id = FRED_GDP,
  frequency = 'q'
) %>% 
  select(-series_id) %>% 
  rename(gdp = value)


deflator <- fredr_series_observations(
  series_id = FRED_DEFLATOR,
  frequency = 'q'
) %>% 
  select(-series_id) %>% 
  rename(deflator = value)

hours <- fredr_series_observations(
  series_id = FRED_HOURS,
  frequency = 'q'
) %>% 
  select(-series_id) %>% 
  rename(hours = value)

consumption <- fredr_series_observations(
  series_id = FRED_CONSUMPTION,
  frequency = 'q'
) %>% 
  select(-series_id) %>% 
  rename(consumption = value)

investment <- fredr_series_observations(
  series_id = FRED_INVESTMENT,
  frequency = 'q'
) %>% 
  select(-series_id)  %>% 
  rename(investment = value)


# Merge Datasets ----------------------------------------------------------

merged_data <- ford_shocks %>% 
  left_join(fernald_shocks, by = 'date') %>% 
  left_join(stock_prices, by = 'date') %>% 
  left_join(gdp, by = 'date') %>% 
  left_join(consumption, by = 'date') %>% 
  left_join(deflator, by = 'date') %>% 
  left_join(population, by = 'date') %>%
  left_join(hours, by = 'date') %>% 
  left_join(investment, by = 'date')

# drop observations with missing data
merged_data <- merged_data %>% drop_na()
  
# Create Adjusted Variables -----------------------------------------------

# Calculate log-real per capita values for gdp, stock prices, consumption
# and investment
merged_data <- merged_data %>% 
  mutate(
    log_gdp_pc = log((gdp / population) / deflator) * 100,
    log_consumption_pc = log((consumption / population) / deflator) * 100,
    log_stock_price_pc = log((stock_price / population) / deflator) * 100,
    log_investment_pc = log((investment / population) / deflator) * 100
  )

# Compute log hours per capita and log real labor productivity
merged_data <- merged_data %>% 
  mutate(
    log_hours_pc = log(hours / population) * 100,
    log_productivity_pc= log((gdp / hours) / deflator) * 100
  )

save(merged_data, file = "./intermediate/merged_data.rda")
