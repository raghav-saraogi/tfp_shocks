# This script uses the function written in script #2 and given paramters entered
# in the master script to plot impulse responses for each macroeconomic variable.
# The output is a single plot for each type of TFP shock, where each macro variable
# is a subplot (facet).

# Get a data frame with responses using Ford TFP shocks
ford_results <- impulse_response(merged_data, shocks = "ford", H = H)

# plot results
ford_results %>% 
  ggplot(aes(x = horizon, y = estimate)) +
  geom_line(aes(group = response)) + 
  geom_ribbon(aes(ymin = low ,ymax = high), alpha = 0.3) +
  facet_wrap(~response, scales = "free_y") +
  theme_bw() +
  labs(x = "h", y = "Impulse Response")

ggsave("./output/ford_shock_results.png", width = 14, height = 10)

# Get a data frame with responses using Fernald TFP shocks
fernald_results <- impulse_response(merged_data, shocks = "fernald", H = H)

# plot results
fernald_results %>% 
  ggplot(aes(x = horizon, y = estimate)) +
  geom_line(aes(group = response)) + 
  geom_ribbon(aes(ymin = low ,ymax = high), alpha = 0.3) +
  facet_wrap(~response, scales = "free_y") +
  theme_bw() +
  labs(x = "h", y = "Impulse Response")

ggsave("./output/fernald_shock_results.png", width = 14, height = 10)
