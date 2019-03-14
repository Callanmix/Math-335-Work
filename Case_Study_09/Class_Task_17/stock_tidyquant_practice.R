pacman::p_load(tidyquant, timetk, dygraphs)

# Stocks used ->
# ABIOMED, Inc.\ Advanced Micro Devices\ TripAdvisor, Inc.\ Fortinet, Inc.\ Advance Auto Parts, Inc.\ Chipotle Mexican Grill, Inc.

name <- c("ABIOMED, Inc.","Advanced Micro Devices","TripAdvisor, Inc.","Fortinet, Inc.","Advance Auto Parts, Inc.","Chipotle Mexican Grill, Inc.")

tickers_today <- c("ABMD","AMD","TRIP","FTNT","AAP","CMG")

tick <- tidyquant::tq_get(tickers_today, get = "stock.price", from = "2018-10-01") 

tick_advanced <- tidyquant::tq_get(tickers_today, get = "key.ratios", from = "2018-10-01") %>% unnest()

#########################################
ts_tick <- tk_xts(tick, start = "2014-03-07", date_var = date, select = adjusted)


dygraph(ts_tick) %>% 
  dyRangeSelector() %>% 
  dyRebase(value = 333.3)






###########################################

c("FTNT","AAP","CMG") %>% tq_get(get = "stock.price", from = "2018-10-01") %>% 
  group_by(symbol) %>% 
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               col_rename = "daily.returns") %>% 
  ungroup() %>% group_by(date) %>% 
  summarise(mean = mean(daily.returns)) %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_line(color = "red") +
  geom_line(data = c("ABMD","AMD","TRIP") %>% tq_get(get = "stock.price", from = "2018-10-01") %>% 
              group_by(symbol) %>% 
              tq_transmute(select     = adjusted, 
                           mutate_fun = periodReturn, 
                           period     = "daily", 
                           col_rename = "daily.returns") %>% 
              ungroup() %>% group_by(date) %>% 
              summarise(mean = mean(daily.returns)), aes(x = date, y = mean), color = "blue")


###########################################
tick %>% 
  group_by(symbol) %>% 
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "daily.returns") %>% 
  ggplot(aes(x = date, y = daily.returns, fill = symbol)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "My stocks: Monthly Returns",
       y = "Returns", x = "", color = "") +
  facet_wrap(~symbol) +
  scale_y_continuous(labels = scales::percent) +
  theme_tq() +
  scale_fill_tq()


#######################################
ticker <- tick %>% 
  group_by(symbol) %>% 
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               col_rename = "Ra")

tick2 <- "XLK" %>% 
  tq_get(get = "stock.price", from = "2018-10-01") %>% 
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               col_rename = "Rb")

base <- left_join(ticker,tick2,by = "date")

base %>% 
  group_by(symbol) %>% 
  summarise(mean = mean(Ra))

base %>% 
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)


base %>%
  tq_transmute_xy(x = Ra, 
                y = Rb,
                mutate_fun = runCor,
                n = 6,
                col_rename = "rolling.corr.6") %>% 
  ggplot(aes(x = date, y = rolling.corr.6, color = symbol)) +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(size = 1) +
  labs(title = "My Stocks: Rolling Correlation to XLK",
       x = "", y = "Correlation", color = "") +
  facet_wrap(~ symbol, ncol = 2) +
  theme_tq() + 
  scale_color_tq()  


######################################################
tick %>%
  ggplot(aes(x = date, color = symbol)) +
  geom_segment(aes(xend = date, y = low, yend = high),
               size = 1) +
  geom_point(aes(y = high), size = 2) +
  geom_point(aes(y = low), size = 2) +
  facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
  labs(title = "My Stocks: Min/Max Price By Day",
       y = "Stock Price", color = "") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank())
  
#######################################

tick %>%     
  group_by(symbol) %>%
  tq_transmute(adjusted, periodReturn, period = "daily") %>%
  tq_transmute(daily.returns, Return.clean, alpha = 0.05) %>%
  tq_transmute(daily.returns, Return.excess, Rf = 0.03 / 252) %>% 
  ggplot(aes(x = date, y = `daily.returns > Rf`, color = symbol)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(facets = "symbol")

