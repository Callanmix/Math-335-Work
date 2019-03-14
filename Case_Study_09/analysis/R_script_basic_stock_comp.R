pacman::p_load(tidyquant, timetk, dygraphs)

## List of Stocks that you would like to use
## c("Ford","Amazon","Coca Cola","Tesla","Disney","Twitter","Microsoft","Sony","Walmart")
## List of tickers for those stocks
## c("F","AMZN","KO","TSLA","DIS","TWTR","MSFT","SNE","WMT")

tickers_used <- c("F","AMZN","KO","TSLA","DIS","TWTR","MSFT","SNE","WMT")

start_date <- "2014-03-07"

tick_for_ggplot <- tq_get(tickers_used, from = start_date, get = "stock.price")

int_stock <- cbind(tk_xts(tq_get("F", from = start_date, get = "stock.price"),start = start_date, date_var = date, select = adjusted),
          tk_xts(tq_get("AMZN", from = start_date, get = "stock.price"),start = start_date, date_var = date, select = adjusted),
          tk_xts(tq_get("KO", from = start_date, get = "stock.price"),start = start_date, date_var = date, select = adjusted),
          tk_xts(tq_get("TSLA", from = start_date, get = "stock.price"),start = start_date, date_var = date, select = adjusted),
          tk_xts(tq_get("DIS", from = start_date, get = "stock.price"),start = start_date, date_var = date, select = adjusted),
          tk_xts(tq_get("TWTR", from = start_date, get = "stock.price"),start = start_date, date_var = date, select = adjusted),
          tk_xts(tq_get("MSFT", from = start_date, get = "stock.price"),start = start_date, date_var = date, select = adjusted),
          tk_xts(tq_get("SNE", from = start_date, get = "stock.price"),start = start_date, date_var = date, select = adjusted),
          tk_xts(tq_get("WMT", from = start_date, get = "stock.price"),start = start_date, date_var = date, select = adjusted))  
          
colnames(int_stock) <- c("Ford","Amazon","Coca Cola","Tesla","Disney","Twitter","Microsoft","Sony","Walmart")

################################################################################################
################################ Interactive Graphs ##################################

dygraph(int_stock) %>% 
  dyRangeSelector() %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))

####################################

predict(int_stock$Ford, n.ahead = 72, prediction.interval = TRUE)

##########################

dygraph(int_stock) %>% 
  dyRebase(value = 100) %>% 
  dyRangeSelector() %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))

################################################################################
################################ ggplot Graphs #################################

tick_for_ggplot %>% 
  group_by(symbol) %>% 
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "daily.returns") %>% 
  ggplot(aes(x = date, y = daily.returns,
             fill = symbol)) +
  geom_bar(position = "dodge", stat = "identity", show.legend = FALSE) +
  labs(title = "My stocks: Monthly Returns",
       y = "Returns", x = "", color = "") +
  facet_wrap(~symbol) +
  scale_y_continuous(labels = scales::percent) +
  theme_tq() +
  scale_fill_tq()

#########################################################
amount_invested <- 10000

tick_for_ggplot %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra") %>%
  tq_portfolio(assets_col   = symbol, 
               returns_col  = Ra,
               col_rename   = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth * amount_invested) %>% 
  ggplot(aes(x = date, y = investment.growth)) +
  geom_line(size = 2, color = palette_light()[[1]]) +
  labs(title = "Portfolio Growth",
       subtitle =  "$10,000 spead over all investments",
       caption = "Over the course of five years",
       x = "", y = "Portfolio Value") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

#################################################################
join1 <- tq_get(tickers_used, from = start_date, get = "stock.price") %>% 
  group_by(symbol) %>% 
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")

join <- "DIA" %>% 
  tq_get(get = "stock.price", from = start_date) %>% 
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")

base <- left_join(join1,join,by = "date")

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
  geom_hline(yintercept = 0, color = palette_light()[[1]], show.legend = FALSE) +
  geom_line(size = 1, show.legend = FALSE) +
  labs(title = "My Stocks: Rolling Correlation to DIA",
       x = "", y = "Correlation", color = "") +
  facet_wrap(~ symbol, ncol = 2) +
  theme_tq() + 
  scale_color_tq()  


