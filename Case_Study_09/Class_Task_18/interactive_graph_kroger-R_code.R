pacman::p_load(tidyquant,dygraphs,timetk)

#############  Kroger (KG)

kroger <- c("KR") %>% 
  tq_get(get = "stock.prices", from = "2014-03-07")

##################### Interactive Graph ###############################

kr_ts <- tk_xts(kroger, start = "2014-03-07", date_var = date, select = adjusted)

dygraph(kr_ts) %>% 
  dyRangeSelector() %>% 
  dyOptions(fillGraph = TRUE, fillAlpha = .25, colors = "pink") 






######################## Investing 10,0000 ##########################

dygraph(kr_ts) %>% 
  dyRangeSelector() %>% 
  dyOptions(fillGraph = TRUE, fillAlpha = .25, colors = "red") %>% 
  dyRebase(value = 10000)




###################### Annotate Graph #########################

dygraph(kr_ts) %>% 
  dyRangeSelector() %>% 
  dyOptions(fillGraph = TRUE, fillAlpha = .25, colors = "blue") %>% 
  dyAnnotation("2017-6-14", text = "Whole Foods and Amazon", attachAtBottom = TRUE, width = 160)





#################################
kroger %>%
  ggplot(aes(x = date)) +
  geom_segment(aes(xend = date, y = low, yend = high),
               size = 1, alpha = .75) +
  geom_point(aes(y = high), size = 1) +
  geom_point(aes(y = low), size = 1) +
  labs(title = "My Stocks: Min/Max Price By Day",
       y = "Stock Price", color = "") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank())
