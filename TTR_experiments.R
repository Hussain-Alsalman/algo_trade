library("tasi")
library("quantmod")
library("lcyanalysis")
library("TTR")
indx <- get_index_records(start_date = "2010-01-01", end_date = "2021-04-04",use_cache = TRUE)
indx_xts <- df_to_xts(indx)
date_range <- "2019-01-01/2021-04-04"
chart_Series(indx_xts[date_range])
add_BBands(n = 20)
add_SMA(n = 30, col = "red")
add_SMA(n = 5, col = "blue")
add_MACD()

msci30 <- get_MSCI30("2021-01-01", "2021-04-04")
msci30_xts <- df_to_xts(msci30)
chart_Series(msci30_xts[date_range])
add_BBands(n = 20)
add_SMA(n = 30, col = "red")
add_SMA(n = 5, col = "blue")

insurance <- get_insurance("2019-01-01", "2021-04-04")
insurance_xts <- df_to_xts(insurance)
chart_Series(insurance_xts[date_range])
add_BBands(n = 20)
add_SMA(n = 30, col = "red")
add_SMA(n = 5, col = "blue")
commercials <- get_commercials("2018-01-01", "2021-04-04")
commercials_xts <- df_to_xts(commercials)
chart_Series(commercials_xts[date_range])
add_BBands(n = 20)
add_SMA(n = 30, col = "red")
add_SMA(n = 5, col = "blue")
add_Vo()
library("dplyr")
stock_indices %>%  filter( sectorName == "Commercial & Professional Svc") %>%  select(symbol)

maharah <- get_company_records(start_date = "2019-01-01",end_date =  "2021-04-04",company_symbol = 1831,use_cache = FALSE)
maharah_xts <- df_to_xts(maharah)
chart_Series(maharah_xts[date_range])
add_BBands(n = 20)
add_SMA(n = 30, col = "red")
add_SMA(n = 5, col = "blue")
add_Vo()
add_RSI()
add_TA(bearpower(maharah_xts,down = 30,day = 30), on = TRUE)



install.packages("Quandl")
library("Quandl")

oil_prices <- Quandl("OPEC/ORB", type = "xts")
chart_Series(oil_prices)


library("highcharter")
highchart(type = "stock") %>%
  hc_title(text = "OPEC oil Prices") %>%
  hc_yAxis_multiples(
    list(lineWidth = 3),
    list(showLastLabel = TRUE, opposite = TRUE)
  ) %>% 
  hc_add_series(oil_prices) %>% 
  hc_legend(
    align = "left",
    verticalAlign = "top",
    layout = "vertical",
    x = 0,
    y = 100
  )