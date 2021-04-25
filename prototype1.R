library("tasi")
library("quantmod")
library("lcyanalysis")
library("TTR")
library("lubridate")


analyze <- function(symbol, num_weeks) { 
  str_date <- as.character(Sys.Date() - dweeks(num_weeks))
  comp_name <- stock_indices[which(stock_indices$symbol == symbol),"companyNameEN"]
  end_date <- as.character(Sys.Date())
  df_stock <- get_company_records(start_date = str_date, end_date = end_date, company_symbol = symbol, use_cache = TRUE)
  df_stock <- df_to_xts(df_stock)
  chart_Series(df_stock,TA = c('add_MACD()'), name = comp_name )
  add_TA(ultimateOscillator(df_stock))
  
  }

analyze(symbol = 2222, num_weeks =  24)






stock_symbol <- 2280
indx <- get_index_records("2018-01-01", "2021-04-04")
indx_xts <- df_to_xts(indx)
date_range <- "" #"2020-01-01/2021-04-04"
chart_Series(indx_xts[date_range])
add_BBands(n = 20)
add_SMA(n = 30, col = "red")
add_SMA(n = 5, col = "blue")

msci30 <- get_MSCI30("2018-01-01", "2021-04-04")
msci30_xts <- df_to_xts(msci30)
chart_Series(msci30[date_range])
add_BBands(n = 20)
add_SMA(n = 30, col = "red")
add_SMA(n = 5, col = "blue")


add_BBands(indx_xts[date_range])
add_TA(w_bottom(h = indx_xts[date_range],top = 60,down = 40,month = 5,day = 3), on = TRUE, col = "red")
add_TA(bullpower(h = indx_xts[date_range],top = 60,day = 50), on = TRUE, col = "red")
data("stock_indices")

nama_symbol<- as.numeric(stock_indices[which("NAMA CHEMICALS" == stock_indices$tradingNameEn),"symbol"])
nama_df <- get_company_records("2011-01-01", "2021-04-18", 2280)
nama_xts <- df_to_xts(nama_df)
chart_Series(x = nama_xts["2011-01-01/2021-04-18"], TA = c("add_BBands()", "add_Vo()"))
add_TA(x = RSI(Cl(nama_xts)))
add_TA(w_bottom(h =  nama_xts["2011-01-01/2021-04-18"],top = 60,down = 40,month = 5,day = 3), on = TRUE, col = "red")
add_TA(bullpower(h =  nama_xts["2011-01-01/2021-04-18"],top = 60,day = 50), on = TRUE, col = "red")


library("rvest")

html_elements(read_html(
  "https://www.saudiexchange.sa/wps/portal/tadawul/market-participants/issuers/issuers-directory/company-details/!ut/p/z1/pZLdboJAEEafhusZFxDs3XahLAVJVsXK3jSrtWrCXyy2wafvik0ak0ptOneTnDOT-TIgYQGyVO-7jWp2Valy3Wdy-JzwmHF0SRT4MxPp0BuzSRISRISnDiCEuYORhTHGzkADAYZibJkoTJB_8oMwcZAKyucPc4265H8-Wrf5eKUo_uZn2ne-AUzTkQZMJoRnEwxsmIIE-bZW-9VWHNb7FjJC3NNeeTkaOfG06UeRx7R5b38BfdFeAj9k1wucwumAnusfQW7yann-hG3T1HcGGtioF_VxyA2dzqoqalW207ZYVhrqbquLNE0Xx9lr7U_4MaafRITKYQ!!/p0/IZ7_NHLCH082KGET30A6DMCRNI2086=CZ6_NHLCH082KGET30A6DMCRNI2000=NJstatementsTabData=/?statementType=4&reportType=0&symbol=2280"),
  css = ".ico:nth-child(1) img"))


