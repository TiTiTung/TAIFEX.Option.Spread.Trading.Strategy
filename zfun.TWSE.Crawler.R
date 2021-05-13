# =============================================================================== #
# T-T-Tung
# TWSE.Crawler
# 2021/05
# =============================================================================== #
# =============================================================================== #
library("magrittr", character.only = TRUE)
library("stringr", character.only = TRUE)
library("lubridate", character.only = TRUE)
library("tidyr", character.only = TRUE)
library("tibble", character.only = TRUE)
library("dplyr", character.only = TRUE)
library("jsonlite")
# =============================================================================== #
# =============================================================================== #




twse.TWII.Crawler <- function(start.date = as.Date("2005-01-01"), end.date = Sys.Date()){
# 證交所網址 : https://www.twse.com.tw/zh/page/trading/indices/MI_5MINS_HIST.html  
# 爬蟲網站 : "https://www.twse.com.tw/indicesReport/MI_5MINS_HIST?response=sjason&date=20130310"
# =============================================================================== #
# 證交所爬蟲特性: 輸入任何一個日期，當月資料會全部爬下來
# 因此跑回圈爬蟲時，我只需要產出每一年的每一個月的第一天的字串就可以爬蟲
# =============================================================================== #
  
  
  
  # 計算start.date至end.date以來，總共有幾個月
  stock.length.by.month = (year(end.date) - year(start.date))*12 + month(end.date) 
  
  if (stock.length.by.month > 12) {
      # 產出每個月一號的字串 : "2011-01-01" "2011-02-01" "2011-03-01"
      stock.date.by.month <- seq.Date(from = as.Date(start.date ,format = "%Y/%m/%d"), 
                                    by = "month", length.out = stock.length.by.month)
    
  } else if (stock.length.by.month <= 12 & month(end.date) - month(start.date) != 0) {
    
      stock.date.by.month <- seq.Date(from = as.Date(start.date ,format = "%Y/%m/%d"), 
                                    by = "month", length.out = month(end.date) - month(start.date))
    
  } else if (stock.length.by.month <= 12 & month(end.date) - month(start.date) == 0) {
    
      stock.date.by.month <- start.date
    
  }
  
  
  # 證交所需要的日期格式不能有 "-" , str_remove_all("-")
  stock.date.by.month %<>% str_remove_all("-")
  
  # 爬蟲迴圈-----------------------------------------------------------------------
  TWII <- NULL
  for (i in stock.date.by.month) {
    # R裡面Json檔爬下來之後會是list格式
    TWII.json <- jsonlite::fromJSON(str_c("https://www.twse.com.tw/indicesReport/MI_5MINS_HIST?response=sjason&date=", i), )
    # 由於爬蟲下來之後是Matrix格式，要轉成Dataframe來rbind
    x  <-  TWII.json$data %>% as.data.frame(row.names=NULL,stringsAsFactors = FALSE)
    
    TWII <- rbind(TWII, x)
    
    # 證交所爬蟲會檔，設定等待時間
    Sys.sleep(5)
    
  }# 爬蟲迴圈 END------------------------------------------------------------------
  
  
  
      # 資料整理-----------------------------------------------------------------------
      names(TWII) <- c("date", "open", "high", "low", "close")
      
      TWII.date <- TWII$date %>% as.Date()
      year(TWII.date) <- year(TWII.date) + 1911 # 民國轉西元
      
      TWII$date <- TWII.date
      
      for (j in names(TWII)[-1]) {
        TWII[j]  <-  str_remove(TWII[[j]], pattern = ",") %>% as.numeric() %>% na.replace(0)
      }
      
      TWII %<>% arrange(date)
      # 資料整理-----------------------------------------------------------------------
  
  
  return(TWII)
  
}




# 
# TWII.TEST <- twse.TWII.Crawler(start.date = as.Date("2005-01-01"), end.date = Sys.Date())
# TWII.TEST.1 <-  TWII.TEST %>% drop_na(date)
# 
# write_rds(TWII.TEST.1, str_c("TWII.TEST.Crawler", TWII.TEST.1$date %>% max(), ".raw.dat.RDS"))

# write.csv(TWII.TEST.1, str_c("TWII.TEST.Crawler.", y$date %>% max(), ".raw.dat.csv"))




