#==================================================================================
# T-T-Tung
# TAIFEX.Crawler
#==================================================================================

source("packages.you.need.R")
source("zfun.TWSE.Crawler.R")


TWII <- read.xlsx("TWII.Cmoney.xlsm", sheet = 1, detectDates = TRUE, startRow = 3) %>% 
  select(-2)%>%
  as.tibble()  
names(TWII) <- c("date", "open", "high", "low", "close")

# 把資料改成 numeric
TWII[,2:5] %<>%
  sapply(as.numeric) %>%
  na.replace(0) %>%
  as.tibble() 

TWII.date <- TWII$date



# ================================================================================================== #
# ================================================================================================== #
# 台指期資料更新 (TX)
# ================================================================================================== #
# ================================================================================================== #
TX <- read_rds(dir(pattern = "^TX.20"))

#--------------------------------------------------------------------------
# 期交所下載之資料的網址只有兩個地方不一樣，至作變數遠端控制變數
# "https://www.taifex.com.tw/cht/3/ -[dlFutDataDown]- "
# "?down_type=1&commodity_id= -[TX]- &commodity_id2=&queryStartDate=2020%2F09%2F01&queryEndDate=2020%2F09%2F24"
#--------------------------------------------------------------------------
taifex.commodity.type <- "Fut"
taifex.commodity <- "TX"
start_date <- TX$date %>% max() + 1
start_year <- TX$date %>% max() %>% year()


# 找出有開盤的日期-----------------------------------------------------------
# 下載期交所得資料最多只能載一個月，因此先用 quantmod 抓出大盤資料，選出有開盤的時間
# 再把每個月有開盤的第一天跟最後一天選出來，塞到下載連結裡面
#--------------------------------------------------------------------------
# # 期交所網站:
# "https://www.taifex.com.tw/cht/3/dlOptDataDown"
# # 下載動作：
# "?down_type=1&commodity_id=TXO&commodity_id2=&queryStartDate=2020%2F09%2F01&queryEndDate=2020%2F09%2F18"
#--------------------------------------------------------------------------
{
  # https://rdrr.io/cran/quantmod/man/getSymbols.html
  twii_date <- TWII.date[TWII.date >= start_date ]
  
  
  for(year in start_year:( Sys.Date() %>% year()) ){
    
    for(month in sprintf('%02d', 1:12)){
      x = twii_date[str_detect(twii_date, str_c(year,"-",month,"-"))] %>% min()
      y = twii_date[str_detect(twii_date, str_c(year,"-",month,"-"))] %>% max()
      
      if(year  == start_year & month == sprintf('%02d', 1)){
        min.date = x
        max.date = y
      }else{
        min.date = c(min.date, x)
        max.date = c(max.date, y)
      }
    }
  }
  
  # 剔除 NA
  # 轉換成期交所抓資料需要的日期格式
  # magrittr::%<>%
  min.date %<>% as.character() %>% na.omit() %>% str_replace_all("-","%2F")
  max.date %<>% as.character() %>% na.omit() %>% str_replace_all("-","%2F")
  
  downlode.operation <- str_c("?down_type=1&commodity_id=",
                              taifex.commodity,
                              "&commodity_id2=&queryStartDate=",
                              min.date,"&queryEndDate=",max.date)
}

#--------------------------------------------------------------------------
# 用 read.csv() 把資料爬進來
#--------------------------------------------------------------------------
# n 是用來儲存第一筆下載的檔案開關，為了後面rbind(y,x)
y <- NULL
# 計算for 迴圈跑的時間
time.start <- Sys.time()

for(i in downlode.operation){
  
  URL <- str_c("https://www.taifex.com.tw/cht/3/dl", taifex.commodity.type, "DataDown",i)
  # x <- read.csv(URL, sep = ",",header = T, stringsAsFactors = FALSE,row.names =NULL,fileEncoding='big5')
  x <- readr::read_csv(URL, locale = readr::locale(encoding = "big5"))
  y = rbind(y,x)
  
  # 計算for 迴圈跑的時間
  time.interval <- time_length(interval(time.start, Sys.time()), unit = "min") %>% round(2)
  cat("All Data are processed! Execution time: ", time.interval, "mins", "\n")
}

#--------------------------------------------------------------------------
# 爬完資料後的後續處理
#--------------------------------------------------------------------------
# 更改至正確的 column 名稱
colnames(y) <- c("date", "contract", "contract.month", "open", "high", "low", "close", "change",
                 "change.percent", "volume", "settlement.price", "OI", "Best.Bid",	"Best.Ask",
                 "historical.high",	"historical.low",	"Trading.Halt", "trading.session",
                 "Volume(executions among spread order and single order only)")

y$trading.session %<>% stringr::str_replace_all("一般", "regular")
y$trading.session %<>% stringr::str_replace_all("盤後", "after-hours")
#  TX 資料預處理 ----------------------------------------------------------- #
# 更改日期型態
y$date %<>% as.Date()
y$contract.month %<>% str_trim()

# 把資料改成 numeric
y[,4:12] %<>%
  sapply(as.numeric) %>%
  na.replace(0) %>%
  as.tibble()
#  把新抓的資料與舊資料合併 ------------------------------------------------ #
y <- rbind(TX, y)

#  資料儲存 ---------------------------------------------------------------- #
file.remove(dir(pattern = "^TX.20", full.names=TRUE)) %>% sum()

write_rds(y, str_c(taifex.commodity,
                   ".",
                   start_date <- y$date %>% max(),
                   ".raw.dat.RDS"))











# ================================================================================================== #
# ================================================================================================== #
# 小型台指期資料更新 (MTX)
# ================================================================================================== #
# ================================================================================================== #
MTX <- read_rds(dir(pattern = "^MTX.202"))

#--------------------------------------------------------------------------
# 期交所下載之資料的網址只有兩個地方不一樣，至作變數遠端控制變數
# "https://www.taifex.com.tw/cht/3/ -[dlFutDataDown]- "
# "?down_type=1&commodity_id= -[TX]- &commodity_id2=&queryStartDate=2020%2F09%2F01&queryEndDate=2020%2F09%2F24"
#--------------------------------------------------------------------------
taifex.commodity.type <- "Fut"
taifex.commodity <- "MTX"
start_date <- MTX$date %>% max() + 1
start_year <- MTX$date %>% max() %>% year()


# 找出有開盤的日期-----------------------------------------------------------
# 下載期交所得資料最多只能載一個月，因此先用 quantmod 抓出大盤資料，選出有開盤的時間
# 再把每個月有開盤的第一天跟最後一天選出來，塞到下載連結裡面
#--------------------------------------------------------------------------
# # 期交所網站:
# "https://www.taifex.com.tw/cht/3/dlOptDataDown"
# # 下載動作：
# "?down_type=1&commodity_id=TXO&commodity_id2=&queryStartDate=2020%2F09%2F01&queryEndDate=2020%2F09%2F18"
#--------------------------------------------------------------------------
{
  # https://rdrr.io/cran/quantmod/man/getSymbols.html
  twii_date <- TWII.date[TWII.date >= start_date ]
  
  
  for(year in start_year:( Sys.Date() %>% year()) ){
    
    for(month in sprintf('%02d', 1:12)){
      x = twii_date[str_detect(twii_date, str_c(year,"-",month,"-"))] %>% min()
      y = twii_date[str_detect(twii_date, str_c(year,"-",month,"-"))] %>% max()
      
      if(year  == start_year & month == sprintf('%02d', 1)){
        min.date = x
        max.date = y
      }else{
        min.date = c(min.date, x)
        max.date = c(max.date, y)
      }
    }
  }
  
  # 剔除 NA
  # 轉換成期交所抓資料需要的日期格式
  # magrittr::%<>%
  min.date %<>% as.character() %>% na.omit() %>% str_replace_all("-","%2F")
  max.date %<>% as.character() %>% na.omit() %>% str_replace_all("-","%2F")
  
  downlode.operation <- str_c("?down_type=1&commodity_id=",
                              taifex.commodity,
                              "&commodity_id2=&queryStartDate=",
                              min.date,"&queryEndDate=",max.date)
}

#--------------------------------------------------------------------------
# 用 read.csv() 把資料爬進來
#--------------------------------------------------------------------------
# n 是用來儲存第一筆下載的檔案開關，為了後面rbind(y,x)
y <- NULL
# 計算for 迴圈跑的時間
time.start <- Sys.time()

for(i in downlode.operation){
  
  URL <- str_c("https://www.taifex.com.tw/cht/3/dl", taifex.commodity.type, "DataDown",i)
  # x <- read.csv(URL, sep = ",",header = T, stringsAsFactors = FALSE,row.names =NULL,fileEncoding='big5')
  x <- readr::read_csv(URL, locale = readr::locale(encoding = "big5"))
  y = rbind(y,x)
  
  # 計算for 迴圈跑的時間
  time.interval <- time_length(interval(time.start, Sys.time()), unit = "min") %>% round(2)
  cat("All Data are processed! Execution time: ", time.interval, "mins", "\n")
}

#--------------------------------------------------------------------------
# 爬完資料後的後續處理
#--------------------------------------------------------------------------
# 更改至正確的 column 名稱
colnames(y) <- c("date", "contract", "contract.month", "open", "high", "low", "close", "change",
                 "change.percent", "volume", "settlement.price", "OI", "Best.Bid",	"Best.Ask",
                 "historical.high",	"historical.low",	"Trading.Halt", "trading.session",
                 "Volume(executions among spread order and single order only)")

y$trading.session %<>% stringr::str_replace_all("一般", "regular")
y$trading.session %<>% stringr::str_replace_all("盤後", "after-hours")
#  MTX 資料預處理 ----------------------------------------------------------- #
# 更改日期型態
y$date %<>% as.Date()
y$contract.month %<>% str_trim()

# 把資料改成 numeric
y[,4:12] %<>%
  sapply(as.numeric) %>%
  na.replace(0) %>%
  as.tibble()
y %<>% arrange(date)
#  把新抓的資料與舊資料合併 ------------------------------------------------ #
y <- rbind(MTX, y)

#  資料儲存 ---------------------------------------------------------------- #
file.remove(dir(pattern = "^MTX.202", full.names=TRUE)) %>% sum()

write_rds(y, str_c(taifex.commodity,
                   ".",
                   start_date <- y$date %>% max(),
                   ".raw.dat.RDS"))














# ================================================================================================== #
# ================================================================================================== #
# 選擇權資料更新 (TXO)
# ================================================================================================== #
# ================================================================================================== #
TXO <- read_rds(dir(pattern = "^TXO.202"))

# TXO.raw.dat <- read_rds(dir(pattern = "^TXO.20"))
pryr::object_size(TXO)
glimpse(TXO)



start_date <- TXO$date %>% max() + 1
start_year <- TXO$date %>% max() %>% year()

start_date <- "2001-01-01"
start_year <- 2001



{
  twii_date <- TWII.date[TWII.date >= start_date ]
  
  
  for(year in start_year:( Sys.Date() %>% year()) ){
    
    for(month in sprintf('%02d', 1:12)){
      x = twii_date[str_detect(twii_date, str_c(year,"-",month,"-"))] %>% min()  
      y = twii_date[str_detect(twii_date, str_c(year,"-",month,"-"))] %>% max() 
      
      if(year  == start_year & month == sprintf('%02d', 1)){
        min.date = x
        max.date = y
      }else{
        min.date = c(min.date, x)
        max.date = c(max.date, y) 
      }
    }
  }
  
  # 剔除 NA
  # 轉換成期交所抓資料需要的日期格式
  # magrittr::%<>% 
  min.date %<>% as.character() %>% na.omit() %>% str_replace_all("-","%2F")
  max.date %<>% as.character() %>% na.omit() %>% str_replace_all("-","%2F")
  
  downlode.operation <- str_c("?down_type=1&commodity_id=TXO&commodity_id2=&queryStartDate=",
                              min.date,"&queryEndDate=",max.date)
}

#--------------------------------------------------------------------------
# 用 read.csv() 把資料爬進來
#--------------------------------------------------------------------------
# n 是用來儲存第一筆下載的檔案開關，為了後面rbind(y,x)
y <- NULL
# 計算for 迴圈跑的時間
time.start <- Sys.time() 

for(i in downlode.operation){
  URL <- str_c("https://www.taifex.com.tw/cht/3/dlOptDataDown",i)
  # x <- read.csv(URL, sep = ",",header = T, 
  # stringsAsFactors = FALSE,row.names =NULL,fileEncoding='big5')
  x <- readr::read_csv(URL, locale = readr::locale(encoding = "big5"))
  y = rbind(y,x)
  
  # 計算for 迴圈跑的時間
  time.interval <- time_length(interval(time.start, Sys.time()), unit = "min") %>% round(2)
  cat("All articles are processed! Execution time: ", time.interval, "mins", "\n")
}

#--------------------------------------------------------------------------
# 爬完資料後的後續處理
#--------------------------------------------------------------------------
# 更改至正確的 column 名稱
colnames(y) <- c("date", "contract", "contract.month", "strike.price", "call.put", "open", "high", 
                 "low", "close", "volume", "settlement.price", "OI", "Best.Bid",	"Best.Ask",	
                 "historical.high",	"historical.low",	"Trading.Halt", "trading.session")

y$call.put %<>% stringr::str_replace_all("買權", "call")
y$call.put %<>% stringr::str_replace_all("賣權", "put")

y$trading.session %<>% stringr::str_replace_all("一般", "regular")
y$trading.session %<>% stringr::str_replace_all("盤後", "after-hours")


#  TXO 資料預處理 ------------------------------------------------- #

# 更改日期型態
y$date %<>% as.Date()
# 去除字串尾巴的空白
y$contract.month %<>% str_trim()
## 日盤資料
y %<>% 
  filter(trading.session == "regular") %>% 
  select(date, contract, contract.month, strike.price, call.put, 
           open, high, low, close,  volume, settlement.price, OI) 
# 把資料改成 numeric 
y[,6:12] %<>% 
  sapply(as.numeric) %>% 
  na.replace(0) %>% 
  as.tibble()

#  把新抓的資料與舊資料合併 ------------------------------------------------ #
y <- rbind(TXO, y)

#  資料儲存 ---------------------------------------------------------------- #


# write_rds(y, "TXO.raw.dat.RDS")

file.remove(dir(pattern = "^TXO.202", full.names=TRUE)) %>% sum()

# "2001-12-24" to "2021-05-07" cost Execution time:  4.36 mins 
write_rds(y, str_c("TXO.", y$date %>% max(), ".raw.dat.RDS"))








# ================================================================================================== #
# ================================================================================================== #
# 結算價爬蟲 (Final.Settlement.Price)
# ================================================================================================== #
# ================================================================================================== #

{
  #  基本參數 ------------------------------------------------ #
  # 只能從2002開始，不然會當機，忘記為啥了
  start_year <- 2002
  y <- NULL
  # 計算for 迴圈跑的時間
  time.start <- Sys.time()
  
  #--------------------------------------------------------------------------
  # 用 read_csv() 把資料爬進來
  #--------------------------------------------------------------------------
  
  for(year in start_year:( Sys.Date() %>% year() ) ){
    # x <- read.csv(str_c("https://www.taifex.com.tw/cht/5/fSPDataDown?start_year=",
    #                       year, "&start_month=01&end_year=", year, "&end_month=12&dlFileType=3"),
    #                 sep = ",",header = T, stringsAsFactors = FALSE,row.names =NULL,fileEncoding='big5')
    URL <- str_c("https://www.taifex.com.tw/cht/5/fSPDataDown?start_year=",
                 year, "&start_month=01&end_year=", year, "&end_month=12&dlFileType=3")
    
    x <- readr::read_csv(URL, locale = readr::locale(encoding = "big5"))
    
    # rbind()
    y = rbind(y,x)
    
    time.interval <- time_length(interval(time.start, Sys.time()), unit = "min") %>% round(2)
    cat("All Data are processed! Execution time: ", time.interval, "mins", "\n")
    
  }
  
}
#--------------------------------------------------------------------------
# 爬完資料後的後續處理
#--------------------------------------------------------------------------
# 更改至正確的 column 名稱
colnames(y) <- c("date", "settlement.month", "symbols", "symbols.cht", "settlement.price")
y$date %<>% as.Date()
y$delivery.month %<>% str_trim()
y$settlement.price %<>% as.numeric()
y %<>% arrange(date)
#--------------------------------------------------------------------------
# 資料儲存
#--------------------------------------------------------------------------
# write_rds(y, "final.settlement.price.raw.dat.RDS")
file.remove(dir(pattern = "^Final.Settlement", full.names=TRUE)) %>% sum()

write_rds(y, str_c("Final.Settlement.Price.", y$date %>% max(), ".raw.dat.RDS"))











