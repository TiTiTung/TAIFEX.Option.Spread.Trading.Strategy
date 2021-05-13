#==================================================================================
# T-T-Tung
# TAIFEX.Data.Manipulation
#==================================================================================
source("packages.you.need.R")
source("zfun.R")

#------------------------------------------------------------------------- #
#  TWII
#------------------------------------------------------------------------- #

TWII <- read.xlsx("TWII.Cmoney.xlsm", sheet = 1, detectDates = TRUE, startRow = 3) %>% 
  select(-2)%>%
  as.tibble() %>% 
  filter(date >= "2005-01-01")
  
names(TWII) <- c("date", "open", "high", "low", "close")

# 把資料改成 numeric
TWII[,2:5] %<>%
  sapply(as.numeric) %>%
  na.replace(0) %>%
  as.tibble() 

write.csv(TWII, "TWII.csv", row.names = F)

#------------------------------------------------------------------------- #
# read TX, MTX, TXO, final.settlement Data form TAIFEX
#------------------------------------------------------------------------- #
TX <- read_rds(dir(pattern = "^TX.20")) %>%
  select(date, contract, contract, contract.month, open, high, low, close,
         settlement.price, change,  volume, OI, trading.session) %>%
  filter(trading.session == "regular")  # 日盤資料

# 下載結算日期與結算價資料
final.settlement.price.M <- read_rds(dir(pattern = "^Final.Settlement.Price.202")) %>%
  filter(symbols == "TXO", settlement.month %>% str_length == 6 )



MTX <- read_rds(dir(pattern = "^MTX.202"))
TXO <- read_rds(dir(pattern = "^TXO.202"))
# final.settlement.price <- read_rds(dir(pattern = "^Final.Settlement.Price.202"))
final.settlement.price <- read_rds(dir(pattern = "^Final.Settlement.Price.202")) %>%
  filter(symbols == "TXO") %>%
  filter(date >= "2012-11-28") %>%
  select(-symbols.cht)

# 把除夕W1 直接接上月選的資期篩出來
final.settlement.price$T=0
for (i in 1:nrow(final.settlement.price)) {
  
  if (final.settlement.price$settlement.month[i] %>% str_length() == 8 &
      final.settlement.price$settlement.month[i] %>% str_sub( 7, 8) == "W2" &
      final.settlement.price$settlement.month[i + 1] %>% str_length() == 6 ) {
    
    final.settlement.price$T[i] = 1
  }
  
  if (final.settlement.price$settlement.month[i] %>% str_length() == 8 &
      final.settlement.price$settlement.month[i] %>% str_sub( 7, 8) != "W2" &
      final.settlement.price$settlement.month[i + 1] %>% str_length() == 6 ) {
    
    final.settlement.price$T[i] = 1
  }
  
}


date.W2.month <- final.settlement.price %>%
  filter(settlement.month %>% str_length == 6 | T == 1)%>%
  group_by(year(date), month(date)) %>%
  summarise(max = as.Date(date) %>% max ,
            min = as.Date(date) %>% min )


date.W2.to.month <- as.Date(NA)

for (i in seq_along(date.W2.month$max)) {
  xx = (date.W2.month$min[i] : date.W2.month$max[i]) %>% as.Date()
  date.W2.to.month = c(date.W2.to.month,xx)
}

#------------------------------------------------------------------------- #
# read TX, MTX, TXO, final.settlement Data form TAIFEX
#------------------------------------------------------------------------- #

# ============================= 盤後 資料預處理 ==================================== #

TX.afterhours <- TX %>%
  select(date, contract, contract, contract.month, open, high, low, close,
         settlement.price, change,  volume, OI, trading.session) %>%
  ## 盤後資料
  filter(trading.session == "regular")

# 把資料改成 numeric
TX.afterhours[,4:11] %<>%
  sapply(as.numeric) %>%
  na.replace(0) %>%
  as.tibble()

#==================================================================================
# 區分近遠月資料流程
#==================================================================================
# 1. 先用長度區分，只留月份合約 ("202009" vs "202009/202106")
# 2. 用group_by區分日期之後，不能直接用filter()，所以還要再創新column篩選 (mutate())
# 3. 再用mutate(first(),nth())算出同個人期的第一第二個合約，分別就是近月跟次近月合約
# 4. 用filter把contract.month是近月與次近月的合約留下來
# 5. 以防萬一，再創一個 nearby.deferred 來定義是近月(nearby)或次近月(deferred)
# ---------------------------------------------------------------------------------
# **由於是用順序來篩選近遠月合約，後續排序錯誤的跨可能出bug
#==================================================================================
TX.nearby.deferred <- TX %>%
  filter(contract.month %>% str_length == 6) %>%
  group_by(date) %>%
  mutate(nearby.month = first(contract.month),deferred.month = nth(contract.month,2) ) %>%
  filter(contract.month == nearby.month | contract.month == deferred.month) %>%
  mutate(nearby.deferred = ifelse(contract.month == nearby.month, "nearby", "deferred") ) %>%
  select(-nearby.month, -deferred.month)


# ============================= 近月 資料預處理 ==================================== #
TX1 <- TX.nearby.deferred %>%
  filter(nearby.deferred == "nearby") %>%
  mutate(Settlement = ifelse(date %in% (final.settlement.price$date ), 1, 0) ) %>% 
  select(date,  open, high, low, close)

# ============================= 次近 資料預處理 ==================================== #
TX2 <- TX.nearby.deferred %>%
  filter(nearby.deferred == "deferred")%>%
  mutate(Settlement = ifelse(date %in% (final.settlement.price$date ), 1, 0) )


# DT::datatable(TXO, caption = 'Table 2: 價差資料',
#               filter = 'top')

write.csv(TX1, "TX.csv", row.names = F)


#------------------------------------------------------------------------- #
# TXO Data form TAIFEX
#------------------------------------------------------------------------- #
#==================================================================================
# 臺指選擇權最大未平倉壓力與支撐
# 週選結算當天的週合約，要被剃除!!!!
#==================================================================================
# 週選合約
TXO.week <- TXO %>%
  filter(date >= "2012-01-01" ) %>%
  filter((contract.month %>% str_length > 6) |
           (date %in% date.W2.to.month & # str_sub(contract.month, 1, 6)
              str_c(str_sub(date, 1, 4),str_sub(date, 6, 7)) == parse_number(contract.month)) )
 # 把當日結算的合約也去掉
  # filter(!(str_c(date, contract.month) %in%
  #            str_c(final.settlement.price$date, final.settlement.price$settlement.month)))


contract <- unique(TXO.week$contract.month)

TXO.week %<>% mutate(T = 0)
OP.data <- TXO.week[0,]


for(i in seq_along(contract)){
  
  tmp.option = TXO.week[TXO.week$contract.month == contract[i],]
  tmp.date = unique(tmp.option$date)
  tmp.date = tmp.date[order(tmp.date, decreasing = F)]
  
    for(j in 1 : length(tmp.date)){
      tmp.option[tmp.option$date == tmp.date[j],"T"] = length(tmp.date) - j + 1
      OP.data = rbind(OP.data, tmp.option %>% filter(date == tmp.date[j]))
    }
}


write.csv(OP.data, "Option.week.data.csv", row.names = F)
write.csv(final.settlement.price, "Week-Settlement.csv", row.names = F)


#------------------------------------------------------------------------- #
#  MTX Data form TAIFEX
#------------------------------------------------------------------------- #


# 週選合約
MTW <- MTX %>%
  ## 盤後資料
  filter(trading.session == "regular") %>%
  filter(date >= "2013-07-31") %>%
  filter(contract.month %>% str_length  <= 9) %>%
  filter((contract.month %>% str_length == 8 ) |
           (date %in% date.W2.to.month & # str_sub(contract.month, 1, 6)
              str_c(str_sub(date, 1, 4),str_sub(date, 6, 7)) == parse_number(contract.month)) ) %>% 
  filter(!(str_c(date, contract.month) %in%
           str_c(final.settlement.price$date, final.settlement.price$settlement.month))) %>% 
  select(date, open, high, low, close)


write.csv(MTW, "MTW.csv", row.names = F)


MTX %>%
  ## 盤後資料
  filter(trading.session == "regular") %>%
  filter(date >= "2018-02-05") %>%
  filter(contract.month %>% str_length  <= 8) %>%
  filter((contract.month %>% str_length == 8 ) |
           (date %in% date.W2.to.month))


str_c(str_sub(MTX$date, 1, 4),str_sub(MTX$date, 6, 7)) == parse_number(MTX$contract.month)














