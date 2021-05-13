#==========================================================================
# T-T-Tung
#=======================下載物件===========================================
# 有一些必要的 Packages 是一定要載的！
# object.size()
#==========================================================================
{
  if (!is.element("tidyverse", installed.packages()[,1])) {
    install.packages("tidyverse", dep = TRUE)
    require("tidyverse", character.only = TRUE)
  } else {
    require("tidyverse", character.only = TRUE)
  }

  if (!is.element("readr", installed.packages()[,1])) {
    install.packages("readr", dep = TRUE)
    require("readr", character.only = TRUE)
  } else {
    require("readr", character.only = TRUE)
  }

  if (!is.element("dplyr", installed.packages()[,1])) {
    install.packages("dplyr", dep = TRUE)
    require("dplyr", character.only = TRUE)
  } else {
    require("dplyr", character.only = TRUE)
  }

  if (!is.element("ggplot2", installed.packages()[,1])) {
    install.packages("ggplot2", dep = TRUE)
    require("ggplot2", character.only = TRUE)
  } else {
    require("ggplot2", character.only = TRUE)
  }

  if (!is.element("magrittr", installed.packages()[,1])) {
    install.packages("magrittr", dep = TRUE)
    require("magrittr", character.only = TRUE)
  } else {
    require("magrittr", character.only = TRUE)
  }

  if (!is.element("stringr", installed.packages()[,1])) {
    install.packages("stringr", dep = TRUE)
    require("stringr", character.only = TRUE)
  } else {
    require("stringr", character.only = TRUE)
  }

  # > "2020-08-05" %>% wday(label = T)
  # [1] 週三
  # Levels: 週日 < 週一 < 週二 < 週三 < 週四 < 週五 < 週六
  if (!is.element("lubridate", installed.packages()[,1])) {
    install.packages("lubridate", dep = TRUE)
    require("lubridate", character.only = TRUE)
  } else {
    require("lubridate", character.only = TRUE)
  }

  if (!is.element("tidyr", installed.packages()[,1])) {
    install.packages("tidyr", dep = TRUE)
    require("tidyr", character.only = TRUE)
  } else {
    require("tidyr", character.only = TRUE)
  }

  if (!is.element("quantmod", installed.packages()[,1])) {
    install.packages("quantmod", dep = TRUE)
    require("quantmod", character.only = TRUE)
  } else {
    require("quantmod", character.only = TRUE)
  }

  # 可以處理EXCEL資料
  if (!is.element("openxlsx", installed.packages()[,1])) {
    install.packages("openxlsx", dep = TRUE)
    require("openxlsx", character.only = TRUE)
  } else {
    require("openxlsx", character.only = TRUE)
  }
  # 調整 tibble 顯示資料的長度
  # options(tibble.print_min = 50)
  if (!is.element("tibble", installed.packages()[,1])) {
    install.packages("tibble", dep = TRUE)
    require("tibble", character.only = TRUE)
  } else {
    require("tibble", character.only = TRUE)
  }

  if (!is.element("highcharter", installed.packages()[,1])) {
    install.packages("highcharter", dep = TRUE)
    require("highcharter", character.only = TRUE)
  } else {
    require("highcharter", character.only = TRUE)
  }

  if (!is.element("DT", installed.packages()[,1])) {
    install.packages("DT", dep = TRUE)
    require("DT", character.only = TRUE)
  } else {
    require("DT", character.only = TRUE)
  }

  na.replace <- function(object, replacement) {
    na.index <- is.na(object)
    object[na.index] <- replacement
    return(object)
  }

  comma <- function(x) format(x, digits = 2, big.mark = ",")
  # comma(3452345)
  # #> [1] "3,452,345"
  # comma(.12358124331)
  # #> [1] "0.12"

}
