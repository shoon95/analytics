#  ÆÑÅÍÀÇ ÀÚµ¿º¯È¯À» ¹æÁöÇÏ´Â ÇÔ¼ö 

options(stringsAsFactors = FALSE)

# ÆÐÅ°Áö¸¦ ¼³Ä¡ÇÏ°í ½ÇÇàÇÏ´Â ÇÔ¼ö 

pkgload <- function(x){
  for(i in x){
    while(require(i, character.only = TRUE)==FALSE){
      install.packages(i)
    }   
  }
}

x <- c("KoNLP", "dplyr", "rvest", "httr", "stringr", "googleVis", "RColorBrewer", 
       "knitr", "xlsx", "PortfolioAnalytics", "jsonlite", "wordcloud", "RSelenium",
       "lubridate", "xts", "quantmod", "PerformanceAnalytics", "ggplot2", "gridExtra", 
       "EnvStats", "tseries", "XML")

#  ÆÐÅ°Áö ½ÇÇà 

pkgload(x)

# ÁÖ°¡ ÃÖ±Ù °ªÀ» °¡Á®¿À´Â ÇÔ¼ö 

stockcrawl_last <- function(code, name){  # code <- "068270"; name <- "¼¿Æ®¸®¿Â" 
  
  url_base <- paste0("http://finance.naver.com/item/sise_day.nhn?code=", code, "&page=1") 
  
  contents <- GET(url_base) 
  
  all.price <- read_html(contents) %>% html_nodes('table') %>% .[1] %>% html_table() 
  
  all.price <- data.frame(all.price) %>% filter(!(³¯Â¥=="")) 
  
  all.price <- all.price  %>% mutate(³¯Â¥ = as.Date(³¯Â¥, format="%Y.%m.%d"))
  
  for(i in 2:ncol(all.price)){
    all.price[,i] <- as.numeric(gsub(",","",all.price[,i])) 
  }
  
  all.price <- data.frame(Á¾¸ñÄÚµå = code, ±â¾÷¸í = name, all.price[,c("³¯Â¥", "Á¾°¡", "ÀüÀÏºñ", "°Å·¡·®")])
  
  return(all.price[1,])
  
}

# ÁÖ°¡ ÀüÃ¼¸¦ °¡Á®¿À´Â ÇÔ¼ö 

stockcrawl <- function(code, name, number=0){  # code <- "068270"; name <- "¼¿Æ®¸®¿Â" ; number <- 10
  
  url_base <- paste0("http://finance.naver.com/item/sise_day.nhn?code=", code, "&page=", 1) 
  
  contents <- GET(url_base) 
  
  # ¸¶Áö¸· ÆäÀÌÁö¸¦ °¡Á®¿À±â 
  
  pg.last <- strsplit(read_html(contents) %>% 
    html_nodes("td.pgRR a") %>% html_attr("href"), "&")[[1]][2] 
  pg.last <- strsplit(pg.last, "=")[[1]][2]

  # 0ÀÎ °æ¿ì´Â ¸¶Áö¸· ÆäÀÌÁö ±×·¸Áö ¾ÊÀº °æ¿ì´Â °³¼ö¸¸Å­ °¡Á®¿À±â  
  
  if(number == 0){
    pg.last <- pg.last
  }else pg.last <- as.integer(number/10)  
  
  tot.price <- data.frame() 
  
  for (page in 1:pg.last){ # page <- 1
    
    print(page)
    
    url_base <- paste0("http://finance.naver.com/item/sise_day.nhn?code=", code, "&page=", page)  
    
    contents <- GET(url_base) 
    
    content(contents)
    
    all.price <- read_html(contents) %>% html_nodes('table') %>% .[1] %>% html_table() 
    
    all.price <- data.frame(all.price) %>% filter(!(³¯Â¥=="")) 
    
    all.price <- all.price  %>% mutate(³¯Â¥ = as.Date(³¯Â¥, format="%Y.%m.%d"))
    
    for(i in 2:ncol(all.price)){
      all.price[,i] <- as.numeric(gsub(",","",all.price[,i])) 
    }
    
    all.price <- data.frame(Á¾¸ñÄÚµå = code, ±â¾÷¸í = name, all.price[,c("³¯Â¥", "Á¾°¡", "ÀüÀÏºñ", "°Å·¡·®")])
    
    tot.price <- rbind(tot.price, all.price)
    
  }
  
  tot.price <- tot.price %>% arrange(³¯Â¥)
  
  tseries <- xts(tot.price[,"Á¾°¡"], order.by=tot.price[,"³¯Â¥"])
  
  colnames(tseries) <- "Á¾°¡"
  
  return(tseries)
  
}

# ¼öÁ¤ ÁÖ°¡ ÀüÃ¼¸¦ °¡Á®¿À´Â ÇÔ¼ö 

stockcrawl_adj_new <- function(code, name, number=0){  # code <- "033270" ; name <- "À¯³ªÀÌÆ¼µå" 
  
  url <- paste0("https://fchart.stock.naver.com/sise.nhn?symbol=",code,"&timeframe=day&count=1000&requestType=0")
  
  contents <- GET(url)
  
  data <- read_html(iconv(contents,from = "ISO-8859-1", to = "UTF-8"), encoding = "utf8") %>% 
    html_nodes("item") %>% html_attr("data") 
  
  data1 <- data %>% strsplit('\\|')
  
  data2 <- lapply(data1, function(x) {x[c(1, 5, 6)] %>% t() %>% data.frame()})
  
  data3 <- do.call(rbind, data2)
  
  data3[,2] <- as.numeric(data3[,2])
  
  data3[,3] <- as.numeric(data3[,3])
  
  rownames(data3) <- ymd(data3[,1]) %>% as.character
  
  data3[,1] <- NULL
  
  # 0ÀÎ °æ¿ì´Â ¸¶Áö¸·±îÁö ±×·¸Áö ¾ÊÀº °æ¿ì´Â °³¼ö¸¸Å­ °¡Á®¿À±â  
  
  if(number == 0){
    data4 <- data3
  }else{
    data4 <- data3[(nrow(data3)-number):nrow(data3),]
  }
  
  data5 <- as.xts(data4)
  
  colnames(data5) <- c("¼öÁ¤ÁÖ°¡", "°Å·¡·®") 
  
  data5$return <- Delt(data5[,1]) 
  
  colnames(data5) <- c("¼öÁ¤ÁÖ°¡", "°Å·¡·®", "¼öÀÍ·ü")
  
  return(data5)
  
}

#  Æ÷Æ®¸¦ ¿©´Â ÇÔ¼ö 

port.open <- function(){
  
  portn <<- as.integer(runif(1, 1,5000)) 
  
  rD <<- rsDriver(port=portn, browser="chrome", chromever = "75.0.3770.142") 
  
  remDr <<- rD[["client"]]  

}

#  Æ÷Æ®¸¦ ´Ý´Â ÇÔ¼ö 

port.close <- function(){
  
  remDr$close()

  rD[["server"]]$stop()
  
}

#  ¾÷Á¾Å°¿öµå¿¡ ÇØ´çÇÏ´Â ¾÷Á¾¿¡ ¼ÓÇÏ´Â ±â¾÷¸®½ºÆ®¸¦ °¡Á®¿À´Â ÇÔ¼ö 

stock_code_list <-function(ind_key){
  
  remDr$navigate("http://marketdata.krx.co.kr/mdi#document=040601")
  
  webElem <- remDr$findElement("css", "div.design-center span.button-mdi-group button") 
  
  remDr$executeScript("$.down(this,'xls','formc81e728d9d4c2f636f067f89cc14862c')", 
                      args = list(webElem))
  
  Sys.sleep(20) # µ¥ÀÌÅÍ ÀúÀå ½Ã°£ °í·Á 
  
  stock_list0 <- readxl::read_excel("C:/Users/user/Downloads/data.xls", sheet = 1) 
  
  stock_list <- data.frame() 
  
  # "ÀüÃ¼"ÀÏ °æ¿ì´Â ¸ðµç ÄÚµå, ¾÷Á¾ÀÌ ¼±ÅÃµÈ °æ¿ì´Â ¾÷Á¾¸¸ °¡Á®¿À±â 
  
  for ( i in ind_key ){
    
    if( i == "ÀüÃ¼"){
      
      stock_list <- stock_list0
      
      break 
    
    }else{
      
      temp <- stock_list0 %>% filter(grepl(i, ¾÷Á¾) == TRUE )  
      
      stock_list <- rbind( stock_list, temp)     
    }

  }
  
  stock_list <- unique(stock_list)
  
  return(stock_list)
  
}

#  Àç¹«ÃßÁ¤°á°ú¸¦ °¡Á®¿À´Â ÇÔ¼ö - Á¦ÇÑµÈ ±â¾÷¸¸ Á¸ÀçÇÔ 

get_fin <- function(code){  # code <- "068270"; name <- "¼¿Æ®¸®¿Â" 
  
  url <- paste0("https://navercomp.wisereport.co.kr/v2/company/c1010001.aspx?cmp_cd=",code)
  
  remDr$navigate(url) 
  
  # url °¡Á®¿À±â 
  
  data <- GET(url)
  
  # html ÆÄÀÏ ÀÐ°í ÅØ½ºÆÄÀÏ °¡Á®¿À±â 
  
  data.text <- read_html(data) %>% html_text()
  
  # ¿¬°£½ÇÀû Å×ÀÌºíÀÇ encparam°ú id ¹øÈ£¸¦ °¡Á®¿À±â 
  
  data.param <- str_match(data.text, "encparam: '(.*)'")[2]
  
  data.id <- str_match(data.text, "id: '([a-zA-Z0-9]*)' ?")[2]
  
  # »õ·Î¿î url »ý¼º ¹× url °¡Á®¿À±â 
  
  url_fs <- paste0('https://navercomp.wisereport.co.kr/v2/company/ajax/cF1001.aspx?cmp_cd=', code, '&fin_typ=0&freq_typ=Y&encparam=',
                   data.param,"&id=",data.id)
  
  remDr$executeScript(paste0("window.open('",url_fs,"')")) 
  
  data_fs <- GET(url_fs)
  
  # /html/body/table[2] ·Î ½ÃÀÛµÇ´Â nodeÀÇ ³»¿ëÀ» °¡Á®¿À±â 
  
  data_table <- read_html(data_fs) %>%
    html_node(xpath = '/html/body/table[2]') %>%
    html_table(fill = TRUE)
  
  # ¿­ÀÌ¸§ »ý¼º 
  
  colnames(data_table) <- data_table[1, ]
  
  # Ã¹ÁÙ Á¦°Å 
  
  data_table <- data_table[-1, ]  
  
  # ÇàÀÇ ÀÌ¸§Àº NULL·Î Ã³¸® 
  
  rownames(data_table) <- NULL
  
  # Ã¹¹øÂ° ÄÃ·³À» ÇàÀÌ¸§À¸·Î Á¤ÀÇ 
  
  data_table <- tibble::column_to_rownames(data_table, var = 'ÁÖ¿äÀç¹«Á¤º¸')
  
  # ¹®ÀÚ¸¦ ¼ýÀÚ·Î ¹Ù²Ù±â À§ÇØ ","¸¦ Á¦°ÅÇÏ°í ¼ýÀÚ·Î º¯È¯ 
  
  for (j in 1:ncol(data_table)) {
    data_table[, j] = str_replace_all(data_table[, j], ',', '') %>% as.numeric()
  }
  
  # ÄÃ·³ÀÇ ÀÌ¸§¿¡¼­ Æ¯¼ö¹®ÀÚ Á¦°Å 
  # \r\n\t\t\t\t\t\t\t\t\t ¸¦ ³ªÅ¸³»´Â [\r\n\t+] ¸¦ È°¿ë 
  
  colnames(data_table) <- str_replace_all(colnames(data_table), "[\r\n\t+]" , "")
  
  # »ý¼ºµÈ Å×ÀÌºí ¸®ÅÏ   
  
  return(data_table)
  
}

#  Æ÷°ý¼ÕÀÍ°è»ê¼­¸¦ °¡Á®¿À´Â ÇÔ¼ö 

get_fin_is <- function(code){  # code <- "005930"
  
  url <- paste0("https://navercomp.wisereport.co.kr/v2/company/c1030001.aspx?cmp_cd=",code)
  
  remDr$navigate(url) 
  
  # url °¡Á®¿À±â 
  
  data <- GET(url)
  
  # html ÆÄÀÏ ÀÐ°í ÅØ½ºÆÄÀÏ °¡Á®¿À±â 
  
  data.text <- read_html(data) %>% html_text()
  
  # ¿¬°£½ÇÀû Å×ÀÌºíÀÇ encparam°ú id ¹øÈ£¸¦ °¡Á®¿À±â 
  
  data.param <- str_match(data.text, "encparam: '(.*)'")[2] 
  
  # ¿¬°£ Àç¹«Á¦Ç¥ URL »ý¼ºÇÏ°í NEW TAB¿¡¼­ º¸¿©ÁÖ±â  
  
  # »õ·Î¿î url »ý¼º ¹× url °¡Á®¿À±â 
  
  url_is <- paste0("https://navercomp.wisereport.co.kr/v2/company/cF3002.aspx?cmp_cd=", code, "&frq=0&rpt=0&finGubun=MAIN&frqTyp=0&cn=&encparam=", 
                   data.param)
  
  remDr$executeScript(paste0("window.open('",url_is,"')")) 
  
  Sys.sleep(1)
  
  data_is <- fromJSON(url_is)[["DATA"]] 
  
  data_is <- data_is[,c("ACKIND", "ACCODE", "ACC_NM", "DATA1", "DATA2", "DATA3", "DATA4", "DATA5")]
  
  colnames(data_is) <- tolower(c("ACKIND", "ACCODE", "ACC_NM", 
                                 gsub("<br />" , "", fromJSON(url_is)[["YYMM"]][1:5])))   
  
  data_is$acc_nm <- gsub("[.*]","",data_is$acc_nm) 
  
  return(data_is)
}

#  Àç¹«»óÅÂÇ¥¸¦ °¡Á®¿À´Â ÇÔ¼ö 

get_fin_fs <- function(code){  # code <- "005930"
  
  url <- paste0("https://navercomp.wisereport.co.kr/v2/company/c1030001.aspx?cmp_cd=",code)
  
  remDr$navigate(url) 
  
  # url °¡Á®¿À±â 
  
  data <- GET(url)
  
  # html ÆÄÀÏ ÀÐ°í ÅØ½ºÆÄÀÏ °¡Á®¿À±â 
  
  data.text <- read_html(data) %>% html_text()
  
  # ¿¬°£½ÇÀû Å×ÀÌºíÀÇ encparam°ú id ¹øÈ£¸¦ °¡Á®¿À±â 
  
  data.param <- str_match(data.text, "encparam: '(.*)'")[2]; data.param 
  
  # »õ·Î¿î url »ý¼º ¹× url °¡Á®¿À±â 
  
  # remDr$open()
  
  url_fs <- paste0("https://navercomp.wisereport.co.kr/v2/company/cF3002.aspx?cmp_cd=", code, "&frq=0&rpt=1&finGubun=MAIN&frqTyp=0&cn=&encparam=", 
                   data.param)
  
  remDr$executeScript(paste0("window.open('",url_fs,"')")) 
  
  Sys.sleep(1)
  
  data_fs <- fromJSON(url_fs)[["DATA"]] 
  
  data_fs <- data_fs[,c("ACKIND", "ACCODE", "ACC_NM", "DATA1", "DATA2", "DATA3", "DATA4", "DATA5")]
  
  colnames(data_fs) <- tolower(c("ACKIND", "ACCODE", "ACC_NM", 
                                 gsub("<br />" , "", fromJSON(url_fs)[["YYMM"]][1:5])))   
  
  data_fs$acc_nm <- gsub("[.*]","",data_fs$acc_nm) 
  
  return(data_fs)
} 

#  Çö±ÝÈå¸§Ç¥¸¦ °¡Á®¿À´Â ÇÔ¼ö 

get_fin_cf <- function(code){  # code <- "005930"
  
  url <- paste0("https://navercomp.wisereport.co.kr/v2/company/c1030001.aspx?cmp_cd=",code)
  
  remDr$navigate(url) 
  
  # url °¡Á®¿À±â 
  
  data <- GET(url)
  
  # html ÆÄÀÏ ÀÐ°í ÅØ½ºÆÄÀÏ °¡Á®¿À±â 
  
  data.text <- read_html(data) %>% html_text()
  
  # ¿¬°£½ÇÀû Å×ÀÌºíÀÇ encparam°ú id ¹øÈ£¸¦ °¡Á®¿À±â 
  
  data.param <- str_match(data.text, "encparam: '(.*)'")[2] 
  
  # »õ·Î¿î url »ý¼º ¹× url °¡Á®¿À±â 
  
  url_cf <- paste0("https://navercomp.wisereport.co.kr/v2/company/cF3002.aspx?cmp_cd=", code, "&frq=0&rpt=2&finGubun=MAIN&frqTyp=0&cn=&encparam=", 
                   data.param)
  
  remDr$executeScript(paste0("window.open('",url_cf,"')")) 
  
  Sys.sleep(1)
  
  data_cf <- fromJSON(url_cf)[["DATA"]] 
  
  data_cf <- data_cf[,c("ACKIND", "ACCODE", "ACC_NM", "DATA1", "DATA2", "DATA3", "DATA4", "DATA5")]
  
  colnames(data_cf) <- tolower(c("ACKIND", "ACCODE", "ACC_NM", 
                                 gsub("<br />" , "", fromJSON(url_cf)[["YYMM"]][1:5])))   
  
  data_cf$acc_nm <- gsub("[.*]","",data_cf$acc_nm)  
  
  return(data_cf)
}

#  º£Å¸¿Í ½Ã°¡ÃÑ¾×À» °¡Á®¿À´Â ÇÔ¼ö 

capital_beta <- function(code, name){ # code <- "176750"
  
  url <- paste0("https://navercomp.wisereport.co.kr/v2/company/c1010001.aspx?cmp_cd=",code)

  content <- GET(url)
  
  summary.table <-  read_html(content) %>%
    
    html_nodes('table') %>%
    
    .[2] %>%
    
    html_table()
  
  beta <- as.numeric(summary.table[[1]][6,2])
  
  nstock <- as.numeric( gsub("[,ÁÖ]","",unlist(strsplit(summary.table[[1]][7,2], "/"))[1]) )  
  
  # ½Ã°¡ÃÑ¾× (´ÜÀ§:¾ï¿ø)
  
  capital <- as.numeric(gsub("[,¾ï¿ø]","",summary.table[[1]][5,2]))   
  
  capital_result <- data.frame(
    Á¾¸ñÄÚµå = code, ¾÷Ã¼¸í = name, 
    º£Å¸ = beta, ¹ßÇàÁÖ½Ä¼ö = nstock, ½Ã°¡ÃÑ¾× = capital)  
  
  return(capital_result ) 
}

# KOSPI RAW µ¥ÀÌÅÍ¸¦ °¡Á®¿À´Â ÇÔ¼ö   

kospi.crawl <- function(number=0){  # number <- 100
  
  url <- paste0("https://finance.naver.com/sise/sise_index_day.nhn?code=KOSPI&page=",1)  
  
  content <- GET(url)
  
  # ¸¶Áö¸· ÆäÀÌÁö¸¦ °¡Á®¿À±â 
  
  pg.last <- strsplit(read_html(content) %>% 
                        html_nodes("td.pgRR a") %>% html_attr("href"), "&")[[1]][2] 
  pg.last <- strsplit(pg.last, "=")[[1]][2]
  
  # 0ÀÎ °æ¿ì´Â ¸¶Áö¸· ÆäÀÌÁö ±×·¸Áö ¾ÊÀº °æ¿ì´Â °³¼ö¸¸Å­ °¡Á®¿À±â  
  
  if(number == 0){
    pg.last <- as.numeric(pg.last)
  }else pg.last <- ceiling(number/6)  
  
  kospi.data <- data.frame() 
  
  t <- 0 
  
  for(i in 1:pg.last){  # i <- 1
    
    print(t)
    
    url <- paste0("https://finance.naver.com/sise/sise_index_day.nhn?code=KOSPI&page=",i)  
    
    content <- GET(url)
    
    # XPath À§Ä¡ È®ÀÎÇÏ¿© Å×ÀÌºí °¡Á®¿À±â 
    
    data_table <- read_html(content) %>%
      html_node(xpath = '/html/body/div/table[1]') %>%
      html_table(fill = TRUE) 
    
    # ³¯Â¥ ¾ø´Â °ÍÀº Á¦°Å 
    
    data_table <- data_table[!(data_table[,"³¯Â¥"]=="") & !is.na(data_table[,"³¯Â¥"]), ]  
    
    # ÇàÀÇ ÀÌ¸§Àº NULL·Î Ã³¸® 
    
    rownames(data_table) <- NULL
    
    data_table[,"µî¶ô·ü"] <- as.numeric(gsub("[%+-]","",data_table[,"µî¶ô·ü"]))*0.01
    
    # ¹®ÀÚ¸¦ ¼ýÀÚ·Î ¹Ù²Ù±â À§ÇØ ","¸¦ Á¦°ÅÇÏ°í ¼ýÀÚ·Î º¯È¯ 
    
    for (j in 2:ncol(data_table)) {
      data_table[, j] = str_replace_all(data_table[, j], ',', '') %>% as.numeric()
    }
    
    kospi.data <- rbind(kospi.data, data_table)
    
    t <- t + 1
    
    if ( t %% 100 == 0  ) Sys.sleep(2)
    
  }
  
  kospi.data <- kospi.data[1:number,]
  
  # ³¯Â¥Å¸ÀÔÀ¸·Î ¼öÁ¤ 
  
  kospi.data[,"³¯Â¥"] <- as.Date(kospi.data[,"³¯Â¥"], "%Y.%m.%d")
  
  # xts·Î º¯È¯ 
  
  kospi.xts <- as.xts(kospi.data[,2], order.by=kospi.data[,1])
  colnames(kospi.xts) <- "kospi"
  
  # ¿¬º°ÀÚ·á º¯È¯ÇÏ°í ÀÌ¸§ Á¤ÇÏ±â to.yearly(), to.monthly() ÇÔ¼ö È°¿ë 
  
  kospi.year <- to.yearly(kospi.xts)[,4]
  colnames(kospi.year) <- "kospi"
  
  # ¿¬°£¼öÀÍ·ü Æò±Õ ±¸ÇÏ±â, ¿¬°£¼öÀÍ·üÀÇ ±â´ë°ª ÃßÁ¤
  
  returns.data <- CalculateReturns(kospi.year[-length(kospi.year)])
  returns.data <- na.omit(returns.data)
  
  # Æò±Õ ±¸ÇØº¸±â  
  
  market.return <- mean(returns.data, trim=0.1)
  
  return(market.return)
  
}

# KOSPI ¼öÀÍ·ü°ú ¹«À§Çè±¹Ã¤ ¼öÀÍ·üÀ» ±¸ÇÏ´Â ÇÔ¼ö  

macro_crawl <- function(){
  
  message(sprintf("KOSPI¼öÀÍ·ü°ú ¹«À§Çè±¹Ã¤¼öÀÍ·üÀ» °¡Á®¿É´Ï´Ù. \n"))
  
  ########################################################################################
  # ½ÃÀåÆ÷Æ®Æú¸®¿À(KOSPI) Ã¼°á°¡°Ý ½ÃÀÛÆäÀÌÁö¿Í ³¡ ÆäÀÌÁö ¹Þ¾Æ¼­ ¼öÀÍ·ü ¸¸µé±â 
  
  kospi.data <- kospi.crawl(1300)
  
  market.return <- kospi.data 
  
  ########################################################################################
  # ¹«À§Çè ±¹Ã¤ ¼öÀÍ·ü °¡Á®¿À±â 
  
  sample <- "T1JKFDLBBNP41Q1S8NY0"
  
  url <- "http://ecos.bok.or.kr/api/StatisticSearch/sample/json/kr/1/10/060Y001/DD/20190401/20190420/010210000/"
  
  data_rf <- fromJSON(url)[[1]][[2]] 
  
  risk.free.rate <- as.numeric(data_rf[nrow(data_rf),"DATA_VALUE"])/100 
  
  data_macro <- data.frame(market.return=market.return, risk.free.rate=risk.free.rate)
  
}

# FCF¿¡ ±Ù°ÅÇÑ ÁÖ½Ä°¡Ä¡Æò°¡ ÇÔ¼ö 

fcf_value <- function(code, name, market.return, risk.free.rate){  
  
  # code <- "068270"; name <- "¼¿Æ®¸®¿Â" 
  # code <- "085660"; name <- "Â÷¹ÙÀÌ¿ÀÅØ"
  
  message("#################################################################\n",
          "#################################################################\n",
          sprintf("%s, %sÀÇ ºÐ¼®À» ½ÃÀÛÇÕ´Ï´Ù.\n", code, name))  
  
  ########################################################################################
  ## ³×ÀÌ¹ö ÆÄÀÌ³½½º url¿¡¼­ Àç¹«Á¤º¸ °¡Á®¿À±â  
  
  port.open() # Æ÷Æ®¸¦ ÇÏ³ª ¿­¾î¼­ Å©·Ò µå¶óÀÌ¹ö rD¿Í Å¬¶óÀÌ¾ðÆ® remDr ¼³Á¤  
  
  message("#################################################################\n",
          "#################################################################\n",
          sprintf("Àç¹«ÃßÁ¤Á¤º¸¸¦ °¡Á®¿É´Ï´Ù.\n"))
  
  data_table <- get_fin(code)  # Àç¹«ÃßÁ¤Á¤º¸ 
  
  message(sprintf("Æ÷°ý¼ÕÀÍ°è»ê¼­ Á¤º¸¸¦ °¡Á®¿É´Ï´Ù.\n"))
  
  data_is <- get_fin_is(code)  # Æ÷°ý¼ÕÀÍ°è»ê¼­  
  
  message(sprintf("Àç¹«»óÅÂÇ¥ Á¤º¸¸¦ °¡Á®¿É´Ï´Ù.\n"))
  
  data_fs <- get_fin_fs(code)  # Àç¹«»óÅÂÇ¥ 
  
  message(sprintf("Çö±ÝÈå¸§Ç¥ Á¤º¸¸¦ °¡Á®¿É´Ï´Ù.\n"))
  
  data_cf <- get_fin_cf(code)  # Çö±ÝÈå¸§Ç¥ 
  
  data_table2 <- rbind(data_is, data_fs, data_cf)
  
  port.close() # Å¬¶óÀÌ¾ðÆ® remDr°ú rD¸¦ ¸ðµÎ ´ÝÀ½    
  
  message(sprintf("°¡ÁßÆò±ÕÀÚº»ºñ¿ëÀ» °è»êÇÕ´Ï´Ù. \n"))
  
  ########################################################################################
  # º£Å¸, ÁÖ½Ä¼ö ¹× ½Ã°¡ÃÑ¾× °¡Á®¿À±â 
  
  capital_result <- capital_beta(code, name)
  
  beta <- capital_result[,"º£Å¸"]
  
  capital <- capital_result[,"½Ã°¡ÃÑ¾×"]
  
  ########################################################################################
  # CAPMÀ» È°¿ëÇÏ¿© ÀÚº»ºñ¿ë °è»ê 
  
  capital.cost <- risk.free.rate + beta*(market.return - risk.free.rate)
  
  ########################################################################################
  # Å¸ÀÎÀÚº»ºñ¿ë ¹× ºÎÃ¤ °¡Á®¿À±â 
  
  # ºÎÃ¤ÃÑ¾× °¡Á®¿À±â (´ÜÀ§: ¾ï¿ø)
  # ºÎÃ¤ÀÇ °æ¿ì¿¡´Â ÀåºÎ°¡³ª ½Ã°¡°¡ °ÅÀÇ Â÷ÀÌ¾ø´Ù°í °¡Á¤ÇÔ 
  # ½ÇÁ¦·Î ºÎµµÀ§ÇèÀÇ Áõ°¡°°Àº ÀÌº¥Æ®°¡ ¾ø´Â °æ¿ì¿¡´Â Ã¤±ÇÀÇ ½ÃÀå°¡Ä¡¿Í ÀåºÎ°¡Ä¡´Â °ÅÀÇ µ¿µî 
  
  # ÃÑºÎÃ¤ 
  
  debt <- data_table[rownames(data_table)=="ºÎÃ¤ÃÑ°è",5]
  
  # ÀÌÀÚºñ¿ë °¡Á®¿À±â 
  
  # ÀÌÀÚºñ¿ë °¡Á®¿À±â(´ÜÀ§: ¾ï¿ø) 
  int.cost <- as.numeric(data_is[data_is[,2]==202560,8])  
  
  # ÀÌÀÚºÎÃ¤ °¡Á®¿À±â(´ÜÀ§: ¾ï¿ø) : Å¸ÀÎÀÚº»ºñ¿ëÀ» ±¸ÇÏ±â À§ÇØ ½Ã±â ¸ÂÃã 
  
  adj_debt <- as.numeric(data_table[rownames(data_table)=="ÀÌÀÚ¹ß»ýºÎÃ¤",5])  
  
  # Å¸ÀÎÀÚº»ºñ¿ë 
  
  debt.cost <- int.cost/(adj_debt)
  
  ########################################################################################
  # °¡ÁßÆò±ÕÀÚº»ºñ¿ë 
  
  wacc <- debt.cost*debt/(debt+capital) + capital.cost*capital/(debt+capital)
  
  message(sprintf("ÁÖ½Ä°¡Ä¡¸¦ °è»êÇÕ´Ï´Ù. \n"))
  
  ########################################################################################
  # ±â¾÷°¡Ä¡ 
  
  fcf <- data_table[row.names(data_table)=="FCF",6:8] 
  
  # fcf°¡ ¾ø´Â °æ¿ì¿¡´Â °è»êÇÏ¿© ÃßÁ¤: Æò±Õ¼ºÀå·ü °è»ê  
  
  if (is.na(fcf[1])){
    
    gr <- t(data_table[row.names(data_table)=="FCF",3:5])
    gr <- mean(Delt(gr), na.rm=T)
    fcf <- data_table[row.names(data_table)=="FCF",3:5]*(1+gr)  
    
  } 
  
  # ±â¾÷°¡Ä¡´Â 2´Ü°è ¸ðÇüÀ» Àû¿ëÇÏ½Ã¿À. 
  # ¿©±â¼­ 2´Ü°è ¸ðÇüÀº ¹Ì·¡ 3°³³â µ¿¾ÈÀÇ ÇöÀç°¡Ä¡¿Í ±× ÀÌÈÄ´Â ¸í¸ñ°æÁ¦¼ºÀå·ü·Î ¼ºÀåÇÑ´Ù°í 
  # °¡Á¤ÇÏ´Â ÇüÅÂÀÓ 
  
  # ±â¾÷°¡Ä¡ 
  
  corp.value <- fcf[1]/(1+wacc) + fcf[2]/(1+wacc)^2 + fcf[3]/(1+wacc)^3 + fcf[3]*(1+0.025)/(wacc - 0.025)*1/(1+wacc)^3
  
  # ÀÚ±âÀÚº»ÀÇ °¡Ä¡ = ±â¾÷°¡Ä¡ - ºÎÃ¤°¡Ä¡ 
  
  capital.value <- as.numeric(corp.value) - debt 
  
  # ÁÖ½ÄÀÇ °³¼ö 
  
  nstock <- data_table[row.names(data_table)=="¹ßÇàÁÖ½Ä¼ö(º¸ÅëÁÖ)",5] 
  
  # ÁÖ½Ä°¡Ä¡ (´ÜÀ§: Ãµ¿ø) 
  
  stock.value <- capital.value / nstock * 100000 
  
  # ÁÖ½Ä°¡Ä¡Ãâ·Â 
  
  message(sprintf("%s, %sÀÇ Æò°¡µÈ ÁÖ°¡´Â %s(Ãµ¿ø)ÀÔ´Ï´Ù.",
                  code, name,
                  format(round(stock.value, 0), big.mark=","))) 
  
  return(stock.value)
  
}

# »ó´ë°¡Ä¡ÁöÇ¥¿¡ ÀÇÇÑ Æò°¡ 

rel_value <- function(code, name){
  
  # code <- "068270"; name <- "¼¿Æ®¸®¿Â" 
  # code <- "085660"; name <- "Â÷¹ÙÀÌ¿ÀÅØ"
  
  port.open()

  # ½Ã°¡ÃÑ¾×/´ç±â¼øÀÌÀÍ 
  
  capital_result <- capital_beta(code, name)
  
  data_is <- get_fin_is(code)
  
  capital_result[,"´ç±â¼øÀÌÀÍ"] <- data_is[data_is[,"accode"]=="203170",ncol(data_is)]
  
  capital_result[,"per"] <- capital_result[,"½Ã°¡ÃÑ¾×"]/capital_result[,"´ç±â¼øÀÌÀÍ"]
  
  # ½Ã°¡ÃÑ¾×/ÀÚ±âÀÚº» 
  
  data_fs <- get_fin_fs(code)
  
  capital_result[,"ÀÚº»ÃÑ°è"] <- data_fs[data_fs[,"accode"]=="120000",ncol(data_fs)]
  
  capital_result[,"pbr"] <- capital_result[,"½Ã°¡ÃÑ¾×"]/capital_result[,"ÀÚº»ÃÑ°è"]
  
  # ½Ã°¡ÃÑ¾×/¿µ¾÷Çö±ÝÈå¸§ 
  
  data_cf <- get_fin_cf(code)
  
  capital_result[,"¿µ¾÷È°µ¿À¸·ÎÀÎÇÑÇö±ÝÈå¸§"] <- 
    data_cf[data_cf[,"accode"]=="400000",ncol(data_cf)]
  
  capital_result[,"pcfr"] <- 
    capital_result[,"½Ã°¡ÃÑ¾×"]/capital_result[,"¿µ¾÷È°µ¿À¸·ÎÀÎÇÑÇö±ÝÈå¸§"]
  
  # µ¿ÀÏ¾÷Á¾  Á¾¸ñµéµµ µ¿ÀÏÇÑ ¹æ½ÄÀ¸·Î ±¸ÇÏ±â 
  
  dind <- stock_list_tot[stock_list_tot[,2]==code, 5] 
 
  ind_key <- c(dind)
  
  stock_list <- stock_list_tot[stock_list_tot[,5]==ind_key, ] 
  
  capital_all <- data.frame()
  
  for (i in 1:nrow(stock_list)){ # i <- 1
    
    remDr$open()
    
    ef <- as.character(c(stock_list[i,"Á¾¸ñÄÚµå"],stock_list[i,"±â¾÷¸í"]))
    
    print(ef) 
    
    # ½Ã°¡ÃÑ¾×/´ç±â¼øÀÌÀÍ 
    
    capital_result <- try(capital_beta(ef[1], ef[2]), silent = T)
    
    if( class(capital_result) == "try-error" ){
      
      capital_result <- data.frame(
        Á¾¸ñÄÚµå = ef[1], ¾÷Ã¼¸í = ef[2], º£Å¸=NA, ¹ßÇàÁÖ½Ä¼ö=NA,
        ½Ã°¡ÃÑ¾× = NA, ´ç±â¼øÀÌÀÍ = NA, per = NA, ÀÚº»ÃÑ°è = NA,
        pbr = NA, ¿µ¾÷È°µ¿À¸·ÎÀÎÇÑÇö±ÝÈå¸§ = NA, pcfr = NA 
      )
      
    }else{
      
      data_is <- get_fin_is(ef[1])
      
      capital_result[,"´ç±â¼øÀÌÀÍ"] <- data_is[data_is[,"accode"]=="203170",ncol(data_is)]
      
      capital_result[,"per"] <- capital_result[,"½Ã°¡ÃÑ¾×"]/capital_result[,"´ç±â¼øÀÌÀÍ"]
      
      # ½Ã°¡ÃÑ¾×/ÀÚ±âÀÚº» 
      
      data_fs <- get_fin_fs(ef[1])
      
      capital_result[,"ÀÚº»ÃÑ°è"] <- data_fs[data_fs[,"accode"]=="120000",ncol(data_fs)]
      
      capital_result[,"pbr"] <- capital_result[,"½Ã°¡ÃÑ¾×"]/capital_result[,"ÀÚº»ÃÑ°è"]
      
      # ½Ã°¡ÃÑ¾×/¿µ¾÷Çö±ÝÈå¸§ 
      
      data_cf <- get_fin_cf(ef[1])
      
      capital_result[,"¿µ¾÷È°µ¿À¸·ÎÀÎÇÑÇö±ÝÈå¸§"] <- 
        data_cf[data_cf[,"accode"]=="400000",ncol(data_cf)]
      
      capital_result[,"pcfr"] <- 
        capital_result[,"½Ã°¡ÃÑ¾×"]/capital_result[,"¿µ¾÷È°µ¿À¸·ÎÀÎÇÑÇö±ÝÈå¸§"]  
      
    }
    
    capital_all <- rbind(capital_all, capital_result)
    
    remDr$close()
    
    Sys.sleep(2)
  }
  
  port.close()
  
  ########################################################################################
  # ¾÷Á¾º° PER, PBR, PCFR ±¸ÇØ¼­ °³º° ±â¾÷ÀÇ ±â¾÷°¡Ä¡Æò°¡ 
  
  # hist(capital_all[,"pbr"])
  # hist(capital_all[,"per"])
  # hist(capital_all[,"pcfr"])
  
  # ¾÷Á¾º° PER, PBR, PCFR trimmed mean ±¸ÇÏ±â 
  
  mpbr <- mean(capital_all[,"pbr"], trim=0.2, na.rm=T)
  mper <- mean(capital_all[capital_all[,"per"]>0,"per"], trim=0.2, na.rm=T)
  mpcfr <- mean(capital_all[capital_all[,"pcfr"]>0,"pcfr"], trim=0.2, na.rm=T)
  
  # ¾÷Á¾ ÁÖ°¡ Æò°¡ 
  
  market_value <- capital_all
  
  market_value$mv_pbr <- 
    market_value[,"ÀÚº»ÃÑ°è"] * mpbr / market_value[,"¹ßÇàÁÖ½Ä¼ö"]*100000000
  
  market_value$mv_per <- 
    market_value[,"´ç±â¼øÀÌÀÍ"] * mper/ market_value[,"¹ßÇàÁÖ½Ä¼ö"]*100000000
  
  market_value$mv_pcfr <- 
    market_value[,"¿µ¾÷È°µ¿À¸·ÎÀÎÇÑÇö±ÝÈå¸§"] * mpcfr/ market_value[,"¹ßÇàÁÖ½Ä¼ö"]*100000000
  
  market_value$mv_pbr[market_value$mv_pbr<0] <- NA
  
  market_value$mv_per[market_value$mv_per<0] <- NA
  
  market_value$mv_pcfr[market_value$mv_pcfr<0] <- NA
  
  return(market_value)
  
} 

# ´º½ºÅ©·Ñ¸µ 

news_crawl <- function(code, name, number=0){
  
  # code <- "068270"; name <- "¼¿Æ®¸®¿Â" 
  # code <- "085660"; name <- "Â÷¹ÙÀÌ¿ÀÅØ"
  
  # URL »ý¼º 
  
  url_naver_finance_news_code <-
    "http://finance.naver.com/item/news_news.nhn?code=%s&page=%s"  
  
  page <- 1 
  
  r_url <- sprintf(url_naver_finance_news_code, code, page) 
  
  contents <- GET(r_url) 
  
  # ¸¶Áö¸· ÆäÀÌÁö¸¦ °¡Á®¿À±â 
  
  pg.last <- strsplit(read_html(contents) %>% 
                        html_nodes("td.pgRR a") %>% html_attr("href"), "&")[[1]][2] 
  pg.last <- strsplit(pg.last, "=")[[1]][2]
  
  # 0ÀÎ °æ¿ì´Â ¸¶Áö¸· ÆäÀÌÁö ±×·¸Áö ¾ÊÀº °æ¿ì´Â °³¼ö¸¸Å­ °¡Á®¿À±â  
  
  if(number == 0){
    pg.last <- pg.last
  }else pg.last <- number  
  
  # loop ¸¸µé¾î¼­ url list ¸¸µé±â  
  
  message("#################################################################\n",
          "#################################################################\n",
          sprintf("%s, %sÀÇ URLÀ» ¼öÁýÇÕ´Ï´Ù.\n", code, name))  
  
  
  page <- 1:pg.last
  
  news_url <- c() 
  
  for (page_num in page){ # page <- 1
    
    r_url <- sprintf(url_naver_finance_news_code, code, page_num) 
    
    contents <- GET(r_url) 
    
    html <- read_html(contents)
    
    temp <- unique(html_nodes(html, "table") %>%
                     html_nodes("a") %>% 
                     html_attr("href")) 
    
    news_url <- c(news_url, temp)
    print(page_num)
  }
  
  # ±â»ç ¾ÆÀÌµð°¡ Á¸ÀçÇÏ´Â URL¸¸ ¼±ÅÃ 
  
  news_url <- news_url[grepl("article_id=", news_url) == TRUE]  
  
  # ÄÁÅÙÃ÷ ¼öÁý 
  
  message("#################################################################\n",
          "#################################################################\n",
          sprintf("%s, %sÀÇ ÄÁÅÙÃ÷¸¦ ¼öÁýÇÕ´Ï´Ù.\n", code, name))  
  
  news_content <- list()
  
  for ( i in 1:length(news_url)){ # i <- 373
    
    print(i)
    
    n_url <- paste0("http://finance.naver.com",news_url[i]) 
    
    contents <- GET(n_url) 
    
    html <- read_html(contents, encoding="euckr")
    
    date <- html_nodes(html, xpath='//*[@class="tah p11"]')  
    date1 <-  html_text(date)
    
    html <- html_nodes(html, xpath='//*[@id="news_read"]')  
    temp <- html_text(html)
    
    x <- gsub("[^A-Za-z°¡-ÆR[:space:][:digit:][:punct:]]", "", temp)
    x <- gsub("@|\n|RT", "", x)
    x <- gsub("[[:punct:]]", " ", x)
    x <- gsub("[[:digit:]]", "", x)
    x <- tolower(x)
    x <- gsub("[a-z]", "", x)
    x <- gsub("[\t\n]", "", x)
    
    news <- cbind(date = date1, url = n_url, content = x)  
    
    news <- as.data.frame(news)
    
    news_content[[length(news_content)+1]] <- news
    
  }
  
  # Ãâ·Â ¹× È®ÀÎ 
  
  return(news_content)
  
}




