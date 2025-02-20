#  팩터의 자동변환을 방지하는 함수 

options(stringsAsFactors = FALSE)

# 패키지를 설치하고 실행하는 함수 

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

#  패키지 실행 

pkgload(x)

# 주가 최근 값을 가져오는 함수 

stockcrawl_last <- function(code, name){  # code <- "068270"; name <- "셀트리온" 
  
  url_base <- paste0("http://finance.naver.com/item/sise_day.nhn?code=", code, "&page=1") 
  
  contents <- GET(url_base) 
  
  all.price <- read_html(contents) %>% html_nodes('table') %>% .[1] %>% html_table() 
  
  all.price <- data.frame(all.price) %>% filter(!(날짜=="")) 
  
  all.price <- all.price  %>% mutate(날짜 = as.Date(날짜, format="%Y.%m.%d"))
  
  for(i in 2:ncol(all.price)){
    all.price[,i] <- as.numeric(gsub(",","",all.price[,i])) 
  }
  
  all.price <- data.frame(종목코드 = code, 기업명 = name, all.price[,c("날짜", "종가", "전일비", "거래량")])
  
  return(all.price[1,])
  
}

# 주가 전체를 가져오는 함수 

stockcrawl <- function(code, name, number=0){  # code <- "068270"; name <- "셀트리온" ; number <- 10
  
  url_base <- paste0("http://finance.naver.com/item/sise_day.nhn?code=", code, "&page=", 1) 
  
  contents <- GET(url_base) 
  
  # 마지막 페이지를 가져오기 
  
  pg.last <- strsplit(read_html(contents) %>% 
    html_nodes("td.pgRR a") %>% html_attr("href"), "&")[[1]][2] 
  pg.last <- strsplit(pg.last, "=")[[1]][2]

  # 0인 경우는 마지막 페이지 그렇지 않은 경우는 개수만큼 가져오기  
  
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
    
    all.price <- data.frame(all.price) %>% filter(!(날짜=="")) 
    
    all.price <- all.price  %>% mutate(날짜 = as.Date(날짜, format="%Y.%m.%d"))
    
    for(i in 2:ncol(all.price)){
      all.price[,i] <- as.numeric(gsub(",","",all.price[,i])) 
    }
    
    all.price <- data.frame(종목코드 = code, 기업명 = name, all.price[,c("날짜", "종가", "전일비", "거래량")])
    
    tot.price <- rbind(tot.price, all.price)
    
  }
  
  tot.price <- tot.price %>% arrange(날짜)
  
  tseries <- xts(tot.price[,"종가"], order.by=tot.price[,"날짜"])
  
  colnames(tseries) <- "종가"
  
  return(tseries)
  
}

# 수정 주가 전체를 가져오는 함수 

stockcrawl_adj_new <- function(code, name, number=0){  # code <- "033270" ; name <- "유나이티드" 
  
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
  
  # 0인 경우는 마지막까지 그렇지 않은 경우는 개수만큼 가져오기  
  
  if(number == 0){
    data4 <- data3
  }else{
    data4 <- data3[(nrow(data3)-number):nrow(data3),]
  }
  
  data5 <- as.xts(data4)
  
  colnames(data5) <- c("수정주가", "거래량") 
  
  data5$return <- Delt(data5[,1]) 
  
  colnames(data5) <- c("수정주가", "거래량", "수익률")
  
  return(data5)
  
}

#  포트를 여는 함수 

port.open <- function(){
  
  portn <<- as.integer(runif(1, 1,5000)) 
  
  rD <<- rsDriver(port=portn, browser="chrome", chromever = "75.0.3770.142") 
  
  remDr <<- rD[["client"]]  

}

#  포트를 닫는 함수 

port.close <- function(){
  
  remDr$close()

  rD[["server"]]$stop()
  
}

#  업종키워드에 해당하는 업종에 속하는 기업리스트를 가져오는 함수 

stock_code_list <-function(ind_key){
  
  remDr$navigate("http://marketdata.krx.co.kr/mdi#document=040601")
  
  webElem <- remDr$findElement("css", "div.design-center span.button-mdi-group button") 
  
  remDr$executeScript("$.down(this,'xls','formc81e728d9d4c2f636f067f89cc14862c')", 
                      args = list(webElem))
  
  Sys.sleep(20) # 데이터 저장 시간 고려 
  
  stock_list0 <- readxl::read_excel("C:/Users/user/Downloads/data.xls", sheet = 1) 
  
  stock_list <- data.frame() 
  
  # "전체"일 경우는 모든 코드, 업종이 선택된 경우는 업종만 가져오기 
  
  for ( i in ind_key ){
    
    if( i == "전체"){
      
      stock_list <- stock_list0
      
      break 
    
    }else{
      
      temp <- stock_list0 %>% filter(grepl(i, 업종) == TRUE )  
      
      stock_list <- rbind( stock_list, temp)     
    }

  }
  
  stock_list <- unique(stock_list)
  
  return(stock_list)
  
}

#  재무추정결과를 가져오는 함수 - 제한된 기업만 존재함 

get_fin <- function(code){  # code <- "068270"; name <- "셀트리온" 
  
  url <- paste0("https://navercomp.wisereport.co.kr/v2/company/c1010001.aspx?cmp_cd=",code)
  
  remDr$navigate(url) 
  
  # url 가져오기 
  
  data <- GET(url)
  
  # html 파일 읽고 텍스파일 가져오기 
  
  data.text <- read_html(data) %>% html_text()
  
  # 연간실적 테이블의 encparam과 id 번호를 가져오기 
  
  data.param <- str_match(data.text, "encparam: '(.*)'")[2]
  
  data.id <- str_match(data.text, "id: '([a-zA-Z0-9]*)' ?")[2]
  
  # 새로운 url 생성 및 url 가져오기 
  
  url_fs <- paste0('https://navercomp.wisereport.co.kr/v2/company/ajax/cF1001.aspx?cmp_cd=', code, '&fin_typ=0&freq_typ=Y&encparam=',
                   data.param,"&id=",data.id)
  
  remDr$executeScript(paste0("window.open('",url_fs,"')")) 
  
  data_fs <- GET(url_fs)
  
  # /html/body/table[2] 로 시작되는 node의 내용을 가져오기 
  
  data_table <- read_html(data_fs) %>%
    html_node(xpath = '/html/body/table[2]') %>%
    html_table(fill = TRUE)
  
  # 열이름 생성 
  
  colnames(data_table) <- data_table[1, ]
  
  # 첫줄 제거 
  
  data_table <- data_table[-1, ]  
  
  # 행의 이름은 NULL로 처리 
  
  rownames(data_table) <- NULL
  
  # 첫번째 컬럼을 행이름으로 정의 
  
  data_table <- tibble::column_to_rownames(data_table, var = '주요재무정보')
  
  # 문자를 숫자로 바꾸기 위해 ","를 제거하고 숫자로 변환 
  
  for (j in 1:ncol(data_table)) {
    data_table[, j] = str_replace_all(data_table[, j], ',', '') %>% as.numeric()
  }
  
  # 컬럼의 이름에서 특수문자 제거 
  # \r\n\t\t\t\t\t\t\t\t\t 를 나타내는 [\r\n\t+] 를 활용 
  
  colnames(data_table) <- str_replace_all(colnames(data_table), "[\r\n\t+]" , "")
  
  # 생성된 테이블 리턴   
  
  return(data_table)
  
}

#  포괄손익계산서를 가져오는 함수 

get_fin_is <- function(code){  # code <- "005930"
  
  url <- paste0("https://navercomp.wisereport.co.kr/v2/company/c1030001.aspx?cmp_cd=",code)
  
  remDr$navigate(url) 
  
  # url 가져오기 
  
  data <- GET(url)
  
  # html 파일 읽고 텍스파일 가져오기 
  
  data.text <- read_html(data) %>% html_text()
  
  # 연간실적 테이블의 encparam과 id 번호를 가져오기 
  
  data.param <- str_match(data.text, "encparam: '(.*)'")[2] 
  
  # 연간 재무제표 URL 생성하고 NEW TAB에서 보여주기  
  
  # 새로운 url 생성 및 url 가져오기 
  
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

#  재무상태표를 가져오는 함수 

get_fin_fs <- function(code){  # code <- "005930"
  
  url <- paste0("https://navercomp.wisereport.co.kr/v2/company/c1030001.aspx?cmp_cd=",code)
  
  remDr$navigate(url) 
  
  # url 가져오기 
  
  data <- GET(url)
  
  # html 파일 읽고 텍스파일 가져오기 
  
  data.text <- read_html(data) %>% html_text()
  
  # 연간실적 테이블의 encparam과 id 번호를 가져오기 
  
  data.param <- str_match(data.text, "encparam: '(.*)'")[2]; data.param 
  
  # 새로운 url 생성 및 url 가져오기 
  
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

#  현금흐름표를 가져오는 함수 

get_fin_cf <- function(code){  # code <- "005930"
  
  url <- paste0("https://navercomp.wisereport.co.kr/v2/company/c1030001.aspx?cmp_cd=",code)
  
  remDr$navigate(url) 
  
  # url 가져오기 
  
  data <- GET(url)
  
  # html 파일 읽고 텍스파일 가져오기 
  
  data.text <- read_html(data) %>% html_text()
  
  # 연간실적 테이블의 encparam과 id 번호를 가져오기 
  
  data.param <- str_match(data.text, "encparam: '(.*)'")[2] 
  
  # 새로운 url 생성 및 url 가져오기 
  
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

#  베타와 시가총액을 가져오는 함수 

capital_beta <- function(code, name){ # code <- "176750"
  
  url <- paste0("https://navercomp.wisereport.co.kr/v2/company/c1010001.aspx?cmp_cd=",code)

  content <- GET(url)
  
  summary.table <-  read_html(content) %>%
    
    html_nodes('table') %>%
    
    .[2] %>%
    
    html_table()
  
  beta <- as.numeric(summary.table[[1]][6,2])
  
  nstock <- as.numeric( gsub("[,주]","",unlist(strsplit(summary.table[[1]][7,2], "/"))[1]) )  
  
  # 시가총액 (단위:억원)
  
  capital <- as.numeric(gsub("[,억원]","",summary.table[[1]][5,2]))   
  
  capital_result <- data.frame(
    종목코드 = code, 업체명 = name, 
    베타 = beta, 발행주식수 = nstock, 시가총액 = capital)  
  
  return(capital_result ) 
}

# KOSPI RAW 데이터를 가져오는 함수   

kospi.crawl <- function(number=0){  # number <- 100
  
  url <- paste0("https://finance.naver.com/sise/sise_index_day.nhn?code=KOSPI&page=",1)  
  
  content <- GET(url)
  
  # 마지막 페이지를 가져오기 
  
  pg.last <- strsplit(read_html(content) %>% 
                        html_nodes("td.pgRR a") %>% html_attr("href"), "&")[[1]][2] 
  pg.last <- strsplit(pg.last, "=")[[1]][2]
  
  # 0인 경우는 마지막 페이지 그렇지 않은 경우는 개수만큼 가져오기  
  
  if(number == 0){
    pg.last <- as.numeric(pg.last)
  }else pg.last <- ceiling(number/6)  
  
  kospi.data <- data.frame() 
  
  t <- 0 
  
  for(i in 1:pg.last){  # i <- 1
    
    print(t)
    
    url <- paste0("https://finance.naver.com/sise/sise_index_day.nhn?code=KOSPI&page=",i)  
    
    content <- GET(url)
    
    # XPath 위치 확인하여 테이블 가져오기 
    
    data_table <- read_html(content) %>%
      html_node(xpath = '/html/body/div/table[1]') %>%
      html_table(fill = TRUE) 
    
    # 날짜 없는 것은 제거 
    
    data_table <- data_table[!(data_table[,"날짜"]=="") & !is.na(data_table[,"날짜"]), ]  
    
    # 행의 이름은 NULL로 처리 
    
    rownames(data_table) <- NULL
    
    data_table[,"등락률"] <- as.numeric(gsub("[%+-]","",data_table[,"등락률"]))*0.01
    
    # 문자를 숫자로 바꾸기 위해 ","를 제거하고 숫자로 변환 
    
    for (j in 2:ncol(data_table)) {
      data_table[, j] = str_replace_all(data_table[, j], ',', '') %>% as.numeric()
    }
    
    kospi.data <- rbind(kospi.data, data_table)
    
    t <- t + 1
    
    if ( t %% 100 == 0  ) Sys.sleep(2)
    
  }
  
  kospi.data <- kospi.data[1:number,]
  
  # 날짜타입으로 수정 
  
  kospi.data[,"날짜"] <- as.Date(kospi.data[,"날짜"], "%Y.%m.%d")
  
  # xts로 변환 
  
  kospi.xts <- as.xts(kospi.data[,2], order.by=kospi.data[,1])
  colnames(kospi.xts) <- "kospi"
  
  # 연별자료 변환하고 이름 정하기 to.yearly(), to.monthly() 함수 활용 
  
  kospi.year <- to.yearly(kospi.xts)[,4]
  colnames(kospi.year) <- "kospi"
  
  # 연간수익률 평균 구하기, 연간수익률의 기대값 추정
  
  returns.data <- CalculateReturns(kospi.year[-length(kospi.year)])
  returns.data <- na.omit(returns.data)
  
  # 평균 구해보기  
  
  market.return <- mean(returns.data, trim=0.1)
  
  return(market.return)
  
}

# KOSPI 수익률과 무위험국채 수익률을 구하는 함수  

macro_crawl <- function(){
  
  message(sprintf("KOSPI수익률과 무위험국채수익률을 가져옵니다. \n"))
  
  ########################################################################################
  # 시장포트폴리오(KOSPI) 체결가격 시작페이지와 끝 페이지 받아서 수익률 만들기 
  
  kospi.data <- kospi.crawl(1300)
  
  market.return <- kospi.data 
  
  ########################################################################################
  # 무위험 국채 수익률 가져오기 
  
  sample <- "T1JKFDLBBNP41Q1S8NY0"
  
  url <- "http://ecos.bok.or.kr/api/StatisticSearch/sample/json/kr/1/10/060Y001/DD/20190401/20190420/010210000/"
  
  data_rf <- fromJSON(url)[[1]][[2]] 
  
  risk.free.rate <- as.numeric(data_rf[nrow(data_rf),"DATA_VALUE"])/100 
  
  data_macro <- data.frame(market.return=market.return, risk.free.rate=risk.free.rate)
  
}

# FCF에 근거한 주식가치평가 함수 

fcf_value <- function(code, name, market.return, risk.free.rate){  
  
  # code <- "068270"; name <- "셀트리온" 
  # code <- "085660"; name <- "차바이오텍"
  
  message("#################################################################\n",
          "#################################################################\n",
          sprintf("%s, %s의 분석을 시작합니다.\n", code, name))  
  
  ########################################################################################
  ## 네이버 파이낸스 url에서 재무정보 가져오기  
  
  port.open() # 포트를 하나 열어서 크롬 드라이버 rD와 클라이언트 remDr 설정  
  
  message("#################################################################\n",
          "#################################################################\n",
          sprintf("재무추정정보를 가져옵니다.\n"))
  
  data_table <- get_fin(code)  # 재무추정정보 
  
  message(sprintf("포괄손익계산서 정보를 가져옵니다.\n"))
  
  data_is <- get_fin_is(code)  # 포괄손익계산서  
  
  message(sprintf("재무상태표 정보를 가져옵니다.\n"))
  
  data_fs <- get_fin_fs(code)  # 재무상태표 
  
  message(sprintf("현금흐름표 정보를 가져옵니다.\n"))
  
  data_cf <- get_fin_cf(code)  # 현금흐름표 
  
  data_table2 <- rbind(data_is, data_fs, data_cf)
  
  port.close() # 클라이언트 remDr과 rD를 모두 닫음    
  
  message(sprintf("가중평균자본비용을 계산합니다. \n"))
  
  ########################################################################################
  # 베타, 주식수 및 시가총액 가져오기 
  
  capital_result <- capital_beta(code, name)
  
  beta <- capital_result[,"베타"]
  
  capital <- capital_result[,"시가총액"]
  
  ########################################################################################
  # CAPM을 활용하여 자본비용 계산 
  
  capital.cost <- risk.free.rate + beta*(market.return - risk.free.rate)
  
  ########################################################################################
  # 타인자본비용 및 부채 가져오기 
  
  # 부채총액 가져오기 (단위: 억원)
  # 부채의 경우에는 장부가나 시가가 거의 차이없다고 가정함 
  # 실제로 부도위험의 증가같은 이벤트가 없는 경우에는 채권의 시장가치와 장부가치는 거의 동등 
  
  # 총부채 
  
  debt <- data_table[rownames(data_table)=="부채총계",5]
  
  # 이자비용 가져오기 
  
  # 이자비용 가져오기(단위: 억원) 
  int.cost <- as.numeric(data_is[data_is[,2]==202560,8])  
  
  # 이자부채 가져오기(단위: 억원) : 타인자본비용을 구하기 위해 시기 맞춤 
  
  adj_debt <- as.numeric(data_table[rownames(data_table)=="이자발생부채",5])  
  
  # 타인자본비용 
  
  debt.cost <- int.cost/(adj_debt)
  
  ########################################################################################
  # 가중평균자본비용 
  
  wacc <- debt.cost*debt/(debt+capital) + capital.cost*capital/(debt+capital)
  
  message(sprintf("주식가치를 계산합니다. \n"))
  
  ########################################################################################
  # 기업가치 
  
  fcf <- data_table[row.names(data_table)=="FCF",6:8] 
  
  # fcf가 없는 경우에는 계산하여 추정: 평균성장률 계산  
  
  if (is.na(fcf[1])){
    
    gr <- t(data_table[row.names(data_table)=="FCF",3:5])
    gr <- mean(Delt(gr), na.rm=T)
    fcf <- data_table[row.names(data_table)=="FCF",3:5]*(1+gr)  
    
  } 
  
  # 기업가치는 2단계 모형을 적용하시오. 
  # 여기서 2단계 모형은 미래 3개년 동안의 현재가치와 그 이후는 명목경제성장률로 성장한다고 
  # 가정하는 형태임 
  
  # 기업가치 
  
  corp.value <- fcf[1]/(1+wacc) + fcf[2]/(1+wacc)^2 + fcf[3]/(1+wacc)^3 + fcf[3]*(1+0.025)/(wacc - 0.025)*1/(1+wacc)^3
  
  # 자기자본의 가치 = 기업가치 - 부채가치 
  
  capital.value <- as.numeric(corp.value) - debt 
  
  # 주식의 개수 
  
  nstock <- data_table[row.names(data_table)=="발행주식수(보통주)",5] 
  
  # 주식가치 (단위: 천원) 
  
  stock.value <- capital.value / nstock * 100000 
  
  # 주식가치출력 
  
  message(sprintf("%s, %s의 평가된 주가는 %s(천원)입니다.",
                  code, name,
                  format(round(stock.value, 0), big.mark=","))) 
  
  return(stock.value)
  
}

# 상대가치지표에 의한 평가 

rel_value <- function(code, name){
  
  # code <- "068270"; name <- "셀트리온" 
  # code <- "085660"; name <- "차바이오텍"
  
  port.open()

  # 시가총액/당기순이익 
  
  capital_result <- capital_beta(code, name)
  
  data_is <- get_fin_is(code)
  
  capital_result[,"당기순이익"] <- data_is[data_is[,"accode"]=="203170",ncol(data_is)]
  
  capital_result[,"per"] <- capital_result[,"시가총액"]/capital_result[,"당기순이익"]
  
  # 시가총액/자기자본 
  
  data_fs <- get_fin_fs(code)
  
  capital_result[,"자본총계"] <- data_fs[data_fs[,"accode"]=="120000",ncol(data_fs)]
  
  capital_result[,"pbr"] <- capital_result[,"시가총액"]/capital_result[,"자본총계"]
  
  # 시가총액/영업현금흐름 
  
  data_cf <- get_fin_cf(code)
  
  capital_result[,"영업활동으로인한현금흐름"] <- 
    data_cf[data_cf[,"accode"]=="400000",ncol(data_cf)]
  
  capital_result[,"pcfr"] <- 
    capital_result[,"시가총액"]/capital_result[,"영업활동으로인한현금흐름"]
  
  # 동일업종  종목들도 동일한 방식으로 구하기 
  
  dind <- stock_list_tot[stock_list_tot[,2]==code, 5] 
 
  ind_key <- c(dind)
  
  stock_list <- stock_list_tot[stock_list_tot[,5]==ind_key, ] 
  
  capital_all <- data.frame()
  
  for (i in 1:nrow(stock_list)){ # i <- 1
    
    remDr$open()
    
    ef <- as.character(c(stock_list[i,"종목코드"],stock_list[i,"기업명"]))
    
    print(ef) 
    
    # 시가총액/당기순이익 
    
    capital_result <- try(capital_beta(ef[1], ef[2]), silent = T)
    
    if( class(capital_result) == "try-error" ){
      
      capital_result <- data.frame(
        종목코드 = ef[1], 업체명 = ef[2], 베타=NA, 발행주식수=NA,
        시가총액 = NA, 당기순이익 = NA, per = NA, 자본총계 = NA,
        pbr = NA, 영업활동으로인한현금흐름 = NA, pcfr = NA 
      )
      
    }else{
      
      data_is <- get_fin_is(ef[1])
      
      capital_result[,"당기순이익"] <- data_is[data_is[,"accode"]=="203170",ncol(data_is)]
      
      capital_result[,"per"] <- capital_result[,"시가총액"]/capital_result[,"당기순이익"]
      
      # 시가총액/자기자본 
      
      data_fs <- get_fin_fs(ef[1])
      
      capital_result[,"자본총계"] <- data_fs[data_fs[,"accode"]=="120000",ncol(data_fs)]
      
      capital_result[,"pbr"] <- capital_result[,"시가총액"]/capital_result[,"자본총계"]
      
      # 시가총액/영업현금흐름 
      
      data_cf <- get_fin_cf(ef[1])
      
      capital_result[,"영업활동으로인한현금흐름"] <- 
        data_cf[data_cf[,"accode"]=="400000",ncol(data_cf)]
      
      capital_result[,"pcfr"] <- 
        capital_result[,"시가총액"]/capital_result[,"영업활동으로인한현금흐름"]  
      
    }
    
    capital_all <- rbind(capital_all, capital_result)
    
    remDr$close()
    
    Sys.sleep(2)
  }
  
  port.close()
  
  ########################################################################################
  # 업종별 PER, PBR, PCFR 구해서 개별 기업의 기업가치평가 
  
  # hist(capital_all[,"pbr"])
  # hist(capital_all[,"per"])
  # hist(capital_all[,"pcfr"])
  
  # 업종별 PER, PBR, PCFR trimmed mean 구하기 
  
  mpbr <- mean(capital_all[,"pbr"], trim=0.2, na.rm=T)
  mper <- mean(capital_all[capital_all[,"per"]>0,"per"], trim=0.2, na.rm=T)
  mpcfr <- mean(capital_all[capital_all[,"pcfr"]>0,"pcfr"], trim=0.2, na.rm=T)
  
  # 업종 주가 평가 
  
  market_value <- capital_all
  
  market_value$mv_pbr <- 
    market_value[,"자본총계"] * mpbr / market_value[,"발행주식수"]*100000000
  
  market_value$mv_per <- 
    market_value[,"당기순이익"] * mper/ market_value[,"발행주식수"]*100000000
  
  market_value$mv_pcfr <- 
    market_value[,"영업활동으로인한현금흐름"] * mpcfr/ market_value[,"발행주식수"]*100000000
  
  market_value$mv_pbr[market_value$mv_pbr<0] <- NA
  
  market_value$mv_per[market_value$mv_per<0] <- NA
  
  market_value$mv_pcfr[market_value$mv_pcfr<0] <- NA
  
  return(market_value)
  
} 

# 뉴스크롤링 

news_crawl <- function(code, name, number=0){
  
  # code <- "068270"; name <- "셀트리온" 
  # code <- "085660"; name <- "차바이오텍"
  
  # URL 생성 
  
  url_naver_finance_news_code <-
    "http://finance.naver.com/item/news_news.nhn?code=%s&page=%s"  
  
  page <- 1 
  
  r_url <- sprintf(url_naver_finance_news_code, code, page) 
  
  contents <- GET(r_url) 
  
  # 마지막 페이지를 가져오기 
  
  pg.last <- strsplit(read_html(contents) %>% 
                        html_nodes("td.pgRR a") %>% html_attr("href"), "&")[[1]][2] 
  pg.last <- strsplit(pg.last, "=")[[1]][2]
  
  # 0인 경우는 마지막 페이지 그렇지 않은 경우는 개수만큼 가져오기  
  
  if(number == 0){
    pg.last <- pg.last
  }else pg.last <- number  
  
  # loop 만들어서 url list 만들기  
  
  message("#################################################################\n",
          "#################################################################\n",
          sprintf("%s, %s의 URL을 수집합니다.\n", code, name))  
  
  
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
  
  # 기사 아이디가 존재하는 URL만 선택 
  
  news_url <- news_url[grepl("article_id=", news_url) == TRUE]  
  
  # 컨텐츠 수집 
  
  message("#################################################################\n",
          "#################################################################\n",
          sprintf("%s, %s의 컨텐츠를 수집합니다.\n", code, name))  
  
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
    
    x <- gsub("[^A-Za-z가-힣[:space:][:digit:][:punct:]]", "", temp)
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
  
  # 출력 및 확인 
  
  return(news_content)
  
}




