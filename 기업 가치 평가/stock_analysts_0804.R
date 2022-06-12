#########################################################################################################
# 0. 필요한 패키지 설치 

setwd("C:/Users/user/Desktop/단기과정/corpvalue")

source("prescribed_0804.R")

#########################################################################################################
# 1. stockcrawl_last

stockcrawl_last("068270", "셀트리온")

#########################################################################################################
# 2. stockcrawl

tseries <- stockcrawl("068270", "셀트리온", 10)

plot(tseries)

#########################################################################################################
# 3. stockcrawl_adj

tseries_adj <- stockcrawl_adj_new("068270", "셀트리온",0) 

plot(tseries_adj[,1])

#########################################################################################################
# 4. stock_code_list &  stockcrawl_adj_new 반복적용 

port.open()

ind_key <- c("전체")

stock_list_tot <- stock_code_list(ind_key)

ind_key <- c("의약", "의료", "생물")

stock_list <- stock_code_list(ind_key)

port.close() 

stock_data <- data.frame()

for (i in 1:nrow(stock_list)){ # i <- 1
  
  print(t(c(stock_list[i,"종목코드"], stock_list[i,"기업명"])))

  temp <- stockcrawl_adj_new(stock_list[i,"종목코드"], 
                             stock_list[i,"기업명"], 0) 
  
  temp1 <- data.frame(
    종목코드 = as.character(stock_list[i,"종목코드"]), 
    기업명 = as.character(stock_list[i,"기업명"]),
    temp[nrow(temp),])
 
  row.names(temp1) <- NULL 
  
  stock_data <- rbind(stock_data, temp1) 
  
}

#########################################################################################################
# 5. Tile Chart 

## 업종코드 붙이기 

sf1 <- as.data.frame(left_join(stock_data, stock_list)) 

## 전체에 대한 평균 수익률과 거래량 합 계산

sf2 <- data.frame(수익률 = weighted.mean(sf1[,5], sf1[,4]), 거래량 = sum(sf1[,4])) 

## 중분류산업명별로 평균 수익률과 거래량 합 계산

sf3 <- sf1 %>% group_by(업종) %>% 
  summarise(수익률 = weighted.mean(수익률, 거래량), 거래량 = sum(거래량)) %>% 
  select(업종, 수익률, 거래량)

names(sf3) <- c("하위수준","수익률", "거래량") 

## googleVis의 gvisTreeMap을 그리기 위해 데이터셋 생성 

sf4 <- data.frame(상위수준 = NA, 하위수준 = "전체", sf2) 

sf5 <- data.frame(상위수준 = "전체", sf3) 

sf6 <- data.frame(상위수준=sf1[,8], 하위수준 = sf1[,2], sf1[,c(5,4)]) 

sf7 <- rbind(sf4, sf5, sf6)

# 색“Set1”에서 3가지 색을 가져와 col에 저장 

col <- brewer.pal(3,"Set1")

## gvisTreeMap에 활용할 “,”로 연결된 문자객체 스트링 col1 생성

col1 <- paste(col,collapse=",") 

## gvisTreeMap 함수를 이용하여 그림정보를 산출하고 Tree에 저장 

Tree <- gvisTreeMap(data=sf7, idvar="하위수준", parentvar="상위수준", sizevar="거래량", colorvar="수익률",
                    options=list(width=1200, height=800,
                                 fontSize=12,
                                 colors=col1,  
                                 headerHeight=20,
                                 fontColor='black',
                                 showScale=TRUE))       

## 해당 Tree는 인코딩 방식이 UTF-8로서 한글이 그림에 
## 나타나게 하기 위해서는 Tree파일에 대한변환 수행 

Tree$html$header <- gsub("utf-8", "euckr", Tree$html$header)   

## 그림 출력(웹으로 출력) 

plot(Tree)

########################################################################################
# 6. 개별기업 FCF 평가 

# 매크로 지표 가져오기 

data_macro <- macro_crawl()

market.return <- data_macro[1]

risk.free.rate <- data_macro[2] 

# 개별기업 평가하기 

data.fcf <- fcf_value("068270", "셀트리온", market.return, risk.free.rate)

data.fcf <- fcf_value("085660", "차바이오텍", market.return, risk.free.rate)

########################################################################################
# 7. 개별기업 PER, PBR, PCFR 가져오기  

market_value <- rel_value("085660", "차바이오텍")

########################################################################################
# 8. 뉴스분석 

news_content <- news_crawl("068270", "셀트리온",0)  

# 워드클라우드 그리기 

x <- news_content[[1]]

useNIADic() 

x1 <- lapply(x, extractNoun)
x2 <- lapply(x1, function(x) x[nchar(x)>1])
x3 <- do.call(c, x2)
o <- table(x3)

pal <- brewer.pal(8,"Dark2")

wordcloud(names(o), o, min.freq=1, random.order=F,random.color=T,colors=pal,family="AppleGothic")

# 새로운 그림 창이 필요할 경우에는 dev.new() 

########################################################################################
# 9. ESG 스코어 관계 분석 

# 수정주가 가져와서 월말 수정주가 구하기  

data.price.adj_new <- stockcrawl_adj_new("068270", "셀트리온", 0) 

sadjm <- to.monthly(data.price.adj_new)[,4]

# ESG 스코어 구하기 

# URL 생성 

url_naver_finance_news_code <- "http://finance.naver.com/item/news_news.nhn?code=%s&page=%s"  

# 네이버파이낸스에서 뉴스 가져오기 

news_content <- news_crawl("068270", "셀트리온",0)   

# 부정어휘 키워드 읽기 

keyword <- read.xlsx2("ESG.xlsx", sheetIndex = 1)
kw <- keyword[,2]

# 명사만 추출하여 정리 -> 시간이 많이 소요됨 

news_term <- lapply(news_content, extractNoun)

# 개별 뉴스의 어휘들로부터 부정어휘개수가 몇개인지를 카운트함  

tf <- c()
date <- c() 

for(i in 2019:length(news_content)){  # i <- 1
  
  print(i)
  
  tf[i] <- sum(sapply(kw, function(x) length(grep(x, news_term[[i]])))) 
  
  date[i] <- news_content[[i]][,1]

}

# 날짜와 부정어휘 숫자들을 데이터 프레임으로 생성 

esg.data <- data.frame(date=date, tf=tf)

# 월별 스코어 만들기 

# 날짜를 데이트형식으로 바꾸기 

esg.data$date <- as.Date(substring(str_trim(esg.data$date),1,10), "%Y.%m.%d") 

# 월별로 바꾸기 

esg.data$month <- format(esg.data$date, "%Y.%m")  

# 월별로 스코어 합 구하기 

esg.data1 <- as.data.frame(esg.data %>% group_by(month) %>% summarise(sumtf = sum(tf))) 

# 수정주가와 결합 

sadjm1 <- as.numeric(sadjm) 

month <- format(index(sadjm), "%Y.%m")  

ss <- data.frame(month=month, adjp = sadjm1)

# 수정주가와 ESG스코어 결합 

df1 <- inner_join(ss, esg.data1)

t1 <- as.Date(paste0(df1$month,".01"), "%Y.%m.%d") 
month(t1) <- month(t1) + 1
t1 <- t1 -1 

df1$date <- t1
 
# 패턴에 대한 확인 

p1 <- ggplot(df1, aes(x=date, y=sumtf)) +
  geom_line(color="red") + geom_point(color="red") + 
  ggtitle("ESG Score") 

p2 <- ggplot(df1, aes(x=date, y=adjp)) +
  geom_line(color="blue") + geom_point(color="blue") + 
  ggtitle("adjusted price") 

grid.arrange(p1, p2, nrow = 2)

# 회귀분석 수행

model <- lm(df1$adjp ~ lag(df1$sumtf,3))

summary(model)  

# 전체파일 저장하기 

save.image("temp.Rdata")

# 전체파일 불러들이기 

load("temp.Rdata")



