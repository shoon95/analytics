#################################################################################
# ÆÐÅ°Áö ¼³Ä¡ ¹× ½ÇÇà 

# install.packages("RSelenium")
# install.packages("rvest")
# install.packages("httr")
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("KoNLP")
# install.packages("RColorBrewer")
# install.packages("wordcloud")

library(KoNLP)
library(RSelenium)
library(rvest)
library(httr)
library(stringr)
library(dplyr)
library(RColorBrewer)
library(wordcloud)

#################################################################################
# ½ºÅ©·¡ÀÌÇÎ

# Æ÷Æ®¹øÈ£ ÇÒ´ç 

portn <- as.integer(runif(1, 1,5000)) 

# µå¶óÀÌ¹ö ¼³Á¤ 

rD <- rsDriver(port=portn, browser="chrome", chromever = "75.0.3770.8") 

# Å¬¶óÀÌ¾ðÆ® µå¶óÀÌ¹ö ÀÌ¸§ ¼³Á¤ 

remDr <- rD[["client"]] 

# ³×ÀÌ¹ö·Î±×ÀÎ È­¸éÀ¸·Î ÀÌµ¿ 

remDr$navigate("https://section.cafe.naver.com/")

# ¹öÆ° À§Ä¡ Ã£±â 

btn <- remDr$findElement(using="css selector", value="#gnb_login_button > span.gnb_txt") 

# ·Î±×ÀÎ ¹öÆ° Å¬¸¯ 

btn$clickElement() 

# ¾ÆÀÌµð¿Í ºñ¹Ð¹øÈ£ Ã£±â 

id <- remDr$findElement(using="id", value="id") 
pw <- remDr$findElement(using="id", value="pw") 

# ÀÔ·Â¶õ¿¡ ¾ÆÀÌµð¿Í ºñ¹Ð¹øÈ£ ÀÔ·ÂÇÏ±â 

id$setElementAttribute("value", "chapr1") 
pw$setElementAttribute("value", "ckzpdjtm")  

# ID ¹æ½ÄÀ¸·Î ¹öÆ° À§Ä¡ Ã£±â 

btn <- remDr$findElement(using="class", value="btn_global") 

# ·Î±×ÀÎ ¹öÆ° Å¬¸¯ 

btn$clickElement()  

# ºÒÀÓÀº ¾ø´Ù. ¸¾½ºÈ¦¸¯ ÆäÀÌÁö·Î ÀÌµ¿ 

remDr$navigate("https://cafe.naver.com/ArticleList.nhn?search.clubid=10094499&search.menuid=418&search.boardtype=L")
 
# iframe À¸·Î µé¾î°¡±â 

iframe <- remDr$findElement(using = "xpath", value = '//*[@id="cafe_main"]')

remDr$switchToFrame(iframe)

# ÆäÀÌÁö ÀüÃ¼ ¼Ò½º °¡Á®¿À±â

frontPage <- remDr$getPageSource()

# URL ¼öÁý 

html <- read_html(frontPage[[1]]) 

cafeURL <- html %>% html_nodes("a") %>% html_attr("href") 

cafeURL <- cafeURL[grepl("articleid=", cafeURL) == TRUE & grepl("referrerAllArticles=", cafeURL) == TRUE & 
                     grepl("commentFocus=true", cafeURL) == FALSE]  

cafeDate <- html %>% html_nodes(".td_date") %>% html_text()

# °øÁö³¯Â¥¸¸ Á¦°Å 

cafeDate <- cafeDate[-(1:21)]

# ÃÖ´ë ÆäÀÌÁö ÁöÁ¤ 

tnum <- 1000 

for (i in 1:tnum) { # i <- 288
  
  j <- ifelse(i > 11, i - 9 - 10*((i - 2)%/%10 - 1), i)
  print(c(i,j)) 
 
  # ¹öÆ° element Ã£±â   
  
  webElem <- remDr$findElements(using = 'xpath', value = paste0('//*[@id="main-area"]/div[7]/a[',j,']')) 
 
  if(length(webElem) == 0){
 
  }else{
    
    webElem[[1]]$clickElement()
    
    # Àá½Ã µ¿ÀÛ ¸ØÃã
    
    Sys.sleep(1) 
    
    # ÆäÀÌÁö ÀüÃ¼ ¼Ò½º °¡Á®¿À±â
    
    frontPage <- remDr$getPageSource() 
    
    html <- read_html(frontPage[[1]]) 
    
    cafeURLTemp <- html %>% html_nodes("a") %>% html_attr("href") 
    
    cafeURLTemp <- cafeURLTemp[grepl("articleid=", cafeURLTemp) == TRUE & grepl("referrerAllArticles=", cafeURLTemp) == TRUE & 
                                 grepl("commentFocus=true", cafeURLTemp) == FALSE] 
    
    cafeDateTemp <- html %>% html_nodes(".td_date") %>% html_text()
  
    # µ¥ÀÌÅÍ º´ÇÕ 
    
    cafeURL <- append(cafeURL, cafeURLTemp) 
    
    cafeDate <- append(cafeDate, cafeDateTemp) 
      
  }

}

# cafeURL
# cafeDate

cafeDate1 <- cafeDate 
cafeDate1[nchar(cafeDate1) == 5] <- "2019.07.31."

cafeDate2 <- cafeDate1
cafeDate2 <- substr(cafeDate2,1,8) == "2019.07."

table(cafeDate2)

cafeURL2 <- cafeURL
cafeURL2 <- cafeURL2[cafeDate2]

# ÄÁÅÙÃ÷ ¼öÁý 

cafe_gen <- list() 

cafe_content <- list()

for ( i in 1:length(cafeURL2)){ # i <- 2
  
  print(i)
  
  n_url <- paste0("https://cafe.naver.com", cafeURL2[[i]])  

  remDr$navigate(n_url)
  
  rr <- try(remDr$getPageSource(), silent=TRUE)
  
  if(grepl("try-error", rr)){
    
    remDr$dismissAlert()
    
    date1 <- ""
    
    x <- "" 
    
    cafe <- cbind(date= date1, url = n_url)  
    
    cafe_gen[[length(cafe_gen)+1]] <- cafe
    
    cafe_content[[length(cafe_content)+1]] <-  x   
  
  }else{
    
    iframe <- remDr$findElement(using = "xpath", value = '//*[@id="cafe_main"]') 
    
    remDr$switchToFrame(iframe)
    
    frontPage <- remDr$getPageSource()
    
    html <- read_html(frontPage[[1]]) 
    
    date <- html_nodes(html, xpath='//*[@class="m-tcol-c date"]')  
    
    date1 <-  html_text(date)
    
    print(date1)
    
    html <- html_nodes(html, xpath='//*[@id="tbody"]/p') 
    
    temp <- html_text(html)
    
    x <- gsub("[[:punct:]]", "", temp) 
    x <- gsub("¤µ¤·¤·¤º", "¼­¿ï¿ªÂ÷º´¿ø", x)
    x <- gsub("¤¡¤¤¤º", "°­³²Â÷º´¿ø", x)
    x <- gsub("¤²¤§¤º", "ºÐ´çÂ÷º´¿ø", x)
    x <- gsub("¤µ¤·¤·Â÷", "¼­¿ï¿ªÂ÷º´¿ø", x)
    x <- gsub("¤¡¤¤Â÷", "°­³²Â÷º´¿ø", x)
    x <- gsub("¤²¤§Â÷", "ºÐ´çÂ÷º´¿ø", x)
    x <- gsub("¼­¿ï¿ª¤º", "¼­¿ï¿ªÂ÷º´¿ø", x)
    x <- gsub("ºÐ´ç¤º", "ºÐ´çÂ÷º´¿ø", x)
    x <- gsub("¼­¿ï¿ª¤º", "¼­¿ï¿ªÂ÷º´¿ø", x)
    x <- gsub("ºÐ´ç¤º", "ºÐ´çÂ÷º´¿ø", x)
    x <- gsub("°­³²¤º", "°­³²Â÷º´¿ø", x)
    x <- gsub("¼­¿ï¿ªÂ÷", "¼­¿ï¿ªÂ÷º´¿ø", x)
    x <- gsub("ºÐ´çÂ÷", "ºÐ´çÂ÷º´¿ø", x)
    x <- gsub("°­³²Â÷", "°­³²Â÷º´¿ø", x)
    x <- gsub("¤±¤©¤·", "¸¶¸®¾Æ", x)
    x <- gsub("¤µ¤µ¤±¤©¤·", "½Å¼³¸¶¸®¾Æ", x)
    x <- gsub("¤§¤¡¤±¤©¤·", "´ë±¸¸¶¸®¾Æ", x)
    x <- gsub("¤µ¤½¤±¤©¤·", "¼ÛÆÄ¸¶¸®¾Æ", x)
    x <- gsub("¤²¤µ¤±¤©¤·", "ºÎ»ê¸¶¸®¾Æ", x)
    x <- gsub("¤µ¤²¤±¤©¤·", "»óºÀ¸¶¸®¾Æ", x)
    x <- gsub("¤·¤µ¤±¤©¤·", "ÀÏ»ê¸¶¸®¾Æ", x)
    x <- gsub("¤§¤¸¤±¤©¤·", "´ëÀü¸¶¸®¾Æ", x)
    x <- gsub("¤µ¤¸¤±¤©¤·", "¼öÁö¸¶¸®¾Æ", x)
    x <- gsub("¤½¤º¤±¤©¤·", "ÆòÃÌ¸¶¸®¾Æ", x)
    x <- gsub("¤²¤º¤±¤©¤·", "ºÎÃµ¸¶¸®¾Æ", x)
    x <- gsub("¤µ¤µ¸¶¸®¾Æ", "½Å¼³¸¶¸®¾Æ", x)
    x <- gsub("¤§¤¡¸¶¸®¾Æ", "´ë±¸¸¶¸®¾Æ", x)
    x <- gsub("¤µ¤½¸¶¸®¾Æ", "¼ÛÆÄ¸¶¸®¾Æ", x)
    x <- gsub("¤²¤µ¸¶¸®¾Æ", "ºÎ»ê¸¶¸®¾Æ", x)
    x <- gsub("¤µ¤²¸¶¸®¾Æ", "»óºÀ¸¶¸®¾Æ", x)
    x <- gsub("¤·¤µ¸¶¸®¾Æ", "ÀÏ»ê¸¶¸®¾Æ", x)
    x <- gsub("¤§¤¸¸¶¸®¾Æ", "´ëÀü¸¶¸®¾Æ", x)
    x <- gsub("¤µ¤¸¸¶¸®¾Æ", "¼öÁö¸¶¸®¾Æ", x)
    x <- gsub("¤½¤º¸¶¸®¾Æ", "ÆòÃÌ¸¶¸®¾Æ", x)
    x <- gsub("¤²¤º¸¶¸®¾Æ", "ºÎÃµ¸¶¸®¾Æ", x)
    x <- gsub("¤¸¤·", "Á¦ÀÏº´¿ø", x)
    x <- gsub("¤º¤²¤·", "Â÷º´¿ø", x)
    x <- gsub("È«¾ç", "»ý¸®", x)
    x <- gsub("´ÜÈ£¹Ú", "ÀÓÅ×±âÇÑÁÙ", x)
    x <- gsub("¼É°ü", "½ÃÇè°ü", x)
    x <- gsub("È­À¯", "È­ÇÐÀûÀ¯»ê", x)
    x <- gsub("°èÀ¯", "°è·ùÀ¯»ê", x)
    x <- gsub("ÀÚÀÓ", "ÀÚ¿¬ÀÓ½Å", x)
    x <- gsub("¼÷Á¦", "¼º°ü°è", x)
    x <- gsub("³­Àú", "³­¼Ò±â´ÉÀúÇÏ", x)
    x <- gsub("½Å¶û", "³²Æí", x)
    x <- gsub("ÇÇ°Ë", "ÇÇ°Ë»ç", x)
    x <- gsub("Á÷Àå", "È¸»ç", x)
    x <- gsub("ÃÎÆÄ", "ÃÊÀ½ÆÄ", x)
    x <- gsub("[^A-Za-z°¡-ÆR[:space:][:digit:][:punct:]]", "", x) 
    x <- gsub("@|\n|RT", "", x)
    x <- gsub("[[:punct:]]", " ", x)
    x <- gsub("[[:digit:]]", "", x)
    x <- gsub("ÀÎ°øÂ÷", "ÀÎ°ø", x)
    x <- gsub("½Å¼±Â÷", "½Å¼±", x)
    x <- gsub("ÇÇ°ËÂ÷", "ÇÇ°Ë»ç", x)
    x <- gsub("³Ãµ¿Â÷", "³Ãµ¿", x)
    x <- tolower(x)
    x <- gsub("[a-z]", "", x)
    x <- gsub("[\t\n]", "", x)
    
    cafe <- cbind(date= date1, url = n_url)  
    
    cafe_gen[[length(cafe_gen)+1]] <- cafe
    
    cafe_content[[length(cafe_content)+1]] <-  x        
  }
}

# Å¬¾ÆÀÌ¾ðÆ® Á¾·á (ÀÛ¾÷ Á¾·á ÈÄ)

remDr$close()

# ¼­¹öÁ¾·á 

rD[["server"]]$stop()

########################################################

useNIADic() 

# ¸í»ç/ Çü¿ë»ç/ µ¿»ç ÃßÃâ
ko.words <- function(doc){
  d <- as.character(doc)
  pos <- paste(SimplePos09(d))
  extracted <- str_match(pos, '([°¡-ÆR0-9]+)/N')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}

# SimplePos22(doc)
# extractNoun(doc)
# doc <- "È¸¿ø´Ô Á¡½ÉÀº µµ½Ã¶ô ÁØºñÇÏ°Ú½À´Ï´Ù"
# doc <- "È¸¿ø´Ô Á¡½É °¨»çÇÕ´Ï´Ù"
# ko.words(doc) 

# https://www.rdocumentation.org/packages/tm/versions/0.7-6/topics/VectorSource

# install.packages("tm")
library(tm)

cps <- VCorpus(VectorSource(cafe_content))

# https://www.rdocumentation.org/packages/KoNLP/versions/0.80.1/topics/SimplePos09
# https://kbig.kr/portal/kbig/datacube/niadict.page
# https://www.rdocumentation.org/packages/stringr/versions/1.4.0/topics/str_match

tdm <- TermDocumentMatrix(cps,
                          control = list(weighting= weightBin, 
                                         tokenize=ko.words,
                                         removePunctuation = T,
                                         removeNumbers = T,
                                         stopwords = c()))

tdm <- as.matrix(tdm)
# View(tdm)

v <- sort(slam::row_sums(tdm), decreasing = T)

data <- data.frame(X=names(v),freq=v)

table(data$freq)

data1 <- data[data$freq>=10,]

# https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html

# install.packages("wordcloud2")
library(wordcloud2)

wordcloud2(data1)

wordcloud2(data1, color = "random-light", backgroundColor = "grey")

################################################################
# Å°¿öµå µ¥ÀÌÅÍ ºÒ·¯¿À±â 

# install.packages("openxlsx")
library("openxlsx")

# xls ÆÐÅ°Áö ÀÖ´Â °æ¿ì¿¡´Â detach 
# detach("package:xlsx", unload = TRUE)

keyword <- read.xlsx(xlsxFile="Å°¿öµå¼öÁ¤.xlsx", sheet=1, rows = c(1:271), cols=c(1),
                    colNames=TRUE)

################################################################
# TM¿¡¼­ keyword Á¸ÀçÇÏ´Â °Í¸¸ ³²±â±â 

freq.words <- tdm[row.names(tdm) %in% keyword$x3, ]
 
co.matrix <- freq.words %*% t(freq.words)

write.csv(co.matrix, "co.matrix.csv")

setwd("C:/Users/user/Desktop/´Ü±â°úÁ¤/¼­¿ï¿ªÂ÷º´¿ø")

# save.image(file="moms7adj.Rdata")

# load("moms7adj.Rdata")