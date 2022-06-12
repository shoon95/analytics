ls()
rm(list = ls())

x <- c("rvest", "data.table", "pbapply", "dplyr",
       "xml2", "topicmodels", "tm", "quanteda",'quanteda.textstats','quanteda.textplots','quanteda.corpora',
       "parallel", "doParallel", "foreach",
       "ggplot2", "reshape", "openxlsx", "stm",
       "lubridate", "tidyr", "stringr", "igraph",'tidyverse')

#install.packages(x)
lapply(x, require, character.only = TRUE)

#setwd("C:\\Users\\user\\Desktop\\tm")

# path = 'C:\\Users\\sjhty\\Desktop\\r\\text mining'
#path <- "C:\\Users\\user\\Desktop\\tm"

setwd('D:\\R_Å©·Ñ¸µ\\³×ÀÌ¹ö ´º½º ¼öÁý')
data_ALL <- read.xlsx('¼öÁý.xlsx')
data_ALL=data_ALL[,c(1,2,3,5,6,7,8,9)]
data_ALL=data_ALL[-which(data_ALL['article']=='±â»ç ¾øÀ½'),]

data =read.xlsx('´Ü¾î.xlsx')
data1 = t(data)
data1 = data1[-1,]
data1=gsub('»óÇ°','Á¦Ç°',data1)
data1=gsub('°í°´','¼ÒºñÀÚ',data1)
data1=gsub('¼­¿ï½Ã','¼­¿ï',data1)
data1=gsub('¼­¿ïÆ¯º°½Ã','¼­¿ï',data1)
data1=gsub('¸ÅÀÏ°æÁ¦','',data1)

stop = c("°¡¿îµ¥","°¡Áö","°®Àº","°³±¹","°³³â","°³¼Ò","°æ¿ì","¸°³Ù","¸²ÇÈ","¸Å½º",
         "°³¿ù","°Â","°Å±â","°Å¸®","°Ü¸¦","±ºµ¥","±×°Å","±×°Í","±×°÷",'°ú¿¬','°è¼¼',
         "±×±îÁþ","±×³×","±×³à","±×³ð","±×´ë","±×·¡","±×·¡µµ",'Á¦°ø',"¿À´Ã","°æ¸®",
         "±×¼­","±×·¯³ª","±×·¯´Ï","±×·¯´Ï±î","±×·¯´Ù°¡","ÀÎÅÍºä","Æ¼¿£","ÀÓ¿µ¿õ",
         "±×·¯¸é","±×·¯¸é¼­","±×·¯¹Ç·Î","±×·¯ÀÚ","±×·±","·Î¶ó","Æ®¸®","¸¶Á¦","¾öÅÂ¿õ",
         "±×·±µ¥","±×·³","±×·¸Áö¸¸","±×·ç","±×¸®°í","±×¸®ÇÏ¿©","¿þ½º","±è¼­Çü",'±¤°í',
         "±×ºÐ","±×ÀÌ","±×ÂÊ","±Ù","±Ùµ¥","±Û½ê","±Û½ê¿ä","ÁøÇà","¼­ÇöÁø","À¯¿¬¼®",
         "³ª¸§","³ªÀ§","³²Áþ","³É","³ÊÈñ","´ë»ó","³×³ð","³à¼®","³âµµ","±èÈñ¾Ö",
         "³ð","´©±¸","´Ù¸¥","´Ù¸¸","´Þ·¯","´ç½Å","´ë·Î","´õ±¸³ª","´õ¿íÀÌ","±è¿¹¸²",
         "µÎ¼¼","µÎ¾î","µÕ","µí","µíÀÌ","µîµî","µîÁö","¿ÃÇØ","´ëÇÑ","ÀÌÁöÇý","±èÇÊ",
         "µû¶ó¼­","µû¸§","µûÀ§","µý","¶§¹®","¶Ç","¶Ç´Â","¶ÇÇÑ","ºÎ¹®","¾ßÀÚ","±è³ª¶ó",
         "¸¶´ç","¸¶·Ã","¸¶¸®","¸¸Å­","¸î","¸î¸î","¸ð±Ý","¸ðµç","¹«·Æ","¹«½¼","¹«¾ù","¹¹",
         "¹½","¹ÌÅÍ","¹×","¹Ù¶÷","¹ÙÄû","¹ßÂ¦","»·","»Ó","¼­³Ê","¼¼±â","¼¼¿µ","³ë¿µÁÖ",
         "¼é³×","½º¹«","»ç¶÷","½Ã°£","¾Æ³Ä","¾Æ´Ï","¾Æ´Ï¾ß","¾Æ¹«","¹«¸¨¾²","·Á¸é",
         "¾Æ¹«°³","¾Æ¹«·±","¾Æ¾Æ","¾ÆÀÌ","¾ÆÀÌ°í","¾ÆÀÌ±¸","»ç½Ç","¾Æ½Ã¾Æ°æÁ¦",
         "¾ê","¾î´À","¾îµð","¾î¸Ó","¾ðÁ¦","¿¡ÀÌ","¿£","¿©±â","¿©´À",'¹®µµ',"Á¶ÀÎ¼º",
         "¿©·¯","¿©·¯ºÐ","¿©º¸","¿©º¸¼¼¿ä","¿©Áö","¿ª½Ã","¿À·£","ÅëÇØ","¿þ½º","¿ÀÈ÷·Á",
         "¿Â°®","¿Ö³ÄÇÏ¸é","¿Ø","¿ì¸®","À¢","ÀÌ°Å","ÀÌ°Í","ÀÌ°÷","ÀÌ³ð","ÀÌ·¡","ÀÌ·±",
         "ÀÌ·±Àú·±","ÀÌ¸¥¹Ù","ÀÌ¸®ÇÏ¿©","ÀÌÂÊ","ÀÏ´ë","ÀÓ¸¶","ÀÚ½Å","¹ÎÅÂ±¸","±è¹Î¼­",
         "ÀÚ±â","ÀÚ³×","Àú°Í","Àú±â","Áö±Ý","Àú³ð","Àú·±","ÃÖ´ë","ÇöÀç","±èÁ¤½Ä","ÀúÂÊ",
         "ÀúÆí","ÀúÈñ","ÁÖ³â",'Á¾Á¾',"ÁÖÀÏ","ÁîÀ½","Áï","ÁÖ¸ñ","±è¸¸","Áö°æ","Áö³­",
         "Âë","Å«","Å³·Î¹ÌÅÍ","ÆÛ¼¾Æ®","ÇÏ±â¾ß","ÇÏ±ä","¼º·æ","ÇÏ¹°¸ç","°¡±â","ÀúÀÛ±Ç",
         "ÇÏÁö¸¸","ÇÑµÎ","ÇÑÆí","ÇãÇã","¹èÆ÷","ÀÛ±Ç","È¤Àº","ÀÖ´Ù","ÀÖ´Â","µîÀ»","ÇÑ°Ü·¹",
         "È¸Àå","µåµÈ","¹àÇû´Ù","ÇÔ²²","ÀÌ¹ø","Áö³­ÇØ","°¡Àå","ÇÑ´Ù","¸é¼­","°¡·®",
         "¸ð¹ÙÀÏ","Àç¹èÆ÷","¹«´ÜÀüÀç","¹«´Ü","ÀüÀç","°ÍÀÌ´Ù","°ü·Ã","¿¡¼­","¾ÞÄ¿",
         "¹®Áö¿µ","ÀÏ±îÁö","¶§¹®¿¡","°ü°èÀÚ´Â","¸¸µç","ÀÌµ¥ÀÏ¸®","¿ÀÀü","¾Æ·¡",
         "³×ÀÌ¹ö","ºÐ¾ç","µÎ»ê","Áß°ø¾÷","¼­¿ï½£","Á¶°¨µµ","·£µå¸¶Å©","¿ÀÈÄ","µîÀ¸·Î",
         "ÀúÀÛ±ÇÀÚ","´Ù½Ã","¸ÅÀÏ°æÁ¦","ÀÖ°í","¹ÌÀÌ","Çì·²µå","¾Æ½Ã¾Æ°æÁ¦","Áß½ÉÀ¸·Î",
         "¶ó´Â","Å©°Ô","¹Þ¾Ò´Ù","Àü±¹","´Þ¶ó","¸ð¹ÙÀÏ","¹Ù·Î","³â±îÁö","°¢°¢","µ¿½Ã¿¡",
         "ÀüÃ¼","±¸µ¶ÇÏ±â","¹èÆ÷","±×¶§",'Á÷Á¢',"¸Ó´ÏÅõµ¥ÀÌ","ÀÌ³¯","½±°Ô","ÁßÀÌ´Ù",
         "¸Å³â","¶ó°í","±ÝÁö","¸é¼­","Çì·²µå°æÁ¦","Å¸ÀÌ°¡","¼­¿ï°æÁ¦","ÆäÀÌ½ººÏ",
         "±îÁö","³â°£","¹Þ°í","ÀÏÀÌ",'¾ÆÆÄÆ®',"¿¬°£","¹ÙÅÁÀ¸·Î","º¸ÀÎ´Ù","ÀÖ¾ú´Ù",
         "¾ÕÀ¸·Î","ÀÖ°Ô",'³ª¼±','ÀÌ¹ö',"ÀÌ¿µÀÚ","µ¿»ý","¸Â¾Æ","±âÀÚ","¿ùºÎÅÍ",
         "¾Æ´Ñ","°Íµµ","ÀÖÀ»","¾ø´Ù","ÀÌ»óÀÇ",'º¸±â',"±è°æÈñ","ÀÌ¹Ì","ÄÚ¸®¾Æ",
         "ÃÖ°í","¹ÞÀº","¿­¸°","³âºÎÅÍ","³ôÀº","³×ÀÌ¹ö","ÀÌÈÄ","³×ÀÌ¹ö¿¡¼­","´ëÇ¥",
         "¿ø¿¡","Ã¤³Î","ÇÑ±¹°æÁ¦","ÀüÇß´Ù","´õ¿í","¿¬ÇÕ´º½º","ÄÚ¸®¾ÆÇì·²µå","´º½º",
         "°ü°èÀÚ","ÀÌ»ó","»ç¿ë","ÀÌ¿ë","¸ðµÎ","Á¤µµ","±¸µ¶","±ÝÁö","µ¿¾È","ºñ·Ô",
         "ÇÏ³ª","¼öÁØ","°¢Á¾","´ÜÁö","ÀÏ¹Ý","¸¹ÀÌ","ÀÌ¶ó´Â","ÆÄÀÌ³½¼È´º½º","¸¸¿¡",
         "´ëÇ¥´Â",'À§ÇØ','´ëÇØ','°æÇâ½Å¹®','Àç´Â','¶ó¸ç','´º½Ã½º','³×ÀÌ¹ö','±â»ç',
         'ÇÔºÎ·¯','ÀÏºÎ·¯','¹Ú¹Ì¶ó','ÀÌµ¥ÀÏ¸®','µ¿¾ÆÀÏº¸','ÆÄÀÌ³½¼È','ºñÁî½º',
         'ºñÁî´Ï½º','ÅäÀÏ','¸»Çß´Ù','°ÍÀ¸·Î','Çß´Ù','ÀÌ¶ó°í','À¸·Î','Æ¯È÷','ÀÖµµ·Ï',
         '°°Àº','ÀÌÁ¦','¿Ã¸²ÇÈ','Á¦Ç°','°ÍÀÌ','¸¹Àº','ÇÏ´Â','µîÀÇ','µû¶ó','µîÀÌ',
         'ÇÏ°í','¹Ù·Î°¡±â','ÀÌ¾î','¿À´Â','È°¿ëÇÑ','È°¿ëÇØ','ÀÖ¾î','¿ÍÀÏµåÇÃ¶ó¿ö¸°³Ù',
         '¸¶Æ¾','¿µ¼Û','ºò¹ð','ÅÂ¾ç','Æ®¿ÍÀÏ¶óÀÕ','¹ÎÈ¿¸°','¿þµù','ºÎµ¿»ê','¿ÀÇÇ½ºÅÚ',
         'È£ÅÚ','½Ã¼³','ÁÖ°Å','±Ô¸ð','¿¹Á¤','°¡±¸','¾ÓÄÚ¸£','ÁÖÅÃ','ÁÖ»ó','È­Àç','°ßº»ÁÖÅÃ',
         '°¡·Î¸·','°­º¯ºÏ·Î','°í´ö','Áö»ó','°Ç¼³','ÀÓ´ë','Á¶½Ä','Áö»ó')



num=0
for(i in stop){
  num=num+1
  now = paste0(num,'/',length(stop))
  print(now)
  data1 = gsub(i,'',data1)
}

data_ALL$text = data1



### ¸°³ÙÀÏ °æ¿ì 'Ä£È¯°æÀÇ·ù'->'¸°³Ù' À¸·Î º¯°æ
data_ALL=data_ALL %>% filter(name=='Ä£È¯°æÀÇ·ù')
data_ALL=tidyr::unite(data_ALL,col='date',year,month,day,sep='-',remove=FALSE)

dim(data_ALL)
str(data_ALL)


data_ALL$date2 <- ymd(data_ALL$date)
data_ALL$year <- year(data_ALL$date2)
data_ALL$month <- month(data_ALL$date2)
data_ALL$day <- day(data_ALL$date2)
str(data_ALL)
table(data_ALL$year)
data_2018 <- data_ALL %>%
  filter(data_ALL$year==2018)
data_2019 <- data_ALL %>%
  filter(data_ALL$year==2019)
table(data_2018$month)
table(data_2019$month)
# rdata <- data_ALL[,c(2:3,5:7)]
# 
# q=sapply(rdata['article'], function(x){gsub('+[-_¤Ó£üa-zA-Z0-9°¡-ÆR ]+[°¡-ÆR]{2,4}[ ±âÀÚ]+[ -_.+a-zA-Z0-9]+[@].+[.][[a-zA-Z0-9]+.*','',x)})
# e=sapply(q, function(x){gsub('¢º \\[ÀÎÅÍ·¢Æ¼ºê\\].*','',x)})
# rdata['article']=e


# compute diagnostic values for models with different topic numbers
# held-out likelihood: look for the higest value given a topic number
# residuals: look for the lowest given a topic number
# lower bound: look for the lowest given a topic number
# semantic coherence: look for the hightest given a topic number
# rdata$story<-gsub("\n", "", rdata$article)
# rdata$story<-gsub("¾Æ´Ï¶ó", "",  rdata$article)
# rdata$story<-gsub("ÀÖ½À´Ï´Ù", "",  rdata$article)
# rdata$story<-gsub("À§ÇÏ¿©", "",  rdata$article)
# rdata$story<-gsub("´ëÇØ¼­", "",  rdata$article)
# rdata$story<-gsub("ÀÖ¾î¼­", "",  rdata$article)
# rdata$story<-gsub("ÀÖµµ·Ï", "",  rdata$article)
# rdata$story<-gsub("¢Ñ .*", "",  rdata$article)
# rdata$story<-gsub("¢º .*", "",  rdata$article)
# rdata$story<-gsub("¨Ï .*", "",  rdata$article)
# rdata$story<-gsub('+[-_¤Ó£üa-zA-Z0-9°¡-ÆR ]+[°¡-ÆR]{2,4}[ ±âÀÚ]+[ -_.+a-zA-Z0-9]+[@].+[.][[a-zA-Z0-9]+.*','',rdata$article)
# #ºÒ¿ë¾î Á¦°Å ÆÄÀÏ »ý¼º
# 
write.csv(data_ALL$text, "afstopwords.csv")
# 
f <- read.csv("afstopwords.csv", header=T, stringsAsFactors=FALSE)
# 
class(f)

## ¸í»ç ÃßÃâ±â

# ko.words <- function(doc){
#   d <- as.character(doc)
#   extractNoun(d)
# }


fcorpus <- corpus(data_ALL$text)
e=tokens(fcorpus,remove_url=TRUE, remove_punct=TRUE,remove_number=TRUE)



# document feature matrix
dfm2 <- dfm(e, verbose = FALSE, tolower = TRUE, 
            remove_numbers = TRUE, remove_punct = TRUE,
            remove_symbols = TRUE, remove_separators = TRUE,
            remove = stop)%>% 
  dfm_select(min_nchar = 2L)


textstat_frequency(dfm2) 


word_fre<- textstat_frequency(dfm2) 
#write.csv(word_fre, "word_fre_final.csv")

dfm2@Dimnames$features
#write.csv(dfm2,"dfm2.csv")


dfm2.trim <- dfm_trim(dfm2, min_docfreq = 0.001, 
                      max_docfreq = .90, 
                      docfreq_type = "prop")


#memory.size()
#memory.limit()
#x = rep(0, 300000000)
#memory.size()
#y = rep(0, 300000000)
#memory.limit(5000)
#memory.limit()
#y = rep(0, 300000000)


dfm2.stm <- convert(dfm2.trim, to = "stm")
dfm2

f$article <- data_ALL$article
f$news <- data_ALL$news
f[1,1]

f2 <- f[order(data_ALL$text, decreasing = FALSE), ]
f2$year <- data_ALL$year
f2$month <- data_ALL$month
f2$day <- data_ALL$day
f2$kinds <- data_ALL$kinds

colnames(f2)[1:2]=c('documents','vocab')

dfm2.stm$meta <- f2
out <- dfm2.stm
docs <- out$documents
vocab <- out$vocab
meta <- out$meta


#meta

#f <- read.csv("ai_rawdata.csv", sheet = 1)


library(stm)


#¸ÞÅ¸ µ¥ÀÌÅÍ ÆÑÅÍ Ã³¸® (¿¬¼ÓÇü º¯¼ö ¿¹ÃøÀ» À§ÇØ ÆÑÅÍ Ã³¸® ÇÏÁö ¾ÊÀ½)

#str(rdata)
#rdata$year <- as.factor(rdata$year)
#rdata$month <- as.factor(rdata$month)

str(data_ALL)
# 
 K = c(5, 10, 15, 20, 25, 30,35)
 kresult <- searchK(out$documents, out$vocab, K,prevalence =~kinds+year, data = meta)
# 
 x11()
 plot(kresult)
### optimal k = ÆÄ¾Ç ¾î·Á¿ò 
#optimal k : 15·Î ¼³Á¤

#¸ðµ¨ ¸¸µé±â

out <-prepDocuments( docs,vocab, meta)


# model estimation
# agendaPrevFit <- stm(documents = out$documents, vocab = out$vocab,
#                      K = 15, 
#                      max.em.its = 75, data = out$meta,
#                      init.type = "Spectral")
agendaPrevFit <- stm(documents = out$documents, vocab = out$vocab,
                     K = 20, prevalence =~kinds+s(year),
                     max.em.its = 70,data=out$meta,
                     init.type = "Spectral")


# model selection
agendaSelect <- selectModel(out$documents, out$vocab, K = 20,
                            max.em.its = 70,
                            runs = 20, seed = 8458159)


# Plot of selectModel results
plotModels(agendaSelect, pch=c(1,2,3,4))

# ÅäÇÈº° ´Ü¾î 
labelTopics(agendaPrevFit,c(3,7,15))

# ÅäÇÈº° ¹®¼­
thoughts3 <- findThoughts(agendaPrevFit, texts = meta$article,
                          n = 2, topics = 3)$docs[[1]]
thoughts20 <- findThoughts(agendaPrevFit, texts = meta$article,
                            n = 2, topics = 15)$docs[[1]]

par(mfrow = c(1, 2),mar = c(.5, .5, 1, .5))
plotQuote(thoughts3, width = 30, main = "Topic 3")
plotQuote(thoughts20, width = 30, main = "Topic 20")

out$meta$kinds<-as.factor(out$meta$kinds)

# Estimating metadata/topic relationships
prep <- estimateEffect(1:20 ~kinds+s(year), agendaPrevFit,
                        meta = out$meta, uncertainty = "Global")

summary(prep,topics=5)

plot(agendaPrevFit, type = "summary", xlim = c(0, .3))

# kinds º¯¼ö¸¦ ÁßÁ¡À¸·Î stm ±¸Ãà
poliblogContent <- stm(out$documents, out$vocab, K = 20,
                        prevalenc= ~ kinds+s(year), content =~ kinds,
                        max.em.its = 70, data = out$meta, init.type = "Spectral")

# ¹æ¼Û/Åë½Å°ú °æÁ¦/IT °¡ ÅäÇÈ¿¡ ¹ÌÄ¡´Â ¿µÇâ
plot(prep, covariate = "kinds", topics = c(3, 7, 15),
      model = agendaPrevFit, method = "difference",
      main = "Effect of Liberal vs. Conservative",
       xlim = c(-.1, .1), 
      cov.value1 = "¹æ¼Û/Åë½Å", cov.value2 = "°æÁ¦/IT"
     , xlab = "More °æÁ¦/IT ... More ¹æ¼Û/Åë½Å")

out$meta$year=as.numeric((out$meta$year))

# year º¯¼ö°¡ ÅäÇÈ¿¡ ¹ÌÄ¡´Â ¿µÇâ
#excluded°¡ ³·Àº year¿¡¼­ ÇØ´ç ÅäÇÈÀÌ peak°¡ µÊ 
x11()
plot(prep, 'year', method = "continuous", topics = 15,
      model = z, printlegend = FALSE, xaxt = "n", xlab = "Time ")

yearseq <- seq(from = as.Date('2010-01-01'), to = as.Date('2021-01-01'), by='year')
yearnames <- year(yearseq)

axis(1,at = as.numeric(yearseq) - min(as.numeric(yearseq)),labels = yearnames) 

# º¯¼ö º° ´Ü¾î 
plot(poliblogContent, type= "perspectives", topics=15)

# ÅäÇÈ º° ´Ü¾î 
plot(agendaPrevFit, type = "perspectives", topics = c(12, 15))

library(tidytext)
library(ggplot2)
library(dplyr)
x11() 
td_beta <- tidy(agendaPrevFit)
td_beta %>%
  group_by(topic) %>%
  top_n(10 , beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic", topic), term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta), title = "Highest word probabilities for each topic", 
       subtitle = "Different words are associated with different topics")


## ¹®¼­ º° ÅäÇÈ 
doc_topic=tidy(agendaPrevFit,matrix='gamma') %>% group_by(document)  %>% top_n(1,gamma) %>% ungroup() %>% arrange(document)
doc_topic
data_ALL$topic = doc_topic$topic

#topic correlations
mod.out.corr <- topicCorr(agendaPrevFit)
plot(mod.out.corr)



# criterion: Highest prob, FREX, LIFT, SCORE
df.agenda.prob <- data.frame(t(labelTopics(agendaPrevFit, n = 1)$prob))
df.agenda.frex <- data.frame(t(labelTopics(agendaPrevFit, n = 1)$frex))
df.agenda.lift <- data.frame(t(labelTopics(agendaPrevFit, n = 1)$lift))
df.agenda.score <- data.frame(t(labelTopics(agendaPrevFit, n = 1)$score))


### ¿öµåÅ¬¶ó¿ìµå

library(Rcpp)
library(wordcloud)
library(wordcloud2)


## 200´ë Å°¿öµå¸¸ ¼±Á¤ ÅäÇÈ º° ¿öµåÅ¬¶ó¿ìµå ±×¸®±â

get_topic_term= function(x){
  term=tidy(agendaPrevFit) %>% filter(topic==x)
  colnames(term)[2] = 'feature'
  colnames(term)[3] = 'frequency'
  term= term[,c(2,3)] %>% arrange(-frequency)
  term=term[1:200,]
  wordcloud2(term, fontFamily="Malgun Gothic", size = 0.5, minRotation=0, maxRotation=0)
}

get_topic_term(1)
get_topic_term(2)
get_topic_term(3)
get_topic_term(4)
get_topic_term(5)
get_topic_term(15)
get_topic_term(20)
# ¿öµåÅ¬¶ó¿ìµå ±×¸®±â
word_fre1 <- word_fre[1:200,] 
wordcloud2(word_fre1, fontFamily="Malgun Gothic", size = 0.5, minRotation=0, maxRotation=0)

# Áß¿ä ´Ü¾î tf_idf

tf_idf=dfm2.trim %>% dfm_tfidf(scheme_tf = "prop", scheme_df = "inverse")

word_fre_2=textstat_frequency(tf_idf,force=TRUE)
word_fre2 = word_fre_2[1:200]

wordcloud2(word_fre2, fontFamily="Malgun Gothic", size = 0.2, minRotation=0, maxRotation=0)
