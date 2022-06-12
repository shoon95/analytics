# https://junhewk.github.io/text/2017/08/08/cooccurence-matrix-with-Naver-blog/

# install.packages("rvest")
# install.packages("KoNLP")
# install.packages("stringr")
# install.packages("tm")
# install.packages("qgraph")
# install.packages("dplyr")
# install.packages("networkD3")
install.packages('pacman')
library(pacman)

p_load('rvest','KoNLP','stringr','tm','qgraph','dplyr','networkD3')

library(rvest)
library(KoNLP)
library(stringr)
library(tm)
library(qgraph)
library(dplyr)
library(networkD3)
library(openxlsx)
library(tidyr)
useNIADic()

data<-read.xlsx('D:/용역/5. 정보화사업/정보화사업리스트_전체.xlsx')
data<-data[,6]
texts<-data

cps <- VCorpus(VectorSource(texts))

tdm <- TermDocumentMatrix(cps,
                          control = list(weighting= weightBin, 
                                         tokenize=words,
                                         removePunctuation = T,
                                         removeNumbers = T,
                                         
                                         stopwords = c('차년도','구축차')))


tdm <- as.matrix(tdm)
tdm.matrix<-tdm

word.count <- rowSums(tdm.matrix)
word.order <- order(word.count, decreasing=TRUE)
freq.words <- tdm.matrix[word.order[1:30], ]

co.matrix <- freq.words %*% t(freq.words)

par(family="Apple SD Gothic Neo")
qgraph(co.matrix, labels=rownames(co.matrix),
       diag=FALSE, layout='spring', threshold=3,
       vsize=log(diag(co.matrix)) * 2)

node_df <- data_frame(node=rownames(co.matrix), value=as.numeric(diag(co.matrix))) %>%
  mutate(idx=row_number()-1)

# install.packages("reshape2")
library(reshape2)


link_df <- as.data.frame.table(co.matrix) %>%
  filter(Freq > 3) %>%
  rename(source=Terms, target=Terms.1, n=Freq) %>%
  left_join(node_df %>% rename(source_idx=idx) %>% select(-value), by=c('source'='node')) %>%
  left_join(node_df %>% rename(target_idx=idx) %>% select(-value), by=c('target'='node'))

forceNetwork(Links=as.data.frame(link_df), Nodes=as.data.frame(node_df),
             Source='source_idx', Target='target_idx',
             NodeID='node', Group='node')


forceNetwork(Links=as.data.frame(link_df), Nodes=as.data.frame(node_df),
             Source='source_idx', Target='target_idx',linkColour = 'gray', linkWidth = JS("function(d) { return Math.sqrt(d.value)*0.3; }"),
             NodeID='node', Group='node', Nodesize='value', Value='n', bounded=TRUE,charge=-60, 
             radiusCalculation=JS("Math.sqrt(d.nodesize)*1.5"),
             opacityNoHover=TRUE, linkDistance=150,
             zoom=TRUE, opacity=10000, fontSize=15,
             fontFamily="Apple SD Gothic Neo")


forceNetwork(Links=as.data.frame(link_df), Nodes=as.data.frame(node_df),
             Source='source_idx', Target='target_idx',zoom=TRUE,radiusCalculation=JS("Math.sqrt(d.nodesize)"),
             NodeID='node', Group='node',opacityNoHover=TRUE,opacity=10000)









