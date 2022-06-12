#################################################################################
# 패키지 설치 및 실행 

# install.packages("RSelenium")
# install.packages("rvest")
# install.packages("httr")
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("KoNLP")
# install.packages("RColorBrewer")
# install.packages("wordcloud")
# install.packages("tm")
# install.packages("igraph")
# install.packages("sqldf")
# install.packages("reshape")

library(KoNLP)
library(RSelenium)
library(rvest)
library(httr)
library(stringr)
library(dplyr)
library(RColorBrewer)
library(wordcloud)
library(tm) 
library(igraph) 
library(sqldf) 
library(reshape)

setwd("C:/Users/user/Desktop/단기과정/서울역차병원")

load("moms7adj.Rdata")

options(stringsAsFactors = FALSE)

################################################################
# 키워드 데이터 불러오기 

# install.packages("openxlsx")
library("openxlsx")

# xls 패키지 있는 경우에는 detach 
# detach("package:xlsx", unload = TRUE)

keyword <- read.xlsx(xlsxFile="키워드수정.xlsx", sheet=1, rows = c(1:271), cols=c(1),
                     colNames=TRUE)

################################################################
# TDM에서 keyword 존재하는 것만 남기기 

freq.words <- tdm[row.names(tdm) %in% keyword$x3, ]

############################################################################################

#  KeyWord에 대한 관계 분석 

tf <- apply(freq.words, 1, sum)
morp <- row.names(freq.words)
morp_tf <- data.frame(morp=morp, tf=tf)
row.names(morp_tf) <- NULL

ttfa <- morp_tf %>% summarise(ttfa=sum(tf)) 

morp_tf$p_a <- 
  as.numeric(sapply(morp_tf$tf, function(x) { round(x/ttfa*100, digits=2)} ))     

morp_tf <- morp_tf %>% arrange(desc(p_a)) %>% mutate(rank = row_number())
 
top_keyword <- morp_tf[morp_tf$rank<=10, ]

dt_all <- data.frame(morp=morp, freq.words) 
row.names(dt_all) <- NULL

dt_all1 <- melt(dt_all, id.vars="morp")

dt_all2 <- dt_all1[dt_all1$value>0, ]

dt_all2$variable <- as.numeric(gsub("X","",dt_all2$variable)) 

dt_all2 <- dt_all2 %>% dplyr::rename(src_keyid = variable, etf = value) %>%
  arrange(src_keyid, morp)

media_anal <- function(){  
  
  morp_tf5 <- top_keyword %>% select(morp, p_a)  # 상위 10개 키워드 
  colnames(morp_tf5)[1] <- "term"    
  dt_all3 <- dt_all2  # head(dt_all3, 20)  # 키워드별 문서별 개수 
  colnames(dt_all3)[1] <- "term"   
  morp_tf6 <- morp_tf   # head( morp_tf6 )  # 키워드별 개수 
  colnames(morp_tf6)[1] <- "term"   
  
  morp_tf7 <- c() 
  y <- 1
  for (i in morp_tf5$term ){ # i <- "이식"
    
    dt_all4 <- dt_all3[dt_all3$term ==i,] 
 
    dt_all4 <- dt_all3[dt_all3$src_keyid %in% unique(dt_all4$src_keyid),]   
    
    dt_all5 <- sqldf(" 
                   select  term, count(*) as count, sum(etf) as tf from dt_all4  
                   group by  term
                   order by  count desc
                   ", drv = "SQLite")   
    
    vf4 <- sqldf(" 
                 select  count(*) as tcount from 
                 ( select distinct src_keyid from dt_all4 ) 
                 ", drv = "SQLite")    
  
    dt_all5$count <- as.numeric(dt_all5$count)
    vf4$tcount <- as.numeric(vf4$tcount)  
    
    vf5 <- sqldf(" 
                 select a.*, b.tcount, a.count/b.tcount*100 as p_a_given_b from dt_all5 a, vf4 b
                 ", drv = "SQLite")  
    
    vf8 <- vf5[vf5$p_a_given_b>=5,] # 5% 이상으로 Cut   
    
    vf8$p_a_given_b <- as.numeric(vf8$p_a_given_b)
    morp_tf6$p_a <- as.numeric(morp_tf6$p_a)
    
    vf9_1x <- morp_tf5[morp_tf5$term==i,]  
    vf9_1y <- sqldf(" 
                    select a.*, b.p_a as p_a, 
                    a.p_a_given_b/b.p_a  as d_lift 
                    from vf8 a inner join morp_tf6 b
                    on a.term = b.term
                    order by p_a_given_b desc, d_lift desc
                    ", drv = "SQLite")  
    vf9_1 <- sqldf(" 
                   select b.term as base, a.*, b.p_a as p_b 
                   from vf9_1y a, vf9_1x b
                   ", drv = "SQLite")   
    vf10 <- sqldf(" 
                  select *, p_a_given_b*p_b/100 as p_a_and_b,
                  p_a + p_b - p_a_given_b*p_b/100 as p_a_or_b 
                  from vf9_1 
                  ", drv = "SQLite")  
    
    if(y==1) vf10 <- head(vf10[vf10$d_lift>=2,], 10)
    if(y> 1) vf10 <- head(vf10[vf10$d_lift>=2 & !(vf10$term %in% morp_tf5$term[1:(y-1)] ),], 10)
    
    # vf10 <- head(vf10[vf10$d_lift>=2,], 10)
    
    morp_tf7 <- rbind(morp_tf7,vf10)
    
    y <- y+1
    
  }
  
  morp_tf8 <- morp_tf7[morp_tf7[,1] != morp_tf7[,2],]
  sum_p_b = sum( unique(data.frame(morp_tf8$base, morp_tf8$p_b))[,2] )  #### <- 수정 
  morp_tf8 = data.frame(morp_tf8, sum_p_b)
  morp_tf8 = morp_tf8 %>% data.frame(p_b_i = morp_tf8$p_b / sum_p_b *100) # 주제확률 p_b_i 
  morp_tf8  = morp_tf8 %>% group_by(base)  %>% mutate(sum_p_ab= sum(p_a_given_b), p_ab_i = p_a_given_b / sum_p_ab *100) 
  
  # gephi와 NodeXL로 내보내기
  
  con <- file("result.csv")
  temp <- morp_tf8[,c(1:3)] 
  colnames(temp) <- c("Source", "Target", "Weight")
  write.csv(temp, file=con, row.names=FALSE )

  # clustering
  
  ex <- morp_tf8[,c(1,2,8)]
  colnames(ex)[1] <- 'from'  
  colnames(ex)[2] <- 'to' 
  g <- graph.data.frame(ex, directed=FALSE)
  
  # plot(g)
  
  # https://sojungluciakim.wordpress.com/2016/03/18/r-igraph를-활용한-community-detection/
  
  fgc <- fastgreedy.community(simplify(g)) # NodeXL과 같은방식
  # fgc <- walktrap.community(g)
  # fgc <- edge.betweenness.community(g)
  
  membership(fgc)
  sizes(fgc)
  
  par(mar=c(0,0,0,0))
  V(g)$size = 1*degree(g)
  
  # plot(g, vertex.color=membership(fgc))  ## vertex.color 옵션 사용
  
  # 주제별 클러스터 정보
  
  cluster <- as.data.frame(cbind(fgc$names, fgc$membership))
  cn <- c("names", "membership")
  colnames(cluster) <- cn 
  
  map_lh_cluster <- morp_tf8 %>% select(base,term,p_b_i, p_ab_i)  
  map_cluster <- merge(map_lh_cluster,cluster,by.x="base",by.y="names")
  map_cluster <- merge(map_cluster,cluster,by.x="term",by.y="names")
  map_lh_cluster <- map_cluster[( map_cluster$membership.y == map_cluster$membership.x),]
  map_lh_cluster <-map_lh_cluster[order(map_lh_cluster$ membership.x, decreasing=FALSE), ] 
  
  # 클러스터별 주요 문서
  # 1) 특정 클러스터만
  
  cluster_topic <- function(cluster, cluster_nm){ # cluster_nm = 1
    tp_cluster_df = cluster  %>% filter(names %in% morp_tf5$term)  %>% arrange(membership)
    
    #base
    topic_nm <- as.character(tp_cluster_df[which(tp_cluster_df$membership == cluster_nm),]$names)
    
    #term
    topic_term <- (morp_tf8 %>% filter(base %in% topic_nm))$term  %>% unique
    
    #pop by base
    pop_tn <- (dt_all3  %>% filter(grepl(paste(topic_nm,collapse="|"),term)))$src_keyid  %>% unique
    # head(pop_tn)
    pop_by_base <- dt_all3  %>% filter(src_keyid %in% pop_tn)  
    # head(pop_by_base)
    topic_tn  <- c()
    for(i in topic_term){  # i <- "이식"  
      term_cont  <- pop_by_base %>% filter(term == i)  %>% select(src_keyid)
      topic_tn <- rbind(topic_tn,term_cont)
    }  #head(topic_tn)
    topic_tn <- topic_tn %>% dplyr::group_by(src_keyid) %>% dplyr::summarise(count = n()) %>% dplyr::arrange(desc(count))
    topicnm_top5 <- (topic_tn %>% head(n=5))$src_keyid
    
    topicnm_print <-  as.character(cafe_content[topicnm_top5]) 
    # topicnm_print <-  as.character(cafe_gen[topicnm_top5]) 
    
    return(topicnm_print)  
  }
  
  # 2) 전체 클러스터 내용 확인
  
  cluster_alltopic = function(cluster){
    totaltopic <- c()
    tp_cluster_df = cluster  %>% filter(names %in% morp_tf5$term)  %>% arrange(membership)
    for(i in 1:max(as.numeric(tp_cluster_df$membership))){ # i <- 1
      topic_i <- cluster_topic(cluster,i)
      topic_i <- data.frame(cluster=i,keyword = paste(tp_cluster_df[tp_cluster_df$membership == i,]$names,collapse=", "), 
                            sub_key = paste(cluster[cluster$membership == i,]$names,collapse=", "), topic_i)
      totaltopic <- rbind(totaltopic,topic_i)
    }
    return(totaltopic)
  }
  
  topic_article <- cluster_alltopic(cluster)
  
  write.csv(topic_article,"topic_article.csv")
  
}

media_anal()


