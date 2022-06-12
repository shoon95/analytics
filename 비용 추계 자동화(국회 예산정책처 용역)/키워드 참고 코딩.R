install.packages('pacman')
library(pacman)

p_load('dplyr','openxlsx','stringr',data.table,tidyr)

dir.create('¹Ú»ç´Ô')

setwd('C:/Users/sjhty/Desktop/¿ë¿ª')
list.files()


openxlsx::getSheetNames('Å°¿öµå ¿øº».xlsx')

na<-getSheetNames('Å°¿öµå ¿øº».xlsx')
#i<-'±³Àç'
#j<-2
for(i in 1:12){
  data<-read.xlsx('Å°¿öµå ¿øº».xlsx',sheet=i)
  assign(na[[i]],data)
}

load('r.Rdata')
±³Àçºñ<-±³Àç
na[[1]]<-'±³Àçºñ'

lis<-mget(na)
name<-c('Á¦¸ñ','ÀüÃ¼ ³»¿ë','°ª Á¸Àç ¿©ºÎ','¿¬µµ','Å°¿öµå','ÇÙ½É ³»¿ë')

for(i in na){
  print(i)
  word<-paste0('(?=.*¿ø)(?=.',i,').*')
  value<-sapply(lis[[i]][,2], function(x){grepl(word,x,perl=TRUE)})
  lis[[i]][,3]<-value
  lis[[i]]<-filter(lis[[i]],V3==TRUE)
  
  year<-sapply(lis[[i]][,1], function(x)if(grepl('2020 ¿¹»ê|2020¿¹»ê',x)){2020}
            else if(grepl('2019 ¿¹»ê|2019¿¹»ê',x)){2019}
            else if(grepl('2018 ¿¹»ê|2018¿¹»ê',x)){2018}
            else if(grepl('2017 ¿¹»ê|2017¿¹»ê',x)){2017}
            else if(grepl('2016 ¿¹»ê|2016¿¹»ê',x)){2016})
  lis[[i]][,4]<-year
  lis[[i]][,5]<-i
  

  assign(i,lis[[i]])
}

 aaa<-data.frame()

lis<-mget(na)

for(i in na){     
   
  aaa<-data.frame()
  
  for(j in 1: nrow(lis[[i]])){
  t<-data.frame(str_split(lis[[i]][j,2],'\n'))
  t[,2]<-sapply(t, function(x) str_detect(x,i))
  text<-filter(t,V2==TRUE)
  a<-data.frame(text[,1])
  a<-t(a)
  names(a)<-1:nrow(text)
  aaa<-bind_rows(aaa,a)
  }
  
  lis[[i]][,6]<-unite(aaa,'ÇÙ½É ³»¿ë',colnames(aaa),sep='|')
  word<-paste0('(?=.*[0-9])(?=.¿ø).*')
  value<-sapply(lis[[i]][,6], function(x){grepl(word,x,perl=TRUE)})
  lis[[i]][,3]<-value
  lis[[i]]<-filter(lis[[i]],V3==TRUE)
  value<-sapply(lis[[i]][,6], function(x){str_replace_all(x,'\\|NA','')})
  lis[[i]][,6]<-value
  
  
  assign(i,lis[[i]])

}

all_data<-rbind(`R&D`,°­»ç·á,°ø±âÃ»Á¤±â,°øÃ»È¸,±³Àçºñ,¸¶½ºÅ©,¹è»ó±Ý,º¸»ó±Ý,¿¬¼ö,ÀÎÁõ,Åä·ÐÈ¸,Æ÷»ó±Ý)
word2<-'.*[0-9°¡-ÆR]+( ?[x¡¿] ?)[0-9°¡-ÆR]+.*'

a<-sapply(all_data[,6], function(x) str_extract(x,word2))
b<-data.frame(a)

all_data[,7]<-b

all_data_end<-na.omit(all_data)
table(all_data_end[,5])



key<-createWorkbook()
addWorksheet(key,'Å°¿öµå')
writeDataTable(key, 'Å°¿öµå',all_data_end)
saveWorkbook(key,'Å°¿öµå Á¤¸® ¼öÁ¤.xlsx',overwrite=TRUE)


