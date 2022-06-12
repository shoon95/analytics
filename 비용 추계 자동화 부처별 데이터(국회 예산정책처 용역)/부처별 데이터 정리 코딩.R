###test 평균 

setwd('C:/Users/sjhty/OneDrive - incubate B2C technologies/바탕 화면/연습/딱따구리')

library(pacman)
p_load('reshape2','stringr','openxlsx','dplyr','tidyr','readxl')
name<-list.files(pattern='.xlsx')
name<-sapply(name, function(x) str_split(x, "\\.")[[1]][1])
for(i in 1: length(name)){
  print(i)
  text<-paste0(name[[i]],'.xlsx')
  data<-read.xlsx(text,3,colNames=FALSE)
  assign(name[i],data)
}



lis<-mget(name)

#########################데이터 처리 - 평균1###########################################

for(i in name){ #i<-3
  text<-paste0('평균',i)
  print(text)
  
  if(length(lis[[i]])==15)
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X10')]
  a$X10[is.na(a$X10)==TRUE]<-0
  a<-filter(a, !X10==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  pat2<-'(?<=\\>).*'
  t<-sapply(a[,3], function(x) str_extract(x,pat2))
  t1<-sapply(t, function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('상당','',t1)
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('^14등급','차관',t1)
  t1<-gsub('^12등급|^13등급','고위공무원 가',t1)
  t1<-gsub('^10등급|^11등급','고위공무원 나',t1)
  t1<-gsub('^9등급','3급',t1)
  t1<-gsub('^6등급|7등급|8등급','4급',t1)
  t1<-gsub('^5등급','5급',t1)
  t1<-gsub('^4등급','6급',t1)
  t1<-gsub('^3등급','7급',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(pay=sum(X2),people=sum(X10))
  d<-d %>% mutate(total=pay/people)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  b<-arrange(dd,-봉급)
  
  pat<-"^[0-9].*."
  r<-b[,1][grep(pat,b[,1])]
  u<-b[grep(pat,b[,1]),]
  b[grep(pat,b[,1]),]<-u[order(r),]
  
  text<-paste0(i,'평균')
  assign(text,b)}
  else if(length(lis[[i]])==12)
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X10')]
  a$X10[is.na(a$X10)==TRUE]<-0
  a<-filter(a, !X10==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  t1<-sapply(a[,3], function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('상당','',t1)
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('^14등급','차관',t1)
  t1<-gsub('^12등급|^13등급','고위공무원 가',t1)
  t1<-gsub('^10등급|^11등급','고위공무원 나',t1)
  t1<-gsub('^9등급','3급',t1)
  t1<-gsub('^6등급|7등급|8등급','4급',t1)
  t1<-gsub('^5등급','5급',t1)
  t1<-gsub('^4등급','6급',t1)
  t1<-gsub('^3등급','7급',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(pay=sum(X2),people=sum(X10))
  d<-d %>% mutate(total=pay/people)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  b<-arrange(dd,-봉급)
  
  pat<-"^[0-9].*."
  r<-b[,1][grep(pat,b[,1])]
  u<-b[grep(pat,b[,1]),]
  b[grep(pat,b[,1]),]<-u[order(r),]
  
  text<-paste0(i,'평균')
  assign(text,b)}
  
  else
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X11')]
  a$X11[is.na(a$X11)==TRUE]<-0
  a<-filter(a, !X11==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  pat2<-'(?<=\\>).*'
  t<-sapply(a[,3], function(x) str_extract(x,pat2))
  t1<-sapply(t, function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('상당','',t1)
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('^14등급','차관',t1)
  t1<-gsub('^12등급|^13등급','고위공무원 가',t1)
  t1<-gsub('^10등급|^11등급','고위공무원 나',t1)
  t1<-gsub('^9등급','3급',t1)
  t1<-gsub('^6등급|7등급|8등급','4급',t1)
  t1<-gsub('^5등급','5급',t1)
  t1<-gsub('^4등급','6급',t1)
  t1<-gsub('^3등급','7급',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(pay=sum(X2),people=sum(X11))
  d<-d %>% mutate(total=pay/people)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  b<-arrange(dd,-봉급)
  
  pat<-"^[0-9].*."
  r<-b[,1][grep(pat,b[,1])]
  u<-b[grep(pat,b[,1]),]
  b[grep(pat,b[,1]),]<-u[order(r),]
  
  text<-paste0(i,'평균')
  assign(text,b)}
}
name1<-paste0(name,'평균')
lis_mean<-mget(name1)


####################### 데이터 처리 - 인원 합계1##########################

for(i in 1: length( name)){ #i<-1
  text<-paste0('인원합계_',name[i])
  print(text)
  
  if(length(lis[[i]])==15)
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X10')]
  a$X10[is.na(a$X10)==TRUE]<-0
  a<-filter(a, !X10==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  pat2<-'(?<=\\>).*'
  t<-sapply(a[,3], function(x) str_extract(x,pat2))
  t1<-sapply(t, function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('상당','',t1)
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('^14등급','차관',t1)
  t1<-gsub('^12등급|^13등급','고위공무원 가',t1)
  t1<-gsub('^10등급|^11등급','고위공무원 나',t1)
  t1<-gsub('^9등급','3급',t1)
  t1<-gsub('^6등급|7등급|8등급','4급',t1)
  t1<-gsub('^5등급','5급',t1)
  t1<-gsub('^4등급','6급',t1)
  t1<-gsub('^3등급','7급',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(people=sum(X10))
  d<-d %>% mutate(total=people)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  cha<-lis_mean[[i]][,1]
  a<-lapply(cha, function(x) x==dd[,1])
  num<-do.call(rbind,lapply(a,function(x) which(x==TRUE)))
  dd<-dd[num,]
  t<-c(1,apply(dd[,-1],2,sum))
  dd<-rbind(dd,t)
  y<-apply(dd[,-1],1,sum)
  dd<-cbind(dd,y)
  
  dd[nrow(dd),1]<-'총합계'
  colnames(dd)[[length(dd)]]<-'총합계'
  
  
  text<-paste0(name[i],'인원')
  assign(text,dd)}
  else if(length(lis[[i]])==12)
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X10')]
  a$X10[is.na(a$X10)==TRUE]<-0
  a<-filter(a, !X10==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  t1<-sapply(a[,3], function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('상당','',t1)
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('^14등급','차관',t1)
  t1<-gsub('^12등급|^13등급','고위공무원 가',t1)
  t1<-gsub('^10등급|^11등급','고위공무원 나',t1)
  t1<-gsub('^9등급','3급',t1)
  t1<-gsub('^6등급|7등급|8등급','4급',t1)
  t1<-gsub('^5등급','5급',t1)
  t1<-gsub('^4등급','6급',t1)
  t1<-gsub('^3등급','7급',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(people=sum(X10))
  d<-d %>% mutate(total=people)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  cha<-lis_mean[[i]][,1]
  a<-lapply(cha, function(x) x==dd[,1])
  num<-do.call(rbind,lapply(a,function(x) which(x==TRUE)))
  dd<-dd[num,]
  t<-c(1,apply(dd[,-1],2,sum))
  dd<-rbind(dd,t)
  y<-apply(dd[,-1],1,sum)
  dd<-cbind(dd,y)
  
  dd[nrow(dd),1]<-'총합계'
  colnames(dd)[[length(dd)]]<-'총합계'
  
  text<-paste0(name[i],'인원')
  assign(text,dd)}
  
  else
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X11')]
  a$X11[is.na(a$X11)==TRUE]<-0
  a<-filter(a, !X11==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  pat2<-'(?<=\\>).*'
  t<-sapply(a[,3], function(x) str_extract(x,pat2))
  t1<-sapply(t, function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('상당','',t1)
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('^14등급','차관',t1)
  t1<-gsub('^12등급|^13등급','고위공무원 가',t1)
  t1<-gsub('^10등급|^11등급','고위공무원 나',t1)
  t1<-gsub('^9등급','3급',t1)
  t1<-gsub('^6등급|7등급|8등급','4급',t1)
  t1<-gsub('^5등급','5급',t1)
  t1<-gsub('^4등급','6급',t1)
  t1<-gsub('^3등급','7급',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(people=sum(X11))
  d<-d %>% mutate(total=people)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  cha<-lis_mean[[i]][,1]
  a<-lapply(cha, function(x) x==dd[,1])
  num<-do.call(rbind,lapply(a,function(x) which(x==TRUE)))
  dd<-dd[num,]
  t<-c(1,apply(dd[,-1],2,sum))
  dd<-rbind(dd,t)
  y<-apply(dd[,-1],1,sum)
  dd<-cbind(dd,y)
  
  dd[nrow(dd),1]<-'총합계'
  colnames(dd)[[length(dd)]]<-'총합계'
  
  text<-paste0(name[i],'인원')
  assign(text,dd)}
}
name2<-paste0(name,'인원')
lis_people<-mget(name2)
######################## 데이터 처리 -총합1 ##########################################

for(i in 1: length(name)){ #i<-3
  text<-paste0('급여총합_',name[i])
  print(text)
  
  if(length(lis[[i]])==15)
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X10')]
  a$X10[is.na(a$X10)==TRUE]<-0
  a<-filter(a, !X10==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  pat2<-'(?<=\\>).*'
  t<-sapply(a[,3], function(x) str_extract(x,pat2))
  t1<-sapply(t, function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('상당','',t1)
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('^14등급','차관',t1)
  t1<-gsub('^12등급|^13등급','고위공무원 가',t1)
  t1<-gsub('^10등급|^11등급','고위공무원 나',t1)
  t1<-gsub('^9등급','3급',t1)
  t1<-gsub('^6등급|7등급|8등급','4급',t1)
  t1<-gsub('^5등급','5급',t1)
  t1<-gsub('^4등급','6급',t1)
  t1<-gsub('^3등급','7급',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(pay=sum(X2))
  d<-d %>% mutate(total=pay)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  cha<-lis_mean[[i]][,1]
  a<-lapply(cha, function(x) x==dd[,1])
  num<-do.call(rbind,lapply(a,function(x) which(x==TRUE)))
  dd<-dd[num,]
  t<-c(1,apply(dd[,-1],2,sum))
  dd<-rbind(dd,t)
  y<-apply(dd[,-1],1,sum)
  dd<-cbind(dd,y)
  
  dd[nrow(dd),1]<-'총합계'
  colnames(dd)[[length(dd)]]<-'총합계'
  
  text<-paste0(name[i],'총합')
  assign(text,dd)}
  else if(length(lis[[i]])==12)
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X10')]
  a$X10[is.na(a$X10)==TRUE]<-0
  a<-filter(a, !X10==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  t1<-sapply(a[,3], function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('상당','',t1)
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('^14등급','차관',t1)
  t1<-gsub('^12등급|^13등급','고위공무원 가',t1)
  t1<-gsub('^10등급|^11등급','고위공무원 나',t1)
  t1<-gsub('^9등급','3급',t1)
  t1<-gsub('^6등급|7등급|8등급','4급',t1)
  t1<-gsub('^5등급','5급',t1)
  t1<-gsub('^4등급','6급',t1)
  t1<-gsub('^3등급','7급',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(pay=sum(X2))
  d<-d %>% mutate(total=pay)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  cha<-lis_mean[[i]][,1]
  a<-lapply(cha, function(x) x==dd[,1])
  num<-do.call(rbind,lapply(a,function(x) which(x==TRUE)))
  dd<-dd[num,]
  t<-c(1,apply(dd[,-1],2,sum))
  dd<-rbind(dd,t)
  y<-apply(dd[,-1],1,sum)
  dd<-cbind(dd,y)
  
  dd[nrow(dd),1]<-'총합계'
  colnames(dd)[[length(dd)]]<-'총합계'
  
  text<-paste0(name[i],'총합')
  assign(text,dd)}
  
  else
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X11')]
  a$X11[is.na(a$X11)==TRUE]<-0
  a<-filter(a, !X11==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  pat2<-'(?<=\\>).*'
  t<-sapply(a[,3], function(x) str_extract(x,pat2))
  t1<-sapply(t, function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('상당','',t1)
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('^14등급','차관',t1)
  t1<-gsub('^12등급|^13등급','고위공무원 가',t1)
  t1<-gsub('^10등급|^11등급','고위공무원 나',t1)
  t1<-gsub('^9등급','3급',t1)
  t1<-gsub('^6등급|7등급|8등급','4급',t1)
  t1<-gsub('^5등급','5급',t1)
  t1<-gsub('^4등급','6급',t1)
  t1<-gsub('^3등급','7급',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(pay=sum(X2))
  d<-d %>% mutate(total=pay)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  cha<-lis_mean[[i]][,1]
  a<-lapply(cha, function(x) x==dd[,1])
  num<-do.call(rbind,lapply(a,function(x) which(x==TRUE)))
  dd<-dd[num,]
  t<-c(1,apply(dd[,-1],2,sum))
  dd<-rbind(dd,t)
  y<-apply(dd[,-1],1,sum)
  dd<-cbind(dd,y)
  
  dd[nrow(dd),1]<-'총합계'
  colnames(dd)[[length(dd)]]<-'총합계'
  
  text<-paste0(name[i],'총합')
  assign(text,dd)}
}
name3<-paste0(name,'총합')
lis_pay<-mget(name3)


############################### 데이터 처리 - 평균 총합 구하기
for(i in 1:length(name)){
  pay<-lis_pay[[i]]
  people<-lis_people[[i]]
  
  x<-pay[nrow(pay),-c(1,length(pay))]/people[nrow(people),-c(1,length(pay))]
  직급='총합계'
  t<-cbind(직급,x)
  lis_mean[[i]]<-rbind(lis_mean[[i]],t)
  
  총합계<-pay[,length(pay)]/people[,2]
  lis_mean[[i]]<-cbind(lis_mean[[i]],총합계)
  lis_mean[[i]][lis_mean[[i]]==Inf]<-0
  assign(name1[[i]],lis_mean[[i]])
  
  
}
for(i in 1: length(name)){
  a<-rbind(lis_pay[[i]],colnames(lis_pay[[i]]),lis_people[[i]],colnames(lis_pay[[i]]),lis_mean[[i]])
  text<-paste0(name[i],'통합')
  assign(text,a)
}
text<-paste0(name,'통합')
lis_all<-mget(text)

### ############################데이터 처리 -완성본 코딩
for(i in 1:length(name)){ #i<-32
  print(name[i])
  
  data<-lis[[i]]
  
  if(length(data)==12){data1<-data[,c(1,3,2)]
  
  
  ### 단가 추가
  first<-data1[,2]
  first<- sapply(first, function(x) str_split(x, '\\*')[[1]][1])
  first<-gsub('원|\\=','',first)
  data2<-cbind(data1,first)
  
  ### 1.에서 . 분리 후 앞 부분 
  second<-data[,4]
  second<- sapply(second, function(x) str_split(x, '\\.')[[1]][1])
  data3<-cbind(data2,as.numeric(second))
  
  ### 수당1
  third<-data[,4]
  third<- sapply(third, function(x) str_split(x, '\\.')[[1]][2])         
  third<- sapply(third, function(x) str_split(x, '\\(')[[1]][1])         
  data4<-cbind(data3,third)
  
  ###  수당_괄호
  fourth<-data[,4]
  fourth<- sapply(fourth, function(x) str_split(x, '\\.')[[1]][2])         
  fourth<- sapply(fourth, function(x) str_split(x, '\\(')[[1]][2])
  fourth<-gsub('\\)','',fourth)
  data5<-cbind(data4,fourth)
  
  ### 가.
  fifth<-NA
  data6<-cbind(data5,fifth)
  
  ### 수당2
  sixth<-data[,5]
  data7<-cbind(data6,sixth)
  
  ### 수당2_괄호
  seventh<-data[,5]
  seventh<- sapply(seventh, function(x) str_split(x, '\\(')[[1]][2]) 
  seventh<- gsub('\\)','',seventh)
  data8<-cbind(data7,seventh)
  
  ### 1)
  eightth<-NA
  data9<-cbind(data8,as.numeric(eightth))
  
  ### 직-유형
  nineth<-data[,6]
  data10<-cbind(data9,nineth)
  
  ### 가)
  tenth<-NA
  data11<-cbind(data10,tenth)
  
  ### 연봉제
  eleventh<-data[,7]
  data12<-cbind(data11,eleventh)
  
  ### 1>
  twelfth<-NA
  data13<-cbind(data12,as.numeric(twelfth))
  
  ### 직급
  thirteenth<- data[,8]
  thirteenth<- sapply(thirteenth, function(x) str_split(x, '\\(')[[1]][1]) 
  data14<-cbind(data13,thirteenth)
  
  ### 명
  
  data15<-cbind(data14,data[,10])
  colnames(data15)<-c('연번','산식','금액','단가','1.','수당','수당_괄호','가.','수당2','수당2_괄호','1)','직-유형','가)','연봉제','1>','직급','명')
  text<-paste0(name[i],'완성본')
  assign(text,data15)
  }
  else{
    
    ### 금액까지 가져오기
    data1<-data[,c(1,3,2)]
    
    
    ### 단가 추가
    first<-data1[,2]
    first<- sapply(first, function(x) str_split(x, '\\*')[[1]][1])
    first<-gsub('원|\\=','',first)
    data2<-cbind(data1,first)
    
    ### 1.에서 . 분리 후 앞 부분 
    second<-data[,4]
    second<- sapply(second, function(x) str_split(x, '\\.')[[1]][1])
    data3<-cbind(data2,as.numeric(second))
    
    ### 수당1
    third<-data[,4]
    third<- sapply(third, function(x) str_split(x, '\\.')[[1]][2])         
    third<- sapply(third, function(x) str_split(x, '\\(')[[1]][1])         
    data4<-cbind(data3,third)
    
    ###  수당_괄호
    fourth<-data[,4]
    fourth<- sapply(fourth, function(x) str_split(x, '\\.')[[1]][2])         
    fourth<- sapply(fourth, function(x) str_split(x, '\\(')[[1]][2])
    fourth<-gsub('\\)','',fourth)
    data5<-cbind(data4,fourth)
    
    ### 가.
    fifth<-data[,5]
    fifth<- sapply(fifth, function(x) str_split(x, '\\.')[[1]][1])
    data6<-cbind(data5,fifth)
    
    ### 수당2
    sixth<-data[,5]
    sixth<- sapply(sixth, function(x) str_split(x, '\\.')[[1]][2])
    sixth<- sapply(sixth, function(x) str_split(x, '\\(')[[1]][1]) 
    data7<-cbind(data6,sixth)
    
    ### 수당2_괄호
    seventh<-data[,5]
    seventh<- sapply(seventh, function(x) str_split(x, '\\.')[[1]][2])
    seventh<- sapply(seventh, function(x) str_split(x, '\\(')[[1]][2]) 
    seventh<- gsub('\\)','',seventh)
    data8<-cbind(data7,seventh)
    
    ### 1)
    eightth<-data[,6]
    eightth<- sapply(eightth, function(x) str_split(x, '\\)')[[1]][1]) 
    data9<-cbind(data8,as.numeric(eightth))
    
    ### 직-유형
    nineth<-data[,6]
    nineth<- sapply(nineth, function(x) str_split(x, '\\)')[[1]][2]) 
    data10<-cbind(data9,nineth)
    
    ### 가)
    tenth<-data[,7]
    tenth<- sapply(tenth, function(x) str_split(x, '\\)')[[1]][1]) 
    data11<-cbind(data10,tenth)
    
    ### 연봉제
    eleventh<-data[,7]
    eleventh<- sapply(eleventh, function(x) str_split(x, '\\)')[[1]][2]) 
    data12<-cbind(data11,eleventh)
    
    ### 1>
    twelfth<- data[,8]
    twelfth<- sapply(twelfth, function(x) str_split(x, '\\>')[[1]][1]) 
    data13<-cbind(data12,as.numeric(twelfth))
    
    ### 직급
    thirteenth<- data[,8]
    thirteenth<- sapply(thirteenth, function(x) str_split(x, '\\>')[[1]][2]) 
    thirteenth<- sapply(thirteenth, function(x) str_split(x, '\\(')[[1]][1]) 
    data14<-cbind(data13,thirteenth)
    
    ### 명
    
    if(length(data)==16){
      data15<-cbind(data14,data[,11])
    }
    else{data15<-cbind(data14,data[,10])} 
    
    
    colnames(data15)<-c('연번','산식','금액','단가','1.','수당','수당_괄호','가.','수당2','수당2_괄호','1)','직-유형','가)','연봉제','1>','직급','명')
    text<-paste0(name[i],'완성본')
    assign(text,data15)}
  
  
  
}

text<-paste0(name,'완성본')

lis_end<-mget(text)
##########################################################통계표 2번#########################


#########################데이터 처리2 - 평균###########################################

for(i in name){ #i<-3
  text<-paste0('평균',i)
  print(text)
  
  if(length(lis[[i]])==15)
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X10')]
  a$X10[is.na(a$X10)==TRUE]<-0
  a<-filter(a, !X10==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  pat2<-'(?<=\\>).*'
  t<-sapply(a[,3], function(x) str_extract(x,pat2))
  t1<-sapply(t, function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(pay=sum(X2),people=sum(X10))
  d<-d %>% mutate(total=pay/people)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  b<-arrange(dd,-봉급)
  
  
  text<-paste0(i,'평균2')
  assign(text,b)}
  else if(length(lis[[i]])==12)
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X10')]
  a$X10[is.na(a$X10)==TRUE]<-0
  a<-filter(a, !X10==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  t1<-sapply(a[,3], function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(pay=sum(X2),people=sum(X10))
  d<-d %>% mutate(total=pay/people)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  b<-arrange(dd,-봉급)
  
  
  text<-paste0(i,'평균2')
  assign(text,b)}
  
  else
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X11')]
  a$X11[is.na(a$X11)==TRUE]<-0
  a<-filter(a, !X11==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  pat2<-'(?<=\\>).*'
  t<-sapply(a[,3], function(x) str_extract(x,pat2))
  t1<-sapply(t, function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(pay=sum(X2),people=sum(X11))
  d<-d %>% mutate(total=pay/people)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  b<-arrange(dd,-봉급)

  
  text<-paste0(i,'평균2')
  assign(text,b)}
}
name11<-paste0(name,'평균2')
lis_mean2<-mget(name11)


####################### 데이터 처리2 - 인원2 합계##########################

for(i in 1: length( name)){ #i<-1
  text<-paste0('인원2합계_',name[i])
  print(text)
  
  if(length(lis[[i]])==15)
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X10')]
  a$X10[is.na(a$X10)==TRUE]<-0
  a<-filter(a, !X10==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  pat2<-'(?<=\\>).*'
  t<-sapply(a[,3], function(x) str_extract(x,pat2))
  t1<-sapply(t, function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(people=sum(X10))
  d<-d %>% mutate(total=people)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  cha<-lis_mean2[[i]][,1]
  a<-lapply(cha, function(x) x==dd[,1])
  num<-do.call(rbind,lapply(a,function(x) which(x==TRUE)))
  dd<-dd[num,]
  t<-c(1,apply(dd[,-1],2,sum))
  dd<-rbind(dd,t)
  y<-apply(dd[,-1],1,sum)
  dd<-cbind(dd,y)
  
  dd[nrow(dd),1]<-'총합계'
  colnames(dd)[[length(dd)]]<-'총합계'
  
  
  text<-paste0(name[i],'인원2')
  assign(text,dd)}
  else if(length(lis[[i]])==12)
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X10')]
  a$X10[is.na(a$X10)==TRUE]<-0
  a<-filter(a, !X10==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  t1<-sapply(a[,3], function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(people=sum(X10))
  d<-d %>% mutate(total=people)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  cha<-lis_mean2[[i]][,1]
  a<-lapply(cha, function(x) x==dd[,1])
  num<-do.call(rbind,lapply(a,function(x) which(x==TRUE)))
  dd<-dd[num,]
  t<-c(1,apply(dd[,-1],2,sum))
  dd<-rbind(dd,t)
  y<-apply(dd[,-1],1,sum)
  dd<-cbind(dd,y)
  
  dd[nrow(dd),1]<-'총합계'
  colnames(dd)[[length(dd)]]<-'총합계'
  
  text<-paste0(name[i],'인원2')
  assign(text,dd)}
  
  else
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X11')]
  a$X11[is.na(a$X11)==TRUE]<-0
  a<-filter(a, !X11==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  pat2<-'(?<=\\>).*'
  t<-sapply(a[,3], function(x) str_extract(x,pat2))
  t1<-sapply(t, function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(people=sum(X11))
  d<-d %>% mutate(total=people)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  cha<-lis_mean2[[i]][,1]
  a<-lapply(cha, function(x) x==dd[,1])
  num<-do.call(rbind,lapply(a,function(x) which(x==TRUE)))
  dd<-dd[num,]
  t<-c(1,apply(dd[,-1],2,sum))
  dd<-rbind(dd,t)
  y<-apply(dd[,-1],1,sum)
  dd<-cbind(dd,y)
  
  dd[nrow(dd),1]<-'총합계'
  colnames(dd)[[length(dd)]]<-'총합계'
  
  text<-paste0(name[i],'인원2')
  assign(text,dd)}
}
name22<-paste0(name,'인원2')
lis_people2<-mget(name22)
######################## 데이터 처리2 -총합2 ##########################################

for(i in 1: length(name)){ #i<-3
  text<-paste0('급여총합2_',name[i])
  print(text)
  
  if(length(lis[[i]])==15)
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X10')]
  a$X10[is.na(a$X10)==TRUE]<-0
  a<-filter(a, !X10==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  pat2<-'(?<=\\>).*'
  t<-sapply(a[,3], function(x) str_extract(x,pat2))
  t1<-sapply(t, function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(pay=sum(X2))
  d<-d %>% mutate(total=pay)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  cha<-lis_mean2[[i]][,1]
  a<-lapply(cha, function(x) x==dd[,1])
  num<-do.call(rbind,lapply(a,function(x) which(x==TRUE)))
  dd<-dd[num,]
  t<-c(1,apply(dd[,-1],2,sum))
  dd<-rbind(dd,t)
  y<-apply(dd[,-1],1,sum)
  dd<-cbind(dd,y)
  
  dd[nrow(dd),1]<-'총합계'
  colnames(dd)[[length(dd)]]<-'총합계'
  
  text<-paste0(name[i],'총합2')
  assign(text,dd)}
  else if(length(lis[[i]])==12)
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X10')]
  a$X10[is.na(a$X10)==TRUE]<-0
  a<-filter(a, !X10==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  t1<-sapply(a[,3], function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(pay=sum(X2))
  d<-d %>% mutate(total=pay)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  cha<-lis_mean2[[i]][,1]
  a<-lapply(cha, function(x) x==dd[,1])
  num<-do.call(rbind,lapply(a,function(x) which(x==TRUE)))
  dd<-dd[num,]
  t<-c(1,apply(dd[,-1],2,sum))
  dd<-rbind(dd,t)
  y<-apply(dd[,-1],1,sum)
  dd<-cbind(dd,y)
  
  dd[nrow(dd),1]<-'총합계'
  colnames(dd)[[length(dd)]]<-'총합계'
  
  text<-paste0(name[i],'총합2')
  assign(text,dd)}
  
  else
  {data<-lis[[i]]
  a<-data[-1,c('X2','X4','X8','X11')]
  a$X11[is.na(a$X11)==TRUE]<-0
  a<-filter(a, !X11==0)
  
  ### 급여 이름 정리 
  pat<-'(?<=\\.).*?(?=\\()'
  t<-sapply(a[,2], function(x) str_extract(x,pat))
  a[,2]<-str_trim(t)
  a[is.na(a)==TRUE]<-0
  
  ### 직급 이름 정리
  pat2<-'(?<=\\>).*'
  t<-sapply(a[,3], function(x) str_extract(x,pat2))
  t1<-sapply(t, function(x) str_split(x,'\\(')[[1]][1])
  t1<-gsub('^차관.*.','차관',t1)
  t1<-gsub('^장관.*.','장관',t1)
  t1<-gsub('^채용후보자.*.','채용후보자',t1)
  t1<-gsub('^ 채용후보자.*.','채용후보자',t1)
  t1<-gsub('8 급','8급',t1)
  t1<-gsub('연봉제5급','5급',t1)
  t1<-gsub('전문경력관  나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관가군','전문경력관 가군',t1)
  t1<-gsub('전문경력관나군','전문경력관 나군',t1)
  t1<-gsub('전문경력관다군','전문경력관 다군',t1)
  t1<-gsub('전문임기제 나간','전문경력관 나군',t1)
  a[,3]<-str_trim(t1)
  
  a[is.na(a)==TRUE]<-0
  d<-a %>% group_by(X4,X8)%>% summarize(pay=sum(X2))
  d<-d %>% mutate(total=pay)
  dd<-dcast(d,X8~X4,sum)
  
  a<-which(colnames(dd)=='봉급')
  dd<-cbind(dd[,c(1,a)],dd[,-c(1,a)])
  colnames(dd)[1]<-'직급'
  
  dd[dd=='Inf']<-0
  
  cha<-lis_mean2[[i]][,1]
  a<-lapply(cha, function(x) x==dd[,1])
  num<-do.call(rbind,lapply(a,function(x) which(x==TRUE)))
  dd<-dd[num,]
  t<-c(1,apply(dd[,-1],2,sum))
  dd<-rbind(dd,t)
  y<-apply(dd[,-1],1,sum)
  dd<-cbind(dd,y)
  
  dd[nrow(dd),1]<-'총합계'
  colnames(dd)[[length(dd)]]<-'총합계'
  
  text<-paste0(name[i],'총합2')
  assign(text,dd)}
}
name33<-paste0(name,'총합2')
lis_pay2<-mget(name33)


############################### 데이터 처리2 - 평균 총합2 구하기
for(i in 1:length(name)){
  pay<-lis_pay2[[i]]
  people<-lis_people2[[i]]
  
  x<-pay[nrow(pay),-c(1,length(pay))]/people[nrow(people),-c(1,length(pay))]
  직급='총합계'
  t<-cbind(직급,x)
  lis_mean2[[i]]<-rbind(lis_mean2[[i]],t)
  
  총합계<-pay[,length(pay)]/people[,2]
  lis_mean2[[i]]<-cbind(lis_mean2[[i]],총합계)
  lis_mean2[[i]][lis_mean2[[i]]==Inf]<-0
  assign(name11[[i]],lis_mean2[[i]])
  
  
}
for(i in 1: length(name)){
  a<-rbind(lis_pay2[[i]],colnames(lis_pay2[[i]]),lis_people2[[i]],colnames(lis_pay2[[i]]),lis_mean2[[i]])
  text<-paste0(name[i],'통합2')
  assign(text,a)
}
text<-paste0(name,'통합2')
lis_all2<-mget(text)

################################################### 저장

tt<-list.files()

for(i in 1:length(name)){
  setwd('C:/Users/sjhty/OneDrive - incubate B2C technologies/바탕 화면/연습/딱따구리')

  print(name[i])
  
  data1<-read_xlsx(tt[[i]],1)
  data2<-read_xlsx(tt[[i]],2)
  data3<-read_xlsx(tt[[i]],3)
  
  setwd('C:/Users/sjhty/OneDrive - incubate B2C technologies/바탕 화면/몰라')
  
  a<-createWorkbook()
  addWorksheet(a,'원본')
  addWorksheet(a,'추출본')
  addWorksheet(a,'정리본')
  addWorksheet(a,'완결본')
  addWorksheet(a,'통계표1')
  addWorksheet(a,'통계표2')
  writeData(a,'원본',data1,rowName=FALSE,withFilter=FALSE, )
  writeDataTable(a,'추출본',data2)
  writeDataTable(a,'정리본',data3)
  writeDataTable(a,'완결본',lis_end[[i]])
  writeData(a,'통계표1',lis_all[[i]],rowNames=FALSE, borders="all", borderColour = "black",borderStyle='thin')
  writeData(a,'통계표2',lis_all2[[i]],rowNames=FALSE, borders="all", borderColour = "black",borderStyle='thin')
  
  text<-paste0(name[[i]],'.xlsx')
  openxlsx::saveWorkbook(a,text,overwrite=TRUE)
  
}

t<-data.frame()
for(i in 1:length(name)){
  a<-lis_all[[i]]
  부처명<-name[i]
  b<-cbind(부처명,a)
  t<-bind_rows(t,b)

}

write.xlsx(t,'모음1.xlsx')

t<-data.frame()
for(i in 1:length(name)){
  a<-lis_all2[[i]]
  부처명<-name[i]
  b<-cbind(부처명,a)
  t<-bind_rows(t,b)
  
}
write.xlsx(t,'모음2.xlsx')
getwd()
