install.packages('pacman')
library(pacman)

p_load('RSelenium','dplyr','rvest','openxlsx','stringr','data.table','tidyr')

setwd('D:/국회 자문')

portn<-as.integer(runif(1,1,5000))
rD<-rsDriver(port=portn, browser='chrome',chromever='89.0.4389.23')
remDr<-rD[['client']]

# 검색어 목록 가져오기
exlist = read.xlsx('비용추계자동화_세법명.xlsx',colNames=TRUE, rowNames=TRUE)

exlist[,1]
# 최대 검색건
getdata<-remDr$getPageSource()[[1]]
text_max<-read_html(getdata) %>% html_node(xpath='/html/body/div/div[2]/div[2]/div/p/span/text()') %>% html_text()


url_all_2 = data.frame()
table_all = data.frame()
title_all = data.frame()
cat_all= data.frame()  
for(search in exlist[,1]){
  page_now = 0
  
  url<-'http://likms.assembly.go.kr/bill/BillSearchDetail.do'
  remDr$navigate(url)
  # 기간 선택
  period_show = remDr$findElement('xpath','//*[@id="AGE_AREA"]/select[1]')
  period_show$clickElement()
  
  Sys.sleep(0.3)
  
  period_17 = remDr$findElement('xpath','//*[@id="AGE_AREA"]/select[1]/option[1]')
  period_17$clickElement()
  
  # 제안종류선택: 의원
  do_select<-remDr$findElement(using='xpath', value='//*[@id="srchForm"]/div/div/div[14]/select')
  do_select$clickElement()
  
  Sys.sleep(0.3)
  
  values<-paste0('//*[@id="srchForm"]/div/div/div[14]/select/option[2]')
  do<-remDr$findElement(using='xpath', value=values)
  do$clickElement()
  
  Sys.sleep(0.3)
  # 비용추계서 유무: 있음
  do_select<-remDr$findElement(using='xpath', value='//*[@id="srchForm"]/div/div/div[18]/select')
  do_select$clickElement()
  
  Sys.sleep(0.3)
  
  values<-paste0('//*[@id="srchForm"]/div/div/div[18]/select/option[2]')
  do<-remDr$findElement(using='xpath', value=values)
  do$clickElement()
  
  Sys.sleep(0.3)
  
  start<-remDr$findElement(using='xpath', value='//*[@id="srchForm"]/div/div/div[2]/input')
  start$clearElement() #검색창에 디폴트 값이 있는 경우를 위한 예비코드 : 검색창을 지워라
  
  start$sendKeysToElement(list(search))
  
  Sys.sleep(0.3)
  
  # 검색버튼 누르기
  values<-paste0('/html/body/div/div[2]/div[2]/div/div[2]/button[1]')
  do<-remDr$findElement(using='xpath', value=values)
  do$clickElement()
  
  Sys.sleep(2)
  
  # 페이지당 결과수 100으로 
  do_select<-remDr$findElement(using='xpath', value='//*[@id="pageSizeOption"]')
  do_select$clickElement()
  
  Sys.sleep(0.3)
  
  values<-paste0('//*[@id="pageSizeOption"]/option[4]')
  do<-remDr$findElement(using='xpath', value=values)
  do$clickElement()
  
  Sys.sleep(1)
  
  getdata<-remDr$getPageSource()[[1]]
  text_max<-read_html(getdata) %>% html_node(xpath='/html/body/div/div[2]/div[2]/div/p/span/text()') %>% html_text()
  
  aa1<-strsplit(text_max,"총")
  aa2<-aa1[[1]]
  aa3<-aa2[2]
  aa4<-strsplit(aa3,"건")
  aa5<-aa4[[1]]
  aa5= gsub(',','',aa5[1])
  n_max<-as.numeric(aa5)
  page = ceiling(n_max/100)
  
  if(n_max==0){
    next
  }
  repeat {
    
    page_now = page_now+1

    
    script = sprintf('javascript:GoPage(%d)',page_now)
    pagemove <- remDr$executeScript(script, args = 1:2) 
    ####테이블 가져오기
    
    data = remDr$getPageSource()[[1]]
    
    Sys.setlocale('LC_ALL','english')
    
    table_data = read_html(data) %>% html_nodes(xpath='/html/body/div/div[2]/div[2]/div/div[2]/table') %>% html_table()
    table_data = table_data[[1]]
    
    Sys.setlocale('LC_ALL','korean')
    
    table_all = rbind(table_all,table_data)

    
    ####url 가져오기
    url=remDr$findElements('xpath', '/html/body/div/div[2]/div[2]/div/div[2]/table/tbody/tr[*]/td[2]/div[2]/a')
    url_all = data.frame()
    for( i in url){
      url_all = rbind(url_all,i$getElementAttribute('href')[[1]])
    }
    
    url2=data.frame(sapply(url_all[,1], function(x){str_split(x, '\'')[[1]][2]}))
    url_all_2=rbind(url_all_2,url2)
    
    
    
    #### title 가져오기
    title = remDr$findElements('xpath','/html/body/div/div[2]/div[2]/div/div[2]/table/tbody/tr[*]/td[2]/div[2]/a')
    
    for( i in title){
      title_all = rbind(title_all, i$getElementText()[[1]][1])
    }
    
    if(page==page_now){
      break
    }
    
  }

}

url_all = data.frame()

for( i in url_all_2[,1]){
  t = paste0('https://likms.assembly.go.kr/bill/billDetail.do?billId=',i)
  
  url_all = rbind(url_all, t)
}


writer=gsub("\\)",'',str_split(title_all[1,1],'\\(')[[1]][2])

writer_all = data.frame()
for(i in title_all[,1]){
  writer=gsub("\\)",'',str_split(i,'\\(')[[1]][2])
  writer_all = rbind(writer_all,writer)
  
}


# 데이터 가져오는 코딩
# 가져올 데이터 이름이 먼저 (read_html)
# 노드를 지정
# 가져올 값의 성격에 따라 html_text html_attr 


hwp_url_all = data.frame()
table1_all = data.frame()
table2_all = data.frame()
table3_all = data.frame()
table4_all = data.frame()
table1_id_all =data.frame()
table2_id_all =data.frame()
table3_id_all =data.frame()
table4_id_all =data.frame()
content_id_all = data.frame()
hwp_url_all_id_all = data.frame()
number_list = data.frame(table_all[,1])
contents_all = data.frame()
page_num = 0

for(go in url_all[,1]){
  
  page_num= page_num+1
  
  now=paste0(page_num,'/',length(url_all[,1]))
  
  print(now)
  
  data = read_html(go)
  text_check=data %>% html_nodes('.tableCol01') 
  
  if(sum(grepl('의안접수정보',text_check))!=0){
    
    Sys.setlocale("LC_ALL", 'english')
    
    table1=data %>% html_nodes(xpath = '/html/body/div/div[2]/div[2]/div/div[3]/div[1]/table') %>% html_table()
    table1 = table1[[1]]
    Sys.setlocale("LC_ALL","korean")
    
    table1_all=rbind(table1_all,table1)
    
    table1_id=data.frame('id'=rep(number_list[page_num,1],nrow(table1)))
    table1_id_all = rbind(table1_id_all, table1_id)
    
    where = data %>% html_nodes(xpath='/html/body/div/div[2]/div[2]/div/div[3]/div[1]/table/tbody/tr/td[4]') %>% html_text()
    if(sum(grepl('비용추계',table1)) !=0){
      if(grepl('의안',str_split(where,'문')[[1]][1])){
        num=grep('hwp',data %>% html_nodes(xpat='/html/body/div/div[2]/div[2]/div/div[3]/div[1]/table/tbody/tr/td[4]/a[*]/img'))[2]
        value = sprintf('/html/body/div/div[2]/div[2]/div/div[3]/div[1]/table/tbody/tr/td[4]/a[%d]', num)
        hwp = data %>% html_nodes(xpath = value) %>% html_attr('href')
        
        base1=str_split(hwp,'\\(')[[1]][2]
        base2= gsub("\\'|;|\\)",'', base1)
        text1= str_split(base2, ',')[[1]][1]
        text2=str_split(base2, ',')[[1]][2]
        text3=str_split(base2, ',')[[1]][3]
        
        hwp_url = paste0(text1,'?bookId=',text2,'&type=',text3)
        hwp_url_all = rbind(hwp_url_all,hwp_url)}

      else{
        num=grep('hwp',data %>% html_nodes(xpat='/html/body/div/div[2]/div[2]/div/div[3]/div[1]/table/tbody/tr/td[4]/a[*]/img'))[1]
        value = sprintf('/html/body/div/div[2]/div[2]/div/div[3]/div[1]/table/tbody/tr/td[4]/a[%d]', num)
        hwp = data %>% html_nodes(xpath = value) %>% html_attr('href')
        
        base1=str_split(hwp,'\\(')[[1]][2]
        base2= gsub("\\'|;|\\)",'', base1)
        text1= str_split(base2, ',')[[1]][1]
        text2=str_split(base2, ',')[[1]][2]
        text3=str_split(base2, ',')[[1]][3]
        
        hwp_url = paste0(text1,'?bookId=',text2,'&type=',text3)
        hwp_url_all = rbind(hwp_url_all,hwp_url)
      }
    }else{
      hwp_urp = NA
      hwp_url_all = rbind(hwp_url_all,hwp_url)
    }
  }
  
  if(sum(grepl('소관위 심사정보',text_check))!=0){
    Sys.setlocale('LC_ALL','english')
    
    table2= data %>% html_nodes(xpath='/html/body/div/div[2]/div[2]/div/div[5]/div[1]') %>% html_table()
    table2 = table2[[1]]
    
    Sys.setlocale('LC_ALL',"korean")
    
    table2_all = bind_rows(table2_all, table2)
    
    table2_id=data.frame('id'=rep(number_list[page_num,1],nrow(table2)))
    table2_id_all = rbind(table2_id_all, table2_id)
  }else{
    table2_all = rbind(table2_all, NA)
    
    table2_id=data.frame('id'=number_list[page_num,1])
    table2_id_all = rbind(table2_id_all, table2_id)
    }
  
  if(sum(grepl('소관위 회의정보',text_check))!=0){
    Sys.setlocale('LC_ALL','english')
    
    table3= data %>% html_nodes(xpath='/html/body/div/div[2]/div[2]/div/div[5]/div[2]') %>% html_table()
    table3 = table3[[1]]
    
    Sys.setlocale('LC_ALL',"korean")
    
    table3_all = bind_rows(table3_all, table3)
    
    table3_id=data.frame('id'=rep(number_list[page_num,1],nrow(table3)))
    table3_id_all = rbind(table3_id_all, table3_id)

  }else{
    table3_all = rbind(table3_all,NA)
    
    table3_id=data.frame('id'=number_list[page_num,1])
    table3_id_all = rbind(table3_id_all, table3_id)
  }
  
  if(sum(grepl('본회의 심의정보',text_check))!=0){
    Sys.setlocale('LC_ALL','english')
    
    table4= data %>% html_nodes(xpath='/html/body/div/div[2]/div[2]/div/div[7]/div') %>% html_table()
    table4 = table4[[1]]
    
    Sys.setlocale('LC_ALL',"korean")
    
    table4_all = bind_rows(table4_all, table4)
    
    table4_id=data.frame('id'=rep(number_list[page_num,1],nrow(table4)))
    table4_id_all = rbind(table4_id_all, table4_id)
  }else{
    table4_all = rbind(table4_all,NA)
    
    table4_id=data.frame('id'=number_list[page_num,1])
    table4_id_all = rbind(table4_id_all, table4_id)
  }
  
  ##### 주요 내용 가져오기
  contents=data %>% html_nodes(xpath = '//*[@id="summaryContentDiv"]') %>% html_text()
  if(length(contents)==0){
    contents=NA
    contents_all =rbind(contents_all, contents)
    
    content_id=data.frame('id'=number_list[page_num,1])
    content_id_all = rbind(content_id_all, content_id)
  }else{
    contents_all =rbind(contents_all, contents)
    
    content_id=data.frame('id'=number_list[page_num,1])
    content_id_all = rbind(content_id_all, content_id)
  }

}

data0=data.frame(table_all)
data1=data.frame(cbind(table1_id_all,table1_all))
data2=data.frame(cbind(table2_id_all,table2_all))
data3=data.frame(cbind(table3_id_all,table3_all))
data4=data.frame(cbind(table4_id_all,table4_all))
data5=data.frame(cbind(content_id_all, contents_all))

colnames(data5) = c('id','제안이유 및 주요내용')
data3=data3[,-2]
data4=data4[,-2]

wb=createWorkbook()
addWorksheet(wb, 'Sheet1')
addWorksheet(wb, 'Sheet2')
addWorksheet(wb, 'Sheet3')
addWorksheet(wb, 'Sheet4')
addWorksheet(wb, 'Sheet5')
addWorksheet(wb, 'Sheet6')

writeData(wb,sheet=1, data0)
writeData(wb,sheet=2, data1)
writeData(wb,sheet=3, data2)
writeData(wb,sheet=4, data3)
writeData(wb,sheet=5, data4)
writeData(wb,sheet=6, data5)

saveWorkbook(wb, '국회_sample.xlsx', overwrite=TRUE)
pp=0
for(i in hwp_url_all[,1]){
  pp=pp+1
  print(pp)
  remDr$navigate(i)
  Sys.sleep(0.5)
}


