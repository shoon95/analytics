library(textutils)
library(rvest)
library(dplyr)
library(openxlsx)
library(stringr)
library(httr)
library(jsonlite)
library(stringi)


setwd('D:\\±¹È¸ ÀÚ¹®\\CSV')

paper=list.files(pattern = '.csv')

excel_files=gsub('.csv','.xlsx',paper)
excel_files_name = paste0('D:/±¹È¸ ÀÚ¹®/XLSX','/', excel_files)

for( i in 1:length(paper)){ #i =length(paper)
  print(i)
  
  data_csv = read.csv(paper[i],row.names=NULL)
  
  write.xlsx(data_csv, excel_files_name[i])
}

setwd('D:\\±¹È¸ ÀÚ¹®\\XLSX')

paper = list.files(pattern='.xlsx')

problem_1_all = c()
problem_2_all = c()
data_1_all =c()
data_2_all =c()
problem_3_all = c()
problem_4_all = c()
id_all = c()
id_2_all = c()
id_3_all = c()
id_4_all=c()

page=0

for(i in paper){ # i = paper[3]
  
  page=page+1
  
  print(page)
 
  
  data = read.xlsx(i) 
  
  data[,1]=HTMLdecode(data[,1])
  
  check = grep('???|???', data[,1])
  
  start_1 = check[grep('¥°',data[check-1,1])]-1
  
  
  end_1 = check[grep('¥±',data[check-1,1])]-2
  
  start_2 = check[grep('¥±',data[check-1,1])]-1

  end_2 = check[grep('¥²',data[check-1,1])]-2
  if(length(end_2)==0){
    end_2 = check[grep('¥²',data[check-2,1])]-3
  }
  
  start_3 = check[grep('¥²',data[check-1,1])]-1
  if(length(start_3)==0){
    start_3 = check[grep('¥²',data[check-2,1])]-2
  }
  end_3 = check[grep('¥³',data[check-1,1])]-2
  
  if(length(end_3)==0){
    end_3 = check[grep('¥³',data[check-2,1])]-3
  }
  
  if(length(end_3)==0){
    end_3 = check[grep('¥³',data[check-3,1])]-4
  }
  
  
  if(grepl('¥³',data)==FALSE){
    end_3 = length(data[,1])
    
    
  }else{
    end_4 = length(data[,1])
    
    start_4 = check[grep('¥³',data[check-1,1])]-1
    if(length(start_4)==0){
      start_4 = check[grep('¥³',data[check-2,1])]-2
      
    }
    if(length(start_4)==0){
      start_4 = check[grep('¥³',data[check-3,1])]-3
    }
  }
  
  
  
  data_1=data[start_1:end_1,1]
  data_2=data[start_2:end_2,1]
  data_3=data[start_3:end_3,1]
  
  if(length(start_4)==0){
    data_4 = NA
  }else{
    data_4=data[start_4:end_4,1]}
  start_4 =c()


  ###1¹ø
  
  t=grep('°¨¼Ò|Áõ°¡',data_1,value=TRUE)
  
  problem_1=str_extract(t, '°¨¼Ò|Áõ°¡')
  
  if(length(problem_1)==0){
    problem_1 = NA
    t= NA
  }
  data_1_all = append(data_1_all,t)
  problem_1_all = append(problem_1_all, problem_1)
  
  id = gsub('[^0-9]','',i)
  id_1 = rep(id, length(problem_1))
  id_all = append(id_all, id_1)
  
  
  #### 2¹ø
  
  problem_2=str_extract(data_2,'\\(¾È .*\\){1}')
  problem_2=gsub('\\).*\\(','',problem_2)
  
  problem_2_re=problem_2[grep('[°¡-ÆR]',problem_2)]
  
  if(length(problem_2_re)==0){
    problem_2_all=append(problem_2_all, NA)
    data_2_all = append(data_2_all, NA)
    id_2_all = append(id_2_all, id)
    
  }else{
    problem_2_all = append(problem_2_all, problem_2_re)
    
    data_2_all = append(data_2_all, data_2[grep('[°¡-ÆR]',problem_2)])
    
    id_2 = rep(id, length(problem_2_re))
    id_2_all = append(id_2_all, id_2)
  }


  
  
  
  ###### 3¹ø
  
  problem_3_all = append(problem_3_all, data_3)
  
  id_3 = rep(id, length(data_3))
  id_3_all = append(id_3_all, id_3)
  
  ####### 4¹ø
  
  problem_4_all = append(problem_4_all, data_4)
  
  id_4 = rep(id, length(data_4))
  id_4_all = append(id_4_all, id_4)
  
}

df1 = data.frame('ID' = id_all, 'DATA' = data_1_all,'ÃßÃâ' = problem_1_all)
df2 = data.frame('ID' = id_2_all, 'DATA ' = data_2_all, 'ÃßÃâ' = problem_2_all)
df3 = data.frame('ID' = id_3_all, 'ÃßÃâ'= problem_3_all)
df4 = data.frame('ID'=id_4_all, 'ÃßÃâ'=problem_4_all)

#### Á¦´ë·Î µé¾î¿ÍÁ³´ÂÁö idÀÇ Ã¹ ¹øÂ° µ¥ÀÌÅÍµé¸¸ ÃßÃâ

e3 = df3[-which(duplicated(df3$ID)),]
e4 = df4[-which(duplicated(df4$ID)),]

length(unique(df2$ID))

table(e3$ÃßÃâ)
table(e4$ÃßÃâ)

save.image('data.Rdata')
