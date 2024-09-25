install.packages("dplyr")
library(dplyr)

################
data = select(read.csv("D:/Users/peter/Desktop/Invest Society/data/2022_Monthly_ALL HS code_World.csv"),RefYear,RefMonth,CmdCode,PrimaryValue)
data$PrimaryValue = data$PrimaryValue/1000000
data_spec = data[which(data$CmdCode!='TOTAL'),]

#find primary commodities 
CmdCode_collec = data_spec%>%
  group_by(data_spec$CmdCode)%>%
  summarize(TotalValue = sum(PrimaryValue))
top_5=CmdCode_collec[order(-CmdCode_collec$TotalValue),][1:5,]

#divide the data by primary commodities and other commodities (month)
data_20xx_mon=data_spec[which(data_spec$CmdCode %in% top_5$`data_spec$CmdCode`),]
for (month in 1:12){
  PrimaryCom = sum(data_20xx_mon[which(data_20xx_mon$RefMonth == month),]$PrimaryValue)
  TotalCom = sum(data_spec[which(data_spec$RefMonth == month),]$PrimaryValue,na.rm=T)
  data_20xx_mon = data_20xx_mon%>%
    rows_append(data.frame(RefYear = data_20xx_mon[1,]$RefYear,RefMonth = month,CmdCode='Other',PrimaryValue = TotalCom-PrimaryCom))
} 
data_20xx_mon=data_20xx_mon[order(data_20xx_mon$CmdCode),]

#calculate quarter data 
data_20xx_q = data.frame(RefYear=numeric(0),RefQuarter=character(0),CmdCode=character(0),PrimaryValue=numeric(0))
for (CmdCode in c(top_5$`data_spec$CmdCode`,'Other')){
  index = c('Q1','Q2','Q3','Q4')
  for (i in 1:4){
    data_20xx_q = data_20xx_q%>%
      rows_append(data.frame(RefYear = data_20xx_mon[1,]$RefYear,RefQuarter = index[i],CmdCode=CmdCode,PrimaryValue = sum(data_20xx_mon[which(data_20xx_mon$RefMonth<=3*i & data_20xx_mon$RefMonth>3*(i-1) & data_20xx_mon$CmdCode==CmdCode),]$PrimaryValue,.rm=T)))
  }
}
data_20xx_q=data_20xx_q[order(data_20xx_q$CmdCode),]

#write the organized data into csv file
write.csv(data_20xx_mon, "D:/Users/peter/Desktop/Invest Society/data/divided by year/2022_Monthly_top6_World.csv")
write.csv(data_20xx_q, "D:/Users/peter/Desktop/Invest Society/data/divided by year/2022_Quarterly_top6_World.csv")
