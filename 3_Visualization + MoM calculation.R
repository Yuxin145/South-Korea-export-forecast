install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)

#import organized data
data_84 = read.csv("D:/Users/peter/Desktop/Invest Society/data/divided by commodity/2022-2017_Monthly_84_World.csv")[,2:4]

#visualize to determine whether the data has relatively strong Seasonality
data_84 = data.frame(RefTime = paste(as.character(data_84$RefYear),as.character(data_84$RefQuarter),sep='-'),PrimaryValue = data_84$PrimaryValue)
ggplot(QoQ_84,aes(x=RefQuarter,y=QoQ))+geom_point()+geom_line(colour='blue',aes(group=1))+facet_wrap(~RefYear)


#MoM(Month on Month)/QoQ(Quarter on Quarter) data
data_84= data_84[order(data_84$RefYear),]
MoM_84 = data.frame(RefYear = numeric(0),RefMonth = numeric(0),MoM = numeric(0))

for (i in 1:nrow(data_84)){
  temp = data_84[i,]
  if (temp$RefYear>=2018){
    temp1 = data_84[i-1,]
    MoM_84 = MoM_84%>%
      rows_append(data.frame(RefYear=temp$RefYear,RefMonth=temp$RefMonth,
                             MoM=(temp$PrimaryValue-temp1$PrimaryValue)/temp1$PrimaryValue)
                  )
  }
}

write.csv(MoM_84,'D:/Users/peter/Desktop/Invest Society/data/Calculated data/2022-2018_MoM_84_World.csv')

