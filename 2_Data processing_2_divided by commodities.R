#divide the quarterly data by types of commodities
data_index = c('data_2022','data_2021','data_2020','data_2019','data_2018','data_2017')
CmdCode_index = c('84','85','8542','854232','87','Other')

data_2022 = read.csv("D:/Users/peter/Desktop/Invest Society/data/divided by year/2022_Monthly_top6_World.csv")[,2:5]
data_2021 = read.csv("D:/Users/peter/Desktop/Invest Society/data/divided by year/2021_Monthly_top6_World.csv")[,2:5]
data_2020 = read.csv("D:/Users/peter/Desktop/Invest Society/data/divided by year/2020_Monthly_top6_World.csv")[,2:5]
data_2019 = read.csv("D:/Users/peter/Desktop/Invest Society/data/divided by year/2019_Monthly_top6_World.csv")[,2:5]
data_2018 = read.csv("D:/Users/peter/Desktop/Invest Society/data/divided by year/2018_Monthly_top6_World.csv")[,2:5]
data_2017 = read.csv("D:/Users/peter/Desktop/Invest Society/data/divided by year/2017_Monthly_top6_World.csv")[,2:5]

for (Cmd in 1:6){
  temp = data.frame(RefYear=numeric(0),RefMonth=numeric(0),PrimaryValue=numeric(0))
  for (data in 1:6){
    temp=temp%>%
      rows_append(get(data_index[data])[which(get(data_index[data])$CmdCode==CmdCode_index[Cmd]),c(1,2,4)])
  }
  assign(paste('data_',CmdCode_index[Cmd]),temp)
}

write.csv(`data_ Other`,'D:/Users/peter/Desktop/Invest Society/data/divided by commodity/2022-2017_Monthly_Other_World.csv')
