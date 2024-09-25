MoM_84 = read.csv('D:/Users/peter/Desktop/Invest Society/data/Calculated data/2022-2018_MoM_84_World.csv')[,2:4]

#Modify the MoM data to prepare it for regression analysis
vec = rep(0,12)

temp = rep(replace(vec,1,1),5)
MoM_84 = MoM_84%>%
  mutate(mon1=temp)

temp = rep(replace(vec,2,1),5)
MoM_84 = MoM_84%>%
  mutate(mon2=temp)

temp = rep(replace(vec,3,1),5)
MoM_84 = MoM_84%>%
  mutate(mon3=temp)

temp = rep(replace(vec,4,1),5)
MoM_84 = MoM_84%>%
  mutate(mon4=temp)

temp = rep(replace(vec,5,1),5)
MoM_84 = MoM_84%>%
  mutate(mon5=temp)

temp = rep(replace(vec,6,1),5)
MoM_84 = MoM_84%>%
  mutate(mon6=temp)

temp = rep(replace(vec,7,1),5)
MoM_84 = MoM_84%>%
  mutate(mon7=temp)

temp = rep(replace(vec,8,1),5)
MoM_84 = MoM_84%>%
  mutate(mon8=temp)

temp = rep(replace(vec,9,1),5)
MoM_84 = MoM_84%>%
  mutate(mon9=temp)

temp = rep(replace(vec,10,1),5)
MoM_84 = MoM_84%>%
  mutate(mon10=temp)

temp = rep(replace(vec,11,1),5)
MoM_84 = MoM_84%>%
  mutate(mon11=temp)

temp = rep(replace(vec,12,1),5)
MoM_84 = MoM_84%>%
  mutate(mon12=temp)

#Use regression analysis and Walk forward analysis to forecast the MoM data from 2023-2025 
year_index= 2018:2025
month_index= 1:12

for (i in 1:3){
  start_year = year_index[i]
  goal_year = year_index[i+5]
  scope_index = which(MoM_84$RefYear>=start_year & MoM_84$RefYear<goal_year)
  
  MoM = MoM_84[scope_index,]$MoM
  RefYear = MoM_84[scope_index,]$RefYear
  mon1 = MoM_84[scope_index,]$mon1
  mon2 = MoM_84[scope_index,]$mon2
  mon3 = MoM_84[scope_index,]$mon3
  mon4 = MoM_84[scope_index,]$mon4
  mon5 = MoM_84[scope_index,]$mon5
  mon6 = MoM_84[scope_index,]$mon6
  mon7 = MoM_84[scope_index,]$mon7
  mon8 = MoM_84[scope_index,]$mon8
  mon9 = MoM_84[scope_index,]$mon9
  mon10 = MoM_84[scope_index,]$mon10
  mon11 = MoM_84[scope_index,]$mon11
  mon12 = MoM_84[scope_index,]$mon12
  
  
  mod = lm(MoM~I(RefYear-start_year+1)
           +mon1+mon2+mon3+mon4+mon5+mon6+mon7+mon8+mon9+mon10+mon11+mon12,data=MoM_84)
  nextyear = tail(MoM_84,12)[,c(1,2,4,5,6,7,8,9,10,11,12,13,14,15)]
  nextyear$RefYear = rep(nextyear$RefYear[1]+1,12)
  nextyear = nextyear%>%
    mutate(MoM = predict(mod,newdata=nextyear),.after = RefMonth)
  MoM_84 = MoM_84%>%
    rows_append(nextyear)
}


#write.csv(MoM_84,'D:/Users/peter/Desktop/Invest Society/data/Forecasted data/2025-2018_MoM_84_World.csv')


#Use forecasted MoM from 2023-2025 to forecast the export value
forecast_84 = tail(data_84,1)
MoM_index = which(MoM_84$RefYear>2022)

for (i in MoM_index){
  cur_year = MoM_84[i,]$RefYear
  cur_month = MoM_84[i,]$RefMonth
  cur_MoM = MoM_84[i,]$MoM
  prev_data = tail(forecast_84,1)$PrimaryValue
  temp = data.frame(RefYear=cur_year,RefMonth=cur_month,PrimaryValue=prev_data*(1+cur_MoM))
  forecast_84 = forecast_84%>%
    rows_append(temp)
}
final_84 = data_84%>%
  rows_insert(forecast_84,conflict = "ignore")


#Use forecasted export value of each commodity to calculate to forecasted total export value
Commo_index = c('84','84','87','8542','854232','Other')
final_total = data.frame(RefYear = numeric(0),RefMonth = numeric(0),PrimaryValue=numeric(0))
for (i in 1:nrow(final_84)){
  cur_total = 0
  cur_year = final_84[i,]$RefYear
  cur_month = final_84[i,]$RefMonth
  for (j in Commo_index){
    cur_total = cur_total + get(paste('final',j,sep='_'))[i,]$PrimaryValue
  }
  temp = data.frame(RefYear=cur_year,RefMonth=cur_month,PrimaryValue=cur_total)
  final_total = final_total%>%
    rows_append(temp)
}

#visualize final result
ggplot(final_total,aes(x=RefMonth,y=PrimaryValue))+geom_point()+geom_line(colour='blue',aes(group=1))+facet_wrap(~RefYear)
final_total[25:36,]

write.csv(final_total,'D:/Users/peter/Desktop/Invest Society/data/final/2025-2017_Monthly_total_World.csv')
