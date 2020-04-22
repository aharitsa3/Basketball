library(dplyr)
library(cpm)
library(ggplot2)

fulldf <- data.frame()
for (season in c('04-05','05-06','06-07','07-08','08-09','09-10','10-11',
                 '11-12','12-13','13-14','14-15','15-16','16-17','17-18',
                 '18-19','19-20')){
  filename <- paste0("~/Desktop/Basketball/Data/FT Predictions/Howard_",season,'.csv')
  df <- read_csv(filename) %>%
    mutate(Season = season,
           `FT%` = ifelse(is.na(`FT%`),0,`FT%`),
           FT = ifelse(FT == 'Player Suspended',0,
                       ifelse(FT == 'Not With Team',0,
                              ifelse(FT == 'Inactive',0,
                                     ifelse(FT == 'Did Not Play',0,
                                            ifelse(FT == 'Did Not Dress',0,FT))))),
           FTA = ifelse(FTA == 'Player Suspended',0,
                        ifelse(FTA == 'Not With Team',0,
                               ifelse(FTA == 'Inactive',0,
                                      ifelse(FTA == 'Did Not Play',0,
                                             ifelse(FTA == 'Did Not Dress',0,FTA))))),
           `FT%` = ifelse(`FT%` == 'Player Suspended',0,
                          ifelse(`FT%` == 'Not With Team',0,
                                 ifelse(`FT%` == 'Inactive',0,
                                        ifelse(`FT%` == 'Did Not Play',0,
                                               ifelse(`FT%` == 'Did Not Dress',0,`FT%`))))))
  fulldf <- rbind(fulldf,df)
}

fulldf <- fulldf%>%
  select(FT,FTA,`FT%`,Season) %>%
  mutate(totalFT = 'NA',
         totalMade = 'NA',
         rownum=row_number())
fulldf$totalFT[1]=0
fulldf$totalMade[1] = 0
for (row in 2:nrow(fulldf)){
  fulldf$totalFT[row] = strtoi(fulldf$totalFT[row-1]) + strtoi(fulldf$FTA[row])
  fulldf$totalMade[row] = strtoi(fulldf$totalMade[row-1]) + strtoi(fulldf$FT[row])
}

# fulldf <- fulldf %>%
#   mutate(rollingAvg = strtoi(totalMade)/strtoi(totalFT),
#          rollingAvg = ifelse(is.na(rollingAvg),0,rollingAvg))
fulldf <- fulldf %>%
  mutate(FT = strtoi(FT),
         FTA = strtoi(FTA),
         `FT%` = as.double(`FT%`),
         totalFT = strtoi(totalFT),
         totalMade = strtoi(totalMade))


fulldf <- fulldf %>%
  mutate(lag1FT = lag(FT,1),
         lag1FTA = lag(FTA,1),
         lag2FT = lag(FT,2),
         lag2FTA = lag(FTA,2),
         lag3FT = lag(FT,3),
         lag3FTA = lag(FTA,3),
         lag4FT = lag(FT,4),
         lag4FTA = lag(FTA,4),
         lag5FT = lag(FT,5),
         lag5FTA = lag(FTA,5),
         lag6FT = lag(FT,6),
         lag6FTA = lag(FTA,6),
         lag7FT = lag(FT,7),
         lag7FTA = lag(FTA,7),
         lag8FT = lag(FT,8),
         lag8FTA = lag(FTA,8),
         lag9FT = lag(FT,9),
         lag9FTA = lag(FTA,9),
         lag10FT = lag(FT,10),
         lag10FTA = lag(FTA,10),
         rollingAvg = (lag1FT+lag2FT+lag3FT+lag4FT+lag5FT+lag6FT + lag7FT+lag8FT+lag9FT+lag10FT)/
           (lag1FTA+lag2FTA+lag3FTA+lag4FTA+lag5FTA+lag6FTA+lag7FTA+lag8FTA+lag9FTA+lag10FTA),
         rollingAvg = ifelse(is.na(rollingAvg),0,rollingAvg))

a <- processStream(fulldf$rollingAvg,cpmType = 'Kolmogorov-Smirnov',startup=30)
changepoints <- data.frame(rownum = a[['detectionTimes']]) %>%
  mutate(cpd=1)

fulldf <- fulldf %>%
  merge(changepoints, by='rownum',all.x=T) %>%
  mutate(cpd = ifelse(is.na(cpd),0,1),
         prediction=0)

for (row in 2:nrow(fulldf)){
  if (fulldf$cpd[row]==1){
    fulldf$prediction[row] = fulldf$rollingAvg[row]
  }
  else {
    fulldf$prediction[row] = (fulldf$rollingAvg[row-1] * fulldf$totalFT[row]) / (fulldf$totalFT[row])
  }
}

fulldf <- fulldf %>%
  mutate(trueAvg = totalMade/totalFT)


###### CONFUSED ON HOW TO CALCULATE TRUE FREE THROW PERCENTAGE ######

ggplot(data=fulldf,aes(rownum,trueAvg)) +
  geom_line() +
  geom_line(data=fulldf,aes(rownum,prediction,col='red'))
