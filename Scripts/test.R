library(readr)
library(dplyr)
library(ggplot2)
library(fuzzyjoin)

playoffs <- data.frame(c('Milwaukee Bucks*','Golden State Warriors*','Toronto Raptors*',
                         'Utah Jazz*','Houston Rockets*','Portland Trail Blazers*',
                         'Denver Nuggets*','Boston Celtics*','Oklahoma City Thunder*',
                         'Indiana Pacers*','Philadelphia 76ers*','San Antonio Spurs*',
                         'Los Angeles Clippers*','Orlando Magic*','Brooklyn Nets*',
                         'Miami Heat','Detroit Pistons*','Sacramento Kings','Dallas Mavericks',
                         'Minnesota Timberwolves','New Orleans Pelicans','Charlotte Hornets',
                         'Los Angeles Lakers','Memphis Grizzlies','Washington Wizards',
                         'Atlanta Hawks','Chicago Bulls','Phoenix Suns','New York Knicks',
                         'Cleveland Cavaliers'),
                       c('MIL','GSW','TOR','UTA','HOU','POR','DEN','BOS','OKC','IND','PHI',
                         'SAS','LAC','ORL','BRK','MIA','DET','SAC','DAL','MIN','NOP',
                         'CHA','LAL','MEM','WAS','ATL','CHI','PHO','NYK','CLE'),
                       c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0))

names(playoffs) <- c('Team','Ac','Playoff')


# team_stats_2019 <- read_csv("C:/Users/Ashwin's Computer/Desktop/Basketball/Data/team_stats_2019.csv", skip = 1) %>%
#   mutate(Team = gsub('\\*','',Team))
team_stats_2019 <- read_csv("~/Desktop/Basketball/Data/team_stats_2019.csv",skip=1) %>%
  select(Team,W,L)

team_stats_2019 <- team_stats_2019 %>%
  merge(playoffs,by='Team') %>%
  select(-Team)


total_df = data.frame()

for(team in c('TOR','ATL','LAL','OKC','MIA','HOU','GSW','PHO','CLE','POR','DEN','UTA','SAS',
              'LAC','SAC','MIN','MEM','NOP','DAL','MIL','PHI','BOS','IND','BRK','ORL','DET',
              'CHA','WAS','CHI','NYK')){
  filename = paste0("~/Desktop/Basketball/Data/",team,"_team_totals_2019.csv")
  # filename = paste0("C:/Users/Ashwin's Computer/Desktop/Basketball/Data/",team,"_team_totals_2019.csv")
  team_totals = read_csv(filename) %>%
    rename(Player = X2) 
  
  filename = paste0("~/Desktop/Basketball/Data/",team,"_roster_2019.csv")
  # filename = paste0("C:/Users/Ashwin's Computer/Desktop/Basketball/Data/",team,"_roster_2019.csv")
  roster <- read_csv(filename)

  a <- team_totals %>% 
    merge(roster,by='Player') %>%
    mutate(Player = gsub('\\\\.*$','',Player),
           Team = team,
           PER = (PTS + TRB + AST + STL + BLK - (FGA-FG) - (FTA-FT) - TOV)/G) %>%
    filter(G > (.25*82)) %>% 
    arrange(desc(PER)) %>%
    slice(1:12)
  
  a <- a %>%
    merge(a %>% 
            group_by(Pos) %>%
            summarise(NumPosition = n()), by='Pos')
  
  total_df <- rbind(total_df,a)
}

final <- total_df %>%
  mutate(Pos = ifelse(Pos=='PG',1,
                      ifelse(Pos=='SG',2,
                             ifelse(Pos=='SF',3,
                                    ifelse(Pos=='PF',4,5)))), # rename position to numerics
         `3P%` = ifelse(is.na(`3P%`),0,`3P%`),
         Exp = as.integer(ifelse(Exp=='R',0,Exp))) %>%
  merge(team_stats_2019,by.x='Team',by.y='Ac')
  

test <- final %>%
  group_by(Team) %>%
  summarise(PER = sum(PER),
            Pos = mean(Pos),
            efg = mean(`eFG%`),
            fgp = mean(`FG%`),
            Playoff=mean(Playoff),
            `3PA` = mean(`3PA`),
            P3 = mean(`3P%`),
            FTA = mean(FTA),
            `FT` = mean(`FT%`),
            AST = sum(AST),
            TOV = sum(TOV),
            STL = sum(STL),
            experience = mean(Exp),
            Age = mean(Age),
            W = trunc(mean(W)/10)*10,
            L = mean(L),
            orb = sum(ORB))


ggplot(data=test,aes(Team,W,fill=Playoff)) +
  geom_col() #+facet_wrap(vars(Team))


model <- lm(W ~ FT + experience + P3 - 1,data=test)
summary(model)
plot(model$residuals)

pred_data <- data.frame(PER=159,efg=.542,fgp=.483,FTA=152,FT=.786,P3=.319,AST=1783,
                        Age=26.08,Pos=3.16,experience=5.08,W=50,orb=933)
predict(model,pred_data)


