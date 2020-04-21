library(dplyr)

TOR_roster_2019 <- read_csv("Desktop/basketballProject/TOR_roster_2019.csv")
TOR_per_game_2019 <- read_csv("Desktop/basketballProject/TOR_per_game_2019.csv")
team_stats_2019 <- read_csv("Desktop/basketballProject/team_stats_2019.csv", skip = 1) %>%
  mutate(Team = gsub('\\*','',Team))

total_df = data.frame()

for(team in c('TOR','ATL','LAL','OKC','MIA','HOU','GSW','PHO','CLE','POR')){
  filename = paste0("Desktop/basketballProject/",team,"_team_totals_2019.csv")
  team_totals = read_csv(filename) %>%
    rename(Player = X2) 
  
  filename = paste0("Desktop/basketballProject/",team,"_roster_2019.csv")
  roster = read_csv(filename)

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
            summarise(Totals = n()), by='Pos')
  
  total_df <- rbind(total_df,a)
}


test <- total_df %>%
  group_by(Team) %>%
  summarise(PER = sum(PER))


barplot(test$PER,names.arg = test$Team)