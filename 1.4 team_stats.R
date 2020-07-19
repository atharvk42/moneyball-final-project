
team_stats <- 
  final_data_per_game %>%
  select(home_team)

team_stats <- team_stats[!duplicated(team_stats$home_team), ]
colnames(team_stats) <- c("Team")

win_tbl <- as.data.frame(table(final_data_per_game$winning_team))
colnames(win_tbl) <- c("Team", "Wins")

loss_tbl <- as.data.frame(table(final_data_per_game$losing_team))
colnames(loss_tbl) <- c("Team", "Losses")

rushing_yards_per_team_tbl <- 
  final_data %>%
  group_by(home_team) %>%
  filter(play_type == 'run') 

rushing_yards_per_team_tbl <- aggregate(x = rushing_yards_per_team_tbl$yards_gained,               
                                    by = list(rushing_yards_per_team_tbl$home_team),        
                                    FUN = sum)
colnames(rushing_yards_per_team_tbl) <- c("Team", "rushing_yards")

passing_yards_per_team_tbl <- 
  final_data %>%
  group_by(home_team) %>%
  filter(play_type == 'pass') 

passing_yards_per_team_tbl <- aggregate(x = passing_yards_per_team_tbl$yards_gained,               
                                        by = list(passing_yards_per_team_tbl$home_team),        
                                        FUN = sum)
colnames(passing_yards_per_team_tbl) <- c("Team", "passing_yards")

ypc_tbl <- final_data
ypc_tbl <-
  ypc_tbl %>%
  filter(winning_team == posteam, play_type == "run")

ypc_tbl <- aggregate(x = ypc_tbl$yards_gained,               
                     by = list(ypc_tbl$home_team),        
                     FUN = mean)

colnames(ypc_tbl) <- c("Team", "YPC")

ypa_tbl <- final_data
ypa_tbl <-
  ypa_tbl %>%
  filter(winning_team == posteam, play_type == "pass")

ypa_tbl <- aggregate(x = ypa_tbl$yards_gained,               
                                        by = list(ypa_tbl$home_team),        
                                        FUN = mean)

colnames(ypa_tbl) <- c("Team", "YPA")

team_stats <-
  team_stats %>%
  inner_join(win_tbl, by = "Team") %>%
  inner_join(loss_tbl, by = "Team") %>%
  inner_join(rushing_yards_per_team_tbl, by = "Team") %>%
  inner_join(passing_yards_per_team_tbl, by = "Team")


team_stats <-
  team_stats %>%
  mutate("winning_percentage" = (Wins)/(Wins+Losses)) %>%
  mutate("percentage_yards_rushing" = rushing_yards_per_team_tbl$rushing_yards/(rushing_yards_per_team_tbl$rushing_yards+passing_yards_per_team_tbl$passing_yards)) %>%
  mutate("YPC" = ypc_tbl$YPC) %>%
  mutate("YPA" = ypa_tbl$YPA)
