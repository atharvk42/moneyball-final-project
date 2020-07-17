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
  filter(play_type == 'run') %>%
  mutate(rushing_yards_per_team_tbl = sum(yards_gained)) %>%
  select('home_team', 'rushing_yards_per_team_tbl')

rushing_yards_per_team_tbl <- rushing_yards_per_team_tbl[!duplicated(rushing_yards_per_team_tbl$home_team), ]

passing_yards_per_team_tbl <- 
  final_data %>%
  group_by(home_team) %>%
  filter(play_type == 'pass') %>%
  mutate(passing_yards_per_team_tbl = sum(yards_gained)) %>%
  select('home_team', 'passing_yards_per_team_tbl')

passing_yards_per_team_tbl <- passing_yards_per_team_tbl[!duplicated(passing_yards_per_team_tbl$home_team), ]

ypc_tbl <- final_data
ypc_tbl <-
  ypc_tbl %>%
  group_by(home_team) %>%
  mutate("ypc" = mean(yards_gained)) %>%
  filter(winning_team == posteam, play_type == "run")


ypc_tbl <- ypc_tbl[!duplicated(ypc_tbl$home_team), ]

ypc_tbl <- select(ypc_tbl, 'game_id', 'ypc')

team_stats <-
  team_stats %>%
  inner_join(win_tbl, by = "Team") %>%
  inner_join(loss_tbl, by = "Team")

team_stats <-
  team_stats %>%
  mutate("rushing_yards_per_team_tbl" = rushing_yards_per_team_tbl$rushing_yards_per_team_tbl) %>%
  mutate("passing_yards_per_team_tbl" = passing_yards_per_team_tbl$passing_yards_per_team_tbl)

team_stats <-
  team_stats %>%
  mutate("winning_percentage" = (Wins)/(Wins+Losses)) %>%
  mutate("percentage_yards_rushing" = rushing_yards_per_team_tbl/(rushing_yards_per_team_tbl+passing_yards_per_team_tbl)) %>%
  mutate("ypc" = ypc_tbl$ypc)

cor(team_stats$percentage_yards_rushing, team_stats$winning_percentage)
ggplot(data = team_stats) + 
  geom_point(aes(x = percentage_yards_rushing, y = winning_percentage, color = Team)) +
  labs(x = "Percentage Yards Rushing", y = "Winning Percentage") + 
  geom_abline(intercept = 0.8119663, slope = -0.9474407 , color = "black")
  
ggplot(data = team_stats) + 
  geom_point(aes(x = ypc, y = winning_percentage, color = Team)) +
  labs(x = "YPC", y = "Winning Percentage") +
  geom_abline(intercept = -0.3881530, slope = 0.1611404 , color = "black")

cor(team_stats$ypc, team_stats$winning_percentage)

