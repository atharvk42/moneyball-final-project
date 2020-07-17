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
  filter(winning_team == posteam, play_type == "run") %>%
  group_by(home_team) %>%
  mutate("ypc" = mean(yards_gained))

ypc_tbl <- ypc_tbl[!duplicated(ypc_tbl$home_team), ]

ypc_tbl <- select(ypc_tbl, 'ypc')

colnames(ypc_tbl) <- c("Team", "YPC")

ypa_tbl <- final_data
ypa_tbl <-
  ypa_tbl %>%
  filter(winning_team == posteam, play_type == "pass") %>%
  group_by(home_team) %>%
  mutate("ypa" = mean(yards_gained))

ypa_tbl <- ypa_tbl[!duplicated(ypa_tbl$home_team), ]

ypa_tbl <- select(ypa_tbl, 'ypa')

colnames(ypa_tbl) <- c("Team", "YPA")

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
  mutate("YPC" = ypc_tbl$YPC) %>%
  mutate("YPA" = ypa_tbl$YPA)


cor(team_stats$percentage_yards_rushing, team_stats$winning_percentage)
ggplot(data = team_stats) + 
  geom_point(aes(x = percentage_yards_rushing, y = winning_percentage, color = Team)) +
  labs(x = "Percentage Yards Rushing", y = "Winning Percentage") + 
  geom_abline(intercept = 0.8119663, slope = -0.9474407 , color = "black")
  
ggplot(data = team_stats) + 
  geom_point(aes(x = YPC, y = winning_percentage, color = Team)) +
  labs(x = "YPC", y = "Winning Percentage") +
  geom_abline(intercept = 0.36443022, slope = 0.03027151 , color = "black")

ggplot(data = team_stats) + 
  geom_point(aes(x = YPA, y = winning_percentage, color = Team)) +
  labs(x = "YPA", y = "Winning Percentage") +
  geom_abline(intercept = -0.17837374, slope = 0.09528795 , color = "black")

cor(team_stats$ypc, team_stats$winning_percentage)

cor(team_stats$ypa, team_stats$winning_percentage)

fit <- lm(formula = winning_percentage ~ ypc,  data =  team_stats)

fit[["coefficients"]]
