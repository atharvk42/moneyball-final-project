rushing <- final_data
passing <- final_data

rushing <-
  rushing %>%
  group_by(game_id) %>%
  filter(play_type == 'run' & winning_team == posteam) %>%
  
  mutate(rushing_yards_winning_team = sum(yards_gained)) %>%
  mutate(ypc = mean(yards_gained)) %>%
  select(game_id, rushing_yards_winning_team, ypc)

rushing <- rushing[!duplicated(rushing$game_id), ]

passing <- passing %>%
  group_by(game_id) %>%
  filter(play_type == 'pass' & winning_team == posteam) %>%
  mutate(passing_yards_winning_team = sum(yards_gained)) %>%
  mutate(ypa = mean(yards_gained)) %>%
  select(game_id, passing_yards_winning_team, ypa)

passing <- passing[!duplicated(passing$game_id), ]

final_data_per_game <- final_data
final_data_per_game <- final_data_per_game[!duplicated(final_data_per_game$game_id), ]

final_data_per_game <-
  final_data_per_game %>%
  filter(winning_team != "tie") %>%
  select('game_id', 'home_team', 'away_team', 'game_date', 'home_score', 'away_score', 'winning_team', 'losing_team')

final_data_per_game <-
  final_data_per_game %>%
  inner_join(rushing, by = 'game_id') %>%
  inner_join(passing, by = 'game_id')

final_data_per_game <-
  final_data_per_game %>%
  mutate(percentage_yards_rushing = rushing_yards_winning_team/(rushing_yards_winning_team+passing_yards_winning_team))

ggplot(data = final_data_per_game) + 
  geom_point(aes(x = rushing_yards_winning_team, y = passing_yards_winning_team, color = winning_team, alpha = 0.2)) +
  labs(x = "Rushing Yards", y = "Passing Yards") + 
  geom_abline(intercept = 297.8966416, slope = -0.4396726, color = "black")

cor(final_data_per_game$ypc, final_data_per_game$ypa)

ggplot(data = final_data_per_game) + 
  geom_point(aes(x = ypc, y = ypa, color = winning_team, alpha = 0.2)) +
  geom_abline(intercept = 7.32429203, slope = -0.03153615, color = "black")
