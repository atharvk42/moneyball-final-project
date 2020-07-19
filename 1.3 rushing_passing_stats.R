final_data_per_game <- final_data
final_data_per_game <- final_data_per_game[!duplicated(final_data_per_game$game_id), ]

rushing <- final_data
rushing <-
  rushing %>%
  filter(play_type == 'run' & winning_team == posteam) 
rushing_yards_per_game <- aggregate(x = rushing$yards_gained,               
                                    by = list(rushing$game_id),        
                                    FUN = sum)
colnames(rushing_yards_per_game) <- c("game_id", "rushing_yards")

rushing <- rushing[!duplicated(rushing$game_id), ]
rushing <- rushing %>%
  mutate(rushing_yards_winning_team = rushing_yards_per_game$rushing_yards) %>%
  select(game_id, rushing_yards_winning_team)

passing <- final_data
passing <-
  passing %>%
  filter(play_type == 'pass' & winning_team == posteam) 
passing_yards_per_game <- aggregate(x = passing$yards_gained,               
                                    by = list(passing$game_id),        
                                    FUN = sum)
colnames(passing_yards_per_game) <- c("game_id", "passing_yards")

passing <- passing[!duplicated(passing$game_id), ]
passing <- passing %>%
  mutate(passing_yards_winning_team = passing_yards_per_game$passing_yards) %>%
  select(game_id, passing_yards_winning_team)

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
