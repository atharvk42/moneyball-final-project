##COMBINE YEARS
pbp_all <- rbind(pbp09, pbp10, pbp11, pbp12, pbp13, pbp14, pbp15, pbp16, pbp17, pbp18, pbp19)
game_data_all <- rbind(game_data09, game_data10, game_data11, game_data12, game_data13, game_data14, game_data15, game_data16, game_data17, game_data18, game_data19) 

# Get the data we want
pbp_all <-
  pbp_all %>%
  select('game_id', 'home_team', 'away_team', 'posteam', 'game_date', 'half_seconds_remaining', 'down', 'ydstogo', 'ydsnet', 'yards_gained', 'play_type', 'td_team')

game_data_all <-
  game_data_all %>%
  select('game_id', 'home_score', 'away_score')



#Combine the play by play and game data
final_data  <-
  pbp_all %>%
  inner_join(game_data_all, by = "game_id")

#Removing Incorrect Names
final_data$home_team[final_data$home_team == 'SD'] <- 'LAC'
final_data$home_team[final_data$home_team == 'JAC'] <- 'JAX'
final_data$home_team[final_data$home_team == 'STL'] <- 'LAR'
final_data$home_team[final_data$home_team == 'LA'] <- 'LAR'

final_data$away_team[final_data$away_team == 'SD'] <- 'LAC'
final_data$away_team[final_data$away_team == 'JAC'] <- 'JAX'
final_data$away_team[final_data$away_team == 'STL'] <- 'LAR'
final_data$away_team[final_data$away_team == 'LA'] <- 'LAR'

final_data$posteam[final_data$posteam == 'SD'] <- 'LAC'
final_data$posteam[final_data$posteam == 'JAC'] <- 'JAX'
final_data$posteam[final_data$posteam == 'STL'] <- 'LAR'
final_data$posteam[final_data$posteam == 'LA'] <- 'LAR'

# Create winning team and losing team columns
final_data <- final_data %>%
  mutate("winning_team" = case_when(
    home_score > away_score ~ home_team,
    home_score < away_score ~ away_team,
    home_score == away_score ~"tie")) %>%
  mutate("losing_team" = case_when(
    home_score < away_score ~ home_team,
    home_score > away_score ~ away_team,
    home_score == away_score ~"tie"))

# Get rid of two games with faulty data
final_data <-
  final_data %>%
  filter(play_type == 'run' | play_type == 'pass') %>%
  filter(game_id != '2013112401') %>%
  filter(game_id != '2013120101')

#Add Point Differential Column
final_data <- final_data %>%
  mutate(home_point_diff = 
           home_score - away_score)


#Filter Out Blowouts
final_data <- final_data %>%
  filter(home_point_diff > -33 &
           home_point_diff < 33)

#Filter Out Last Two Minutes of the Half
final_data <- final_data %>%
  filter(120 <= half_seconds_remaining)
