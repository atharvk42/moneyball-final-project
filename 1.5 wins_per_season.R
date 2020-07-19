#Function for team wins per season
library(plyr)
team_wins_per_season <- function(pbp_year, game_data_year) {
  pbp_year <-
    pbp_year %>%
    select('game_id', 'home_team', 'away_team', 'posteam', 'game_date', 'half_seconds_remaining', 'down', 'ydstogo', 'ydsnet', 'yards_gained', 'play_type', 'td_team')
  
  game_data_year <-
    game_data_year %>%
    select('game_id', 'home_score', 'away_score')
  
  final_data  <-
    pbp_year %>%
    inner_join(game_data_year, by = "game_id")
  
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
  
  
  final_data <- final_data %>%
    mutate("winning_team" = case_when(
      home_score > away_score ~ home_team,
      home_score < away_score ~ away_team,
      home_score == away_score ~"tie")) %>%
    mutate("losing_team" = case_when(
      home_score < away_score ~ home_team,
      home_score > away_score ~ away_team,
      home_score == away_score ~"tie"))
  
  final_data <-
    final_data %>%
    filter(play_type == 'run' | play_type == 'pass') %>%
    filter(game_id != '2013112401') %>%
    filter(game_id != '2013120101') %>%
    filter(home_score != away_score)
  
  final_data_per_game <- final_data
  
  final_data_per_game <- final_data_per_game[!duplicated(final_data_per_game$game_id), ]
  
  final_data_per_game <- 
    final_data_per_game %>%
    count('winning_team')
  
  colnames(final_data_per_game) <- c("Team", "Wins")
 game1_id <- final_data[1,"game_id"]
 
 # checking if it's 2017 to add Brown's data point since they didn't win a single game in 2017
 if(game1_id == "2017090700") {
   final_data_per_game <- 
     final_data_per_game %>%
     rbind(tibble(Team = "CLE", Wins = "0")) %>%
     arrange(Team)

 }
  
  return(final_data_per_game)
}

wins09 <- team_wins_per_season(pbp09, game_data09)
wins10 <- team_wins_per_season(pbp10, game_data10)
wins11 <- team_wins_per_season(pbp11, game_data11)
wins12 <- team_wins_per_season(pbp12, game_data12)
wins13 <- team_wins_per_season(pbp13, game_data13)
wins14 <- team_wins_per_season(pbp14, game_data14)
wins15 <- team_wins_per_season(pbp15, game_data15)
wins16 <- team_wins_per_season(pbp16, game_data16)
wins17 <- team_wins_per_season(pbp17, game_data17)
wins18 <- team_wins_per_season(pbp18, game_data18)
wins19 <- team_wins_per_season(pbp19, game_data19)


