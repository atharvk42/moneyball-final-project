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
    filter(game_id != '2013120101')
  
  final_data_per_game <- final_data
  
  final_data_per_game <- final_data_per_game[!duplicated(final_data_per_game$game_id), ]
  
  final_data_per_game <-
    final_data_per_game %>%
    mutate(Team1 = case_when(
      winning_team == "tie" ~ home_team,
      winning_team != "tie" ~ ""
    )) %>%
    mutate(Team2 = case_when(
      winning_team == "tie" ~ away_team,
      winning_team != "tie" ~ ""
    ))
  
  final_data_per_game1 <- final_data_per_game
  final_data_per_game2 <- final_data_per_game
  
  final_data_per_game <-
    final_data_per_game %>%
    filter(winning_team != "tie") %>%
    count('winning_team')
  
  final_data_per_game1 <-
    final_data_per_game1 %>%
    count('Team1')
  
  final_data_per_game2 <-
    final_data_per_game2 %>%
    count('Team2')
  
  
  colnames(final_data_per_game) <- c("Team", "Wins")
  colnames(final_data_per_game1) <- c("Team1", "Ties")
  colnames(final_data_per_game2) <- c("Team2", "Ties")
  
  
  if (nrow(final_data_per_game1) > 1) {
    for (i in 1:32) {
      for (j in 2:(nrow(final_data_per_game1))) {
        if (final_data_per_game$Team[i] == final_data_per_game1$Team1[j]) {
          final_data_per_game$Wins[i] <- final_data_per_game$Wins[as.integer(i)] + 0.5
        }
      }
    }
    
    for (i in 1:32) {
      for (j in 2:(nrow(final_data_per_game2))) {
        if (final_data_per_game$Team[i] == final_data_per_game2$Team2[j]) {
          final_data_per_game$Wins[i] <- final_data_per_game$Wins[as.integer(i)] + 0.5
        }
      }
    }
  }
  
  
  game1_id <- final_data[1,"game_id"]
  
  # checking if it's 2017 to add Brown's data point since they didn't win a single game in 2017
  if(game1_id == "2017090700") {
    final_data_per_game <- 
      final_data_per_game %>%
      rbind(tibble(Team = "CLE", Wins = "0")) %>%
      arrange(Team)
  }
  
  
  final_data_per_game <-
    final_data_per_game %>%
    mutate(winning_percentage = (as.double(Wins)/16)) %>%
    select(Team, winning_percentage)
  
  
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

write_csv(wins09, "winning_percentages/wins09")
write_csv(wins10, "winning_percentages/wins10")
write_csv(wins11, "winning_percentages/wins11")
write_csv(wins12, "winning_percentages/wins12")
write_csv(wins13, "winning_percentages/wins13")
write_csv(wins14, "winning_percentages/wins14")
write_csv(wins15, "winning_percentages/wins15")
write_csv(wins16, "winning_percentages/wins16")
write_csv(wins17, "winning_percentages/wins17")
write_csv(wins18, "winning_percentages/wins18")
write_csv(wins19, "winning_percentages/wins19")


#calculate rushing/passing percentage per season per team
