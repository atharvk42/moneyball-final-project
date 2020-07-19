  
#merge pbp and game data
merge_pbp_game_data <- function(pbp_all, game_data_all) {
  pbp_all <-
    pbp_all %>%
    select('game_id', 'home_team', 'away_team', 'posteam', 'game_date', 'half_seconds_remaining', 'down', 'ydstogo', 'ydsnet', 'yards_gained', 'play_type', 'score_differential')
  
  game_data_all <-
    game_data_all %>%
    select('game_id', 'home_score', 'away_score')
  
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
  
  return(final_data)
}

# calculate rushing and passing percentages
percentage_yards_rushing_passing <- function(pbp, game_data) {
  
  final_data <- merge_pbp_game_data(pbp, game_data)
  final_data <- merge_pbp_game_data(pbp18, game_data18)
  final_data <-
    final_data %>%
    filter(play_type == 'run' | play_type == 'pass') %>%
    filter(game_id != '2013112401') %>%
    filter(game_id != '2013120101')
  
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
    filter("winning_team" != "tie")
  
  rushing <- final_data
  rushing <-
    rushing %>%
    filter(play_type == 'run' & winning_team == posteam) 
  rushing_yards_per_game <- aggregate(x = rushing$yards_gained,               
                                      by = list(rushing$home_team),        
                                      FUN = sum)
  colnames(rushing_yards_per_game) <- c("home_team", "rushing_yards_winning_team")
  
  
  passing <- final_data
  passing <-
    passing %>%
    filter(play_type == 'pass' & winning_team == posteam) 
  passing_yards_per_game <- aggregate(x = passing$yards_gained,               
                                      by = list(passing$home_team),        
                                      FUN = sum)
  colnames(passing_yards_per_game) <- c("home_team", "passing_yards_winning_team")
  passing_yards_per_game <- passing_yards_per_game[!duplicated(passing_yards_per_game$home_team), ]
  
  
  final_data_per_game <- final_data
  
  final_data_per_game <- final_data_per_game[!duplicated(final_data_per_game$game_id), ]
  final_data_per_game <- final_data_per_game[!duplicated(final_data_per_game$home_team), ]
  
  final_data_per_game <-
    final_data_per_game %>%
    select('home_team')
  
  final_data_per_game <-
    final_data_per_game %>%
    inner_join(rushing_yards_per_game, by = 'home_team') %>%
    inner_join(passing_yards_per_game, by = 'home_team')
  
  final_data_per_game <-
    final_data_per_game %>%
    mutate(percentage_yards_rushing = rushing_yards_winning_team/(rushing_yards_winning_team+passing_yards_winning_team)) %>%
    mutate(percentage_yards_passing = passing_yards_winning_team/(rushing_yards_winning_team+passing_yards_winning_team)) %>%
    select(home_team, percentage_yards_rushing, percentage_yards_passing)
  
  return(final_data_per_game)
}

# use function for every year
percentage_yards_rushing_passing09 <- percentage_yards_rushing_passing(pbp09, game_data09)
percentage_yards_rushing_passing10 <- percentage_yards_rushing_passing(pbp10, game_data10)
percentage_yards_rushing_passing11 <- percentage_yards_rushing_passing(pbp11, game_data11)
percentage_yards_rushing_passing12 <- percentage_yards_rushing_passing(pbp12, game_data12)
percentage_yards_rushing_passing13 <- percentage_yards_rushing_passing(pbp13, game_data13)
percentage_yards_rushing_passing14 <- percentage_yards_rushing_passing(pbp14, game_data14)
percentage_yards_rushing_passing15 <- percentage_yards_rushing_passing(pbp15, game_data15)
percentage_yards_rushing_passing16 <- percentage_yards_rushing_passing(pbp16, game_data16)
percentage_yards_rushing_passing17 <- percentage_yards_rushing_passing(pbp17, game_data17)
percentage_yards_rushing_passing18 <- percentage_yards_rushing_passing(pbp18, game_data18)
percentage_yards_rushing_passing19 <- percentage_yards_rushing_passing(pbp19, game_data19)
