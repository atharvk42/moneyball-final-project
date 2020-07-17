#devtools::install_github(repo = "maksimhorowitz/nflscrapR")
# or the following (these are the exact same packages):
#devtools::install_github(repo = "ryurko/nflscrapR")

remove(list = ls())
setwd("~/Moneyball")


pbp19 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2019.csv"))
pbp18 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))
pbp17 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2017.csv"))
pbp16 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2016.csv"))
pbp15 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2015.csv"))
pbp14 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2014.csv"))
pbp13 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2013.csv"))
pbp12 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2012.csv"))
pbp11 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2011.csv"))
pbp10 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2010.csv"))
pbp09 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2009.csv"))

game_data19 <-read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/games_data/regular_season/reg_games_2019.csv"))
game_data18 <-read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/games_data/regular_season/reg_games_2018.csv"))
game_data17 <-read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/games_data/regular_season/reg_games_2017.csv"))
game_data16 <-read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/games_data/regular_season/reg_games_2016.csv"))
game_data15 <-read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/games_data/regular_season/reg_games_2015.csv"))
game_data14 <-read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/games_data/regular_season/reg_games_2014.csv"))
game_data13 <-read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/games_data/regular_season/reg_games_2013.csv"))
game_data12 <-read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/games_data/regular_season/reg_games_2012.csv"))
game_data11 <-read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/games_data/regular_season/reg_games_2011.csv"))
game_data10 <-read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/games_data/regular_season/reg_games_2010.csv"))
game_data09 <-read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/games_data/regular_season/reg_games_2009.csv"))

