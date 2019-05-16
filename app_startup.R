library(tidyverse)
library(DBI)
library(RSQLite)

source('package/game.R')

set.seed(1234)
tbl_segment <- tibble(
    name = LETTERS[1:4]
  , compare_alpha = c(1.01, 1.01, 15, 15)
  , compare_beta = c(15, 15, 1.01, 1.01)
  , compare_trend = c(0, 1, 0, -1)
  , freq_shape = 2
  , freq_scale = 1
  , freq_trend = c(-.01, .01, -.01, .01)
  , sev_shape = 2
  , sev_scale = 5e3
  , sev_trend = 0.02
) %>% 
  mutate(
    expected_freq = freq_shape * freq_scale
    , expected_severity = sev_shape * sev_scale
    , expected_cost = expected_freq * expected_severity
  )

tbl_bot_player <- gm_dummy_players(1)  %>%
  mutate(name = 'Mona Pauley')

tbl_player_experience <- gm_player_experience_create(tbl_bot_player, tbl_segment)

db_con <- dbConnect(SQLite(), 'big_long.sqlite')

dbExecute(db_con, "DELETE FROM tbl_player")
dbExecute(db_con, "DELETE FROM tbl_player_experience")

dbWriteTable(db_con, 'tbl_player', tbl_bot_player, append = TRUE)
dbWriteTable(db_con, 'tbl_player_experience', tbl_player_experience, append = TRUE)

dbDisconnect(db_con)
