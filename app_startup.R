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

num_policyholders <- 50e3
num_policyholders <- 50
tbl_policyholder <- tbl_segment %>% 
  gm_policyholders_create(num_policyholders)

tbl_round <- tbl_policyholder %>% 
  gm_rounds_create(10)

tbl_player <- gm_dummy_players(1)  %>% 
  mutate(name = 'Mona Polly')

tbl_player_experience <- gm_player_experience_create(tbl_player, tbl_segment)

tbl_round <- tbl_round %>% 
  gm_rounds_initialize(tbl_player_experience)

db_con <- dbConnect(SQLite(), 'big_long.sqlite')

dbExecute(db_con, "DELETE FROM tbl_segment")
dbExecute(db_con, "DELETE FROM tbl_policyholder")
dbExecute(db_con, "DELETE FROM tbl_round")
dbExecute(db_con, "DELETE FROM tbl_player")
dbExecute(db_con, "DELETE FROM tbl_player_experience")

dbWriteTable(db_con, 'tbl_segment', tbl_segment, append = TRUE)
dbWriteTable(db_con, 'tbl_policyholder', tbl_policyholder, append = TRUE)
dbWriteTable(db_con, 'tbl_round', tbl_round, append = TRUE)
dbWriteTable(db_con, 'tbl_player', tbl_player, append = TRUE)
dbWriteTable(db_con, 'tbl_player_experience', tbl_player_experience, append = TRUE)

dbDisconnect(db_con)

mojo <- gm_rounds_update(tbl_round, tbl_player_experience)

tbl_player_xp_update <- gm_player_experience_update(tbl_player_experience, tbl_round)