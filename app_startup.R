library(tidyverse)
library(DBI)
library(RSQLite)

source('package/game.R')

set.seed(1234)
tbl_segment <- tibble(
    name = LETTERS[1:4]
  , compare_alpha = c(1.01, 1.01, 15, 15)
  , compare_beta = c(15, 15, 1.01, 1.01)
  , compare_trend = c(-.05, -.05, .05, .05)
  , freq_shape = 2
  , freq_scale = 1
  , freq_trend = c(-.2, .4, -.2, .4)
  , sev_shape = 2
  , sev_scale = 5e3
  , sev_trend = 0.02
) %>% 
  mutate(
    expected_freq = freq_shape * freq_scale
    , expected_severity = sev_shape * sev_scale
    , expected_cost = expected_freq * expected_severity
  )
