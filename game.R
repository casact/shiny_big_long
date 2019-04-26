#' gm_segments_create
#' 
#' @param num_segments Integer indicating the number of segments to be created
#' @param lst_parms Not used yet
#' 
#' @return Data frame
#' 
#' @details This function will create a data frame which denotes business segments.
#' 
#' The `compare` term indicates the probability that a customer will compare prices in the market. If they do compare, they will purchase a policy only
#' if 1) it is cheaper, and 2) there is a randomized element.
#' 
#' 
gm_segments_create <- function(num_segments, lst_parms) {
  
  lst_parms <- NULL
  
  tibble(
        segment = LETTERS[seq_len(num_segments)]
      , compare_alpha = sample(seq_len(10), size = num_segments, replace = TRUE)
      , compare_beta = compare_alpha * runif(num_segments, 1.5, 3)
      , freq_shape = runif(num_segments, 0.5, 1.5)
      , freq_scale = 1:10 %>% sample(num_segments, replace = TRUE)
      , sev_shape = runif(num_segments, 0.8, 1.2)
      , sev_scale = 10e3
    ) %>% 
    mutate(
        expected_compare = compare_alpha / compare_beta
      , expected_freq = freq_shape * freq_scale
      , expected_sev = sev_shape * sev_scale
      , expected_cost = expected_freq * expected_sev
    )

}

#' gm_policyholders_create
#' 
#' @param tbl_segment Data frame with segment information
#' @param num_policyholders Integer indicating the number of policyholders in the universe
#' 
#' @return Data frame
#' 
gm_policyholders_create <- function(tbl_segment, num_policyholders) {
  
  num_segments <- nrow(tbl_segment)
  
  tbl_segment %>% 
    mutate(
      policyholders = rmultinom(1, size = num_policyholders, prob = rep(1 / num_segments, num_segments))
    ) %>% 
    mutate(
        compare = pmap(list(n = policyholders, shape1 = compare_alpha, shape2 = compare_beta), rbeta)
      , frequency = pmap(list(n = policyholders, shape = freq_shape, scale = freq_scale), rgamma)
      , severity = pmap(list(n = policyholders, shape = sev_shape, scale = sev_scale), rgamma)
    ) %>% 
    select(segment, expected_cost, compare, frequency, severity) %>% 
    unnest() %>% 
    mutate(
        id = seq_len(nrow(.))
      , expected_cost = frequency * severity
    ) %>% 
    select(
      id, segment, everything()
    )
  
}

gm_rounds_create <- function(tbl_policyholder, tbl_player_experience, num_rounds) {

  tbl_round <- tbl_policyholder %>% 
    rename(policyholder_id = id) %>% 
    mutate(
        round = map(num_rounds, seq_len)
    ) %>%
    unnest() %>% 
    mutate(
        observed_claims = rpois(nrow(.), frequency)
      , observed_dollars = pmap(list(n = observed_claims, rate = 1 / severity), rexp)
      , observed_cost = map_dbl(observed_dollars, sum)
      , premium = NA_real_
      , written_by = NA_integer_
      , income = NA_real_
    ) %>% 
    select(
      policyholder_id, segment, written_by, round, premium, observed_cost, income
    )
  
  tbl_round %>%
    mutate(
      written_by = ifelse(
          round == 1
        , base::sample(tbl_player$id, size = nrow(.), replace = TRUE)
        , NA_integer_
      )
    ) %>% 
    left_join(
      tbl_player_experience %>% 
        select(player_id, segment, round, renewal_premium = premium)
      , by = c('written_by' = 'player_id', 'segment', 'round')
    ) %>% 
    mutate(
      premium = renewal_premium
      , income = premium - observed_cost
    ) %>% 
    select(-renewal_premium)

}

which_round <- function(tbl_round) {
  
  # Check to see if any of the rounds have been written
  
  mojo <- !is.na(tbl_round$written_by)
  
  if (sum(mojo)) {
    tbl_round$round[mojo] %>% max()
  } else {
    1
  }
  
  
}

#' gm_rounds_update
#' 
#' @description 
#' 
gm_rounds_update <- function(tbl_round, tbl_player_experience, max_write_pct = 0.25) {
  
  which_round <- which_round(tbl_round) + 1
  
  if (which_round == 1) {
    warning("Something weird is happening")
  } else {
  
    tbl_player_experience <- tbl_player_experience %>% 
      filter(round == which_round) %>% 
      group_by(segment) %>% 
      arrange(premium) %>% 
      slice(1)
    
    for (i_row in seq_len(nrow(tbl_round))) {
    
      if ()
      tbl_round$written_by[i_row] <- tbl_player_experience$player_id[tbl_player_experience$segment == tbl_round$segment[i_row]][1]
      
    }
  
  }
  
  tbl_round <- tbl_round %>% 
    left_join(
      tbl_player_experience %>% 
        select(player_id, segment, round, renewal_premium = premium)
      , by = c('written_by' = 'player_id', 'segment', 'round')
    ) %>% 
    mutate(
        premium = renewal_premium
      , income = premium - observed_cost
    ) %>% 
    select(-renewal_premium)
  
}

gm_dummy_players <- function(num_players = 10) {
  tibble(
      id = seq_len(num_players)
    , name = randomNames::randomNames(
          num_players
        , which.names = 'first'
      )
    , startup = runif(num_players, -.05, .05)
    , hist_include = 1:5 %>% sample(num_players, replace = TRUE)
    , attenuation = 0.5
    , cap_increase = .05
    , cap_decrease = -.05
  )
}

#' gm_player_experience_create
#' 
#' @description 
#' 
gm_player_experience_create <- function(tbl_player, tbl_segment, num_rounds = 10) {
  
  tbl_player %>% 
    rename(player_id = id) %>% 
    crossing(
      tbl_segment %>% select(segment, premium = expected_cost)
    ) %>% 
    mutate(
      premium = premium * (1 + startup)
    ) %>% 
    select(-startup) %>% 
    crossing(
      round = seq_len(num_rounds) 
    ) %>% 
    mutate(
        premium = ifelse(
            round == 1
          , premium
          , NA_real_
        )
      , attenuation = ifelse(
            round == num_rounds
          , NA_real_
          , attenuation
        )
      , hist_include = ifelse(
            round == num_rounds
          , NA_real_
          , hist_include
        )
      , historical_cost = NA_real_
      , historical_premium = NA_real_
      , indicated_change = NA_real_
      , rate_change = NA_real_
    )
  
}

#' gm_player_experience_update
#' 
#' @details I'm faking this for the moment and ignoring the player preferences about how much history to include.
#' 
gm_player_experience_update <- function(tbl_player_experience, tbl_round){
  
  round_to_update <- which_round(tbl_round)
  
  tbl_round_summary <- tbl_round %>% 
    filter(round <= round_to_update) %>% # Need to filter because we pre-fill the observed_cost
    group_by(written_by, segment) %>% 
    summarise(
          observed_cost_2 = sum(observed_cost)
        , premium_2 = sum(premium)
    ) %>%
    ungroup() %>% 
    mutate(
        indicated_change_2 = observed_cost_2 / premium_2 - 1
      , round = round_to_update
    )
  
  tbl_player_experience %>% 
    left_join(tbl_round_summary, by = c('player_id' = 'written_by', 'segment', 'round')) %>% 
    mutate(
        historical_cost = observed_cost_2
      , historical_premium = premium_2
      , indicated_change = indicated_change_2
      , rate_change = indicated_change_2
      , rate_change = (1 - attenuation) * rate_change
      , rate_change = pmax(rate_change, cap_decrease)
      , rate_change = pmin(rate_change, cap_increase)
      , premium = ifelse(
            round == round_to_update + 1
          , dplyr::lag(premium) * (1 + dplyr::lag(rate_change))
          , premium
      )
    ) %>% 
    select(-contains('_2'))
  
}
