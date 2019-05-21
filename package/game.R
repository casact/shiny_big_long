#' gm_segments_create
#' 
#' @param num_segments Integer indicating the number of segments to be created
#' 
#' @return Data frame
#' 
#' @details This function will create a data frame which denotes business segments.
#' 
#' The `compare` term indicates the probability that a customer will compare prices in the market. If they do compare, they will purchase a policy only
#' if 1) it is cheaper, and 2) there is a randomized element.
#' 
#' 
gm_segments_create <- function(num_segments) {
  
  tibble(
        segment = LETTERS[seq_len(num_segments)]
      , compare_alpha = sample(seq_len(10), size = num_segments, replace = TRUE)
      , compare_beta = compare_alpha * runif(num_segments, 1.5, 3)
      , compare_trend = 0
      , freq_shape = runif(num_segments, 0.5, 1.5)
      , freq_scale = 1:10 %>% sample(num_segments, replace = TRUE)
      , freq_trend = 0
      , sev_shape = runif(num_segments, 0.8, 1.2)
      , sev_scale = 10e3
      , sev_trend = 0
    ) %>% 
    mutate(
      expected_compare = compare_alpha / (compare_alpha + compare_beta)
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
    ) %>% 
    select(segment_name = name, expected_cost, compare, frequency, sev_shape, sev_scale, compare_trend, freq_trend, sev_trend) %>% 
    unnest() %>% 
    mutate(
        id = seq_len(nrow(.))
      , expected_cost = frequency * sev_shape * sev_scale
    ) %>% 
    select(
      id, segment_name, everything()
    )
  
}

which_index <- function(x, search_val) {
  which(x == search_val)
}

#' gm_updated_bound_market
#' 
#' 
gm_updated_bound_market <- function(tbl_policyholder_experience, tbl_player_experience, which_round) {
  
  if (which_round == 1) {
    tbl_updated_pol_exp <- tbl_policyholder_experience %>% 
      filter(round_num == 1)
  } else {
    tbl_updated_pol_exp <- tbl_policyholder_experience %>% 
      filter(round_num == which_round - 1) %>%
      mutate(
        round_num = round_num + 1
      )
  }
  
  tbl_offer_premiums <- tbl_player_experience %>% 
    gm_get_offer_premiums(which_round)
  
  tbl_offer_premiums <- tbl_updated_pol_exp %>% 
    mutate(
      prior_market = current_market
      , prior_premium = current_premium) %>% 
    inner_join(tbl_offer_premiums, by = 'segment_name') %>% 
    mutate(
      ref_point = ifelse(
        round_num == 1
        , map_dbl(market_premiums, median)
        , prior_premium
      )
      , offer_weights = pmap(
        list(market_premiums, ref_point = ref_point)
        , compare_weights
      )
      , bestish_market = pmap_chr(
        list(markets, offer_weights, replace = FALSE, size = 1)
        , sample
      )
      , bestish_index = map2_int(markets, bestish_market, which_index)
      , bestish_premium = map2_dbl(
        market_premiums
        , bestish_index
        , pluck
      )      
      , current_market = ifelse(
        compared == 1
        , bestish_market
        , current_market
      )
      , current_premium = ifelse(
        compared == 1
        , bestish_premium
        , current_premium
      )
      , income = current_premium - observed_cost
    ) %>% 
    select(-market_premiums, -markets, -ref_point, -offer_weights, -bestish_market, -bestish_index, -bestish_premium)
  
}

#' gm_policyholder_experience_create
#' 
#' 
gm_policyholder_experience_create <- function(tbl_policyholder, num_rounds) {

  tbl_policyholder %>% 
    rename(policyholder_id = id) %>% 
    mutate(
      round_num = map(num_rounds, seq_len)
    ) %>%
    unnest() %>% 
    group_by(policyholder_id) %>%
    arrange(round_num, .by_group = TRUE) %>% 
    mutate(
      base_sev_scale = head(sev_scale, 1)
      , base_frequency = head(frequency, 1)
      , base_compare = head(compare, 1)
      , sev_scale = base_sev_scale * (1 + sev_trend) ^ (round_num - 1)
      , frequency = base_frequency + cumsum(freq_trend)
      , compare = base_compare + cumsum(compare_trend)
    ) %>% 
    ungroup() %>% 
    mutate(
      compare = pmax(compare, .01)
      , compare = pmin(compare, .98)
      , frequency = pmax(frequency, .02)
    ) %>% 
    mutate(
        observed_claims = rpois(nrow(.), frequency)
      , observed_dollars = pmap(list(n = observed_claims, shape = sev_shape, scale = sev_scale), rgamma)
      , observed_cost = map_dbl(observed_dollars, sum)
      , compared = ifelse(
            round_num == 1
          , 1
          , rbinom(nrow(.), 1, compare)
        )
    ) %>% 
    select(
      -frequency, -sev_scale, -sev_shape, -observed_dollars, -compare_trend, freq_trend, sev_trend
    ) %>% 
    mutate(
        prior_market = NA_character_
        , prior_premium = NA_real_
        , current_market = NA_character_
        , current_premium = NA_real_
        , income = NA_real_
    ) %>% 
    arrange(policyholder_id, round_num)
}

#' gm_policyholder_experience_update
#' 
#' @description 
#' 
gm_policyholder_experience_update <- function(tbl_policyholder_experience, tbl_player_experience) {
  
  mojo <- !is.na(tbl_policyholder_experience$current_market)
  
  if (sum(mojo)) {
    which_round <- tbl_policyholder_experience$round_num[mojo] %>% max() + 1
  } else {
    which_round <- 1
  }

  tbl_update <- tbl_policyholder_experience %>% 
    gm_updated_bound_market(tbl_player_experience, which_round)

  tbl_policyholder_experience <- tbl_policyholder_experience %>% 
    filter(
      round_num != which_round
    ) %>% 
    bind_rows(tbl_update)
  
  tbl_policyholder_experience
  
}

#' compare_weights
#'
compare_weights <- function(points, ref_point, balk = FALSE, ballast = .01, balk_high = TRUE) {
  
  the_weights <- -(points - ref_point)
  height <- min(the_weights)
  the_weights <- the_weights - height + ballast
  
  if (balk) {
    the_weights[points > ref_point] <- 0
  }

  the_weights
}

#' gm_dummy_players
#' 
#' 
gm_dummy_players <- function(num_players = 10) {
  tibble(
      name = randomNames::randomNames(
          num_players
        , which.names = 'first'
      )
    , bot = TRUE
    , default_rate_change = runif(num_players, -.05, .05)
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
gm_player_experience_create <- function(tbl_player, tbl_segment) {
  
  tbl_player %>% 
    select(player_name = name, default_rate_change) %>% 
    crossing(
      tbl_segment %>% select(segment_name = name, expected_cost)
    ) %>% 
    mutate(
      round_num = 1
      , prior_offer_premium = expected_cost 
      , historical_cost = NA_real_
      , historical_premium = NA_real_
      , indicated_change = NA_real_
      , rate_change = default_rate_change
      , offer_premium = prior_offer_premium * (1 + rate_change) 
    ) %>% 
    select(-expected_cost) %>% 
    select(player_name, segment_name, round_num, everything()) 
  
}

#' gm_player_experience_update
#' 
#' @details I'm faking this for the moment and ignoring the player preferences about how much history to include.
#' 
gm_player_experience_update <- function(tbl_player_experience, tbl_policyholder_experience){
  
  round_to_update <- tbl_player_experience$round_num %>% max()
  
  tbl_round_summary <- tbl_policyholder_experience %>% 
    filter(round_num <= round_to_update) %>% # Need to filter because we pre-fill the observed_cost
    group_by(current_market, segment_name) %>% 
    summarise(
        observed_cost_2 = sum(observed_cost, na.rm = TRUE)
      , premium_2 = sum(current_premium, na.rm = TRUE)
      , num_policyholders_2 = n()
    ) %>%
    ungroup()
  
  tbl_player_experience_update <- tbl_player_experience %>%
    filter(round_num == round_to_update) %>% 
    left_join(tbl_round_summary, by = c('player_name' = 'current_market', 'segment_name')) %>% 
    mutate(
        historical_cost = observed_cost_2
      , historical_premium = premium_2
      , indicated_pure_premium = historical_cost / num_policyholders_2
      , indicated_change = historical_cost / historical_premium - 1
      , round_num = round_num + 1
      , prior_offer_premium = offer_premium
      , rate_change = NA_real_
      , offer_premium = NA_real_
    ) %>% 
    select(-contains('_2'))

  tbl_player_experience <- tbl_player_experience %>% 
    filter(
      round_num != (round_to_update + 1)
    ) %>% 
    bind_rows(tbl_player_experience_update)
  
}

gm_player_experience_update_bots <- function(tbl_player_experience, tbl_player) {

  original_cols <- colnames(tbl_player_experience)

  tbl_player_experience <- tbl_player_experience %>%
    left_join(tbl_player %>% select(-default_rate_change), by = c(player_name = 'name'))

  tbl_to_update <- tbl_player_experience %>%
    filter(bot, is.na(offer_premium)) %>%
    mutate(
      rate_change = indicated_change
      , rate_change = (1 - attenuation) * rate_change
      , rate_change = pmax(rate_change, cap_decrease)
      , rate_change = pmin(rate_change, cap_increase)
      , rate_change = coalesce(rate_change, 0)
    )

  tbl_all_good <- tbl_player_experience  %>%
    filter(
      (!bot | !is.na(offer_premium))
    )

  tbl_player_experience <- bind_rows(
    tbl_to_update
    , tbl_all_good
  ) %>% 
    mutate(
      rate_change = coalesce(rate_change, 0)
      , offer_premium = prior_offer_premium * (1 + rate_change)
    )

  tbl_player_experience <- tbl_player_experience[, original_cols]
}

#' gm_get_offer_premium
#' 
gm_get_offer_premiums <- function(tbl_player_experience, which_round) {
  
  # This table will take the player experience for the round under consideration and restructure it so that
  # we have one row for each segment. The market premiums and player names will have 
  # vectors for each segment.
  
  tbl_player_experience %>% 
    filter(
      round_num == which_round
      , !is.na(offer_premium)
      ) %>% 
    select(segment_name, player_name, offer_premium) %>% 
    nest(player_name, offer_premium, .key = 'mojo') %>%
    mutate(
      market_premiums = map(mojo, 'offer_premium') %>% map(unlist)
      , markets = map(mojo, 'player_name')
    ) %>% 
    select(-mojo)
}
