db_connect <- function(db_source = "big_long.sqlite") {
  dbConnect(SQLite(), db_source) 
}

db_create_segment <- function(db_con) {
  
}

db_create_player <- function(
    db_con
  , player_name
  , startup = runif(1, -.05, .05)
  , hist_include = sample(1:5, size = 1)
  , attenuation = 0.5
  , cap_increase = .05
  , cap_decrease = -.05) {
  
  tbl_player <- tibble(
      player_name
    , startup
    , hist_include
    , attenuation
    , cap_increase
    , cap_decrease
  ) 

  dbWriteTable(
      conn = db_con
    , name = 'tbl_player'
    , value = tbl_player
    , append = TRUE
  )
  
}

db_read_player <- function(db_con) {
  
}

db_update_rounds <- function(db_con) {
  
}