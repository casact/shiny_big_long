library(RSQLite)
library(DBI)
#'  There is a fair amount of denormalization in the tables below. We are doing this as a computational expedient. This structure is 
#' for demonstration, education and entertainment only. 
#' SQLite is such a pain in issuing multiple statements.
#' This is an adaptabion of the SQL provision script with each call wrapped in dbExecute. This is no longer flexible to the provision
#' paraemters. You just have to run several calls. Limited time here.

db_con<-dbConnect(SQLite(),'big_long.sqlite')
  
dbExecute(db_con,"DROP TABLE IF EXISTS tbl_segment")
dbExecute(db_con,"DROP TABLE IF EXISTS tbl_player")
dbExecute(db_con,"DROP TABLE IF EXISTS tbl_policyholder_experiece")
dbExecute(db_con,"DROP TABLE IF EXISTS tbl_player_experience")

dbExecute(db_con,"
  CREATE TABLE 
    tbl_segment(
    name TEXT NOT NULL PRIMARY KEY
    , compare_alpha REAL
    , compare_beta REAL
    , compare_trend REAL
    , freq_shape REAL
    , freq_scale REAL
    , freq_trend REAL
    , sev_shape REAL
    , sev_scale REAL
    , sev_trend REAL
    , expected_freq REAL
    , expected_severity REAL
    , expected_cost REAL
  )")

dbExecute(db_con,"
  CREATE TABLE 
    tbl_policyholder(
    id INTEGER NOT NULL PRIMARY KEY
    , segment_name TEXT NOT NULL
    , expected_cost REAL NOT NULL
    , compare REAL NOT NULL
    , frequency REAL NOT NULL
    , severity REAL NOT NULL
  )")

dbExecute(db_con,"
  CREATE TABLE 
    tbl_player(
    name TEXT NOT NULL PRIMARY KEY
    , bot INTEGER NOT NULL
    , default_rate_change REAL
    , hist_include REAL
    , attenuation REAL
    , cap_increase REAL
    , cap_decrease REAL
  )")

dbExecute(db_con,"  
  CREATE TABLE 
    tbl_policyholder_experience(
    round_num INTEGER NOT NULL
    , policyholder_id INTEGER NOT NULL
    , segment_name TEXT NOT NULL
    , expected_cost REAL NOT NULL
    , compare REAL NOT NULL
    , observed_claims REAL NOT NULL
    , observed_cost REAL NOT NULL
    , compared REAL NOT NULL
    , written_by REAL 
    , income REAL 
    , written_premium REAL
    , PRIMARY KEY (round_num, policyholder_id)
  )")

dbExecute(db_con,"
  CREATE TABLE 
    tbl_player_experience (
    player_name TEXT NOT NULL
    , segment_name TEXT NOT NULL
    , round_num INTEGER NOT NULL
    , prior_offer_premium REAL
    , historical_cost REAL 
    , historical_premium REAL 
    , indicated_pure_premium REAL
    , indicated_change REAL
    , default_rate_change REAL
    , rate_change REAL
    , offer_premium REAL NOT NULL
    , PRIMARY KEY (player_name, segment_name, round_num)
  )")
  
  