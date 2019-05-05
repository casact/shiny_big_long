/*
There is a fair amount of denormalization in the tables below. We are doing this as a computational expedient. This structure is 
for demonstration, education and entertainment only. 
*/

DROP TABLE IF EXISTS tbl_segment;

CREATE TABLE tbl_segment(
    segment TEXT NOT NULL PRIMARY KEY
  , compare_alpha REAL
  , compare_beta REAL
  , compare_trend REAL
  , freq_shape REAL
  , freq_scale REAL
  , freq_trend REAL
  , sev_shape REAL
  , sev_scale REAL
  , sev_trend REAL
);

DROP TABLE IF EXISTS tbl_policyholder;

CREATE TABLE tbl_policyholder(
    id INTEGER NOT NULL PRIMARY KEY
  , segment TEXT NOT NULL
  , expected_cost REAL NOT NULL
  , compare REAL NOT NULL
  , frequency REAL NOT NULL
  , severity REAL NOT NULL
);

DROP TABLE IF EXISTS tbl_player;

CREATE TABLE tbl_player(
    name TEXT NOT NULL PRIMARY KEY
  , startup REAL NOT NULL
  , bot INTEGER NOT NULL
);

DROP TABLE IF EXISTS tbl_player_experience;

CREATE TABLE tbl_player_experience(
  player_name TEXT NOT NULL 
);

DROP TABLE IF EXISTS tbl_round;

CREATE TABLE tbl_round(
    round_num INTEGER NOT NULL
  , policyholder_id INTEGER NOT NULL
  , segment TEXT NOT NULL
  , expected_cost REAL NOT NULL
  , compare REAL NOT NULL
  , observed_claims REAL NOT NULL
  , observed_dollars REAL NOT NULL
  , observed_cost REAL NOT NULL
  , compared REAL NOT NULL
  , written_by REAL NOT NULL
  , income REAL NOT NULL
  , written_premium REAL NOT NULL
);

DROP TABLE IF EXISTS tbl_player_experience;

CREATE TABLE tbl_player_experience (
    player_id
  , segment
  , startup
  , attenuation
  , hist_include
  , historical_cost
  , historical_premium
  , indicated_change
  , rate_change
  , offer_premium
)