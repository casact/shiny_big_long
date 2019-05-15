/*
There is a fair amount of denormalization in the tables below. We are doing this as a computational expedient. This structure is 
for demonstration, education and entertainment only. 
*/

DROP TABLE IF EXISTS tbl_segment;

CREATE TABLE tbl_segment(
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
);

DROP TABLE IF EXISTS tbl_policyholder;

CREATE TABLE tbl_policyholder(
    id INTEGER NOT NULL PRIMARY KEY
  , segment_name TEXT NOT NULL
  , expected_cost REAL NOT NULL
  , compare REAL NOT NULL
  , frequency REAL NOT NULL
  , severity REAL NOT NULL
);

DROP TABLE IF EXISTS tbl_player;

CREATE TABLE tbl_player(
    name TEXT NOT NULL PRIMARY KEY
  , bot INTEGER NOT NULL
  , default_rate_change REAL NOT NULL
  , hist_include REAL
  , attenuation REAL
  , cap_increase REAL
  , cap_decrease REAL
);

DROP TABLE IF EXISTS tbl_policyholder_experience;

CREATE TABLE tbl_policyholder_experience(
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
);

DROP TABLE IF EXISTS tbl_player_experience;

CREATE TABLE tbl_player_experience (
    player_name TEXT NOT NULL
  , segment_name TEXT NOT NULL
  , round_num INTEGER NOT NULL
  , historical_cost REAL 
  , historical_premium REAL 
  , indicated_pure_premium REAL
  , indicated_change REAL
  , rate_change REAL
  , offer_premium REAL NOT NULL
  , PRIMARY KEY (player_name, segment_name, round_num)
);
