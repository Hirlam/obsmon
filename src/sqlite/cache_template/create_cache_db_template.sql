PRAGMA foreign_keys = ON;

/* Some parameters to control how the other tables behave */
DROP TABLE IF EXISTS params;
CREATE TABLE params(
  allow_edit_timestamps INTEGER NOT NULL DEFAULT 0 CHECK(allow_edit_timestamps IN (0,1))
);
CREATE TRIGGER params_has_only_one_tuple
BEFORE INSERT ON params
WHEN (SELECT Count(*) FROM params) <> 0
BEGIN
  SELECT RAISE(FAIL, 'Table params can only contain one tuple.');
END;
INSERT INTO params(allow_edit_timestamps) VALUES(0);

/* General info about the experiment itself */
DROP TABLE IF EXISTS experiment;
CREATE TABLE experiment(
  basedir TEXT NOT NULL PRIMARY KEY,
  db TEXT NOT NULL CONSTRAINT valid_db CHECK(db IN ('ccma', 'ecma', 'ecma_sfc')),
  db_table TEXT NOT NULL CONSTRAINT valid_db_table CHECK(db_table IN ('obsmon', 'usage')),
  /* Keeping track of when the record was created */
  cdate_utc INTEGER NOT NULL DEFAULT 0,
  /* Keeping track of when the record was last modified */
  mdate_utc INTEGER NOT NULL DEFAULT 0
);
CREATE TRIGGER experiment_has_only_one_tuple
BEFORE INSERT ON experiment
WHEN (SELECT COUNT(*) FROM experiment) <> 0
BEGIN
  SELECT RAISE(FAIL, 'Table experiment can only contain one tuple.');
END;
/* Not allowing changes in experiment that would compromise consistency */
CREATE TRIGGER experiment_cannot_update_origin_db_info
BEFORE UPDATE OF db, db_table ON experiment
BEGIN
  SELECT RAISE(FAIL, "Cannot update selected column(s) in table experiment");
END;
/* Not allowing expt to be deleted */
CREATE TRIGGER experiment_cannot_be_deleted
BEFORE DELETE ON experiment
BEGIN
  SELECT RAISE(FAIL, "Cannot delete column(s) in table experiment");
END;
/* Making it difficult to manually alter cdate and mdate */
CREATE TRIGGER experiment_cannot_update_timestamps
BEFORE UPDATE OF cdate_utc, mdate_utc ON experiment
BEGIN
  SELECT CASE
  WHEN (SELECT allow_edit_timestamps FROM params ORDER BY allow_edit_timestamps ASC LIMIT 1) <> 1 THEN
    RAISE(FAIL, "Cannot update timestamps in table experiment")
  END;
END;
/* Triggers to set cdate/mdate of experiment upon insertion/updates */
CREATE TRIGGER experiment_set_cdate
AFTER INSERT ON experiment
BEGIN
  UPDATE params SET allow_edit_timestamps = 1;
  UPDATE experiment SET cdate_utc=STRFTIME('%Y%m%d%H%M%S', DATETIME('now')) WHERE ROWID=NEW.ROWID;
  UPDATE params set allow_edit_timestamps = 0;
END;
CREATE TRIGGER experiment_update_mdate
AFTER UPDATE OF basedir ON experiment
BEGIN
  UPDATE params SET allow_edit_timestamps = 1;
  UPDATE experiment SET mdate_utc=STRFTIME('%Y%m%d%H%M%S', DATETIME('now')) WHERE ROWID=NEW.ROWID;
  UPDATE params set allow_edit_timestamps = 0;
END;


/* Available dates in the experiment */
DROP TABLE IF EXISTS dates;
CREATE TABLE dates(
  /* 
    The constraint on date below enforces the format YYYYMMDDHH, and it also prevents the
    insertion of invalid dates such as, e.g., YYYY0230HH.
  */
  date INTEGER NOT NULL CONSTRAINT valid_date PRIMARY KEY CHECK(
    ( (date==strftime('%Y%m%d', substr(date,1,4)||'-'||substr(date,5,2)||'-'||substr(date,7,2), '+0 days')) IS NOT NULL ) AND
    ( (date==strftime('%Y%m%d', substr(date,1,4)||'-'||substr(date,5,2)||'-'||substr(date,7,2), '+0 days')) != 0 )
  )
);
CREATE TRIGGER cannot_update_dates
BEFORE UPDATE ON dates
BEGIN
  SELECT RAISE(FAIL, "Updates not allowed in table dates");
END;

/* Available DTGs (split into dates and cycles) */
DROP TABLE IF EXISTS cycles;
CREATE TABLE cycles(
  date INTEGER NOT NULL,
  cycle INTEGER NOT NULL CONSTRAINT valid_cycle CHECK((cycle>=0) AND (cycle<24)),
  /* Keeping track of when the record was created */
  cdate_utc INTEGER NOT NULL DEFAULT 0,
  FOREIGN KEY (date) REFERENCES dates(date) ON UPDATE CASCADE ON DELETE CASCADE
  PRIMARY KEY (date, cycle)
);
CREATE TRIGGER cannot_update_dtgs_on_table_cycles
BEFORE UPDATE OF date, cycle ON cycles
BEGIN
  SELECT RAISE(FAIL, "Updates to selected column(s) not allowed in table cycles");
END;
/* Making it difficult to manually alter cdate */
CREATE TRIGGER cycles_cannot_update_timestamps
BEFORE UPDATE OF cdate_utc ON cycles
BEGIN
  SELECT CASE
  WHEN (SELECT allow_edit_timestamps FROM params ORDER BY allow_edit_timestamps ASC LIMIT 1) <> 1 THEN
    RAISE(FAIL, "Cannot update timestamps in table cycles")
  END;
END;
/* Trigger to set cdate of cycles upon insertion */
CREATE TRIGGER cycles_set_cdate
AFTER INSERT ON cycles
BEGIN
  UPDATE params SET allow_edit_timestamps = 1;
  UPDATE cycles SET cdate_utc=STRFTIME('%Y%m%d%H%M%S', DATETIME('now')) WHERE ROWID=NEW.ROWID;
  UPDATE params set allow_edit_timestamps = 0;
END;

/* Relations regarding observations */
/* (i) upper-air observations: obnumber in [2, 3, 5, 6, 10] */
DROP TABLE IF EXISTS upper_air_obs;
CREATE TABLE upper_air_obs(
  date INTEGER NOT NULL,
  cycle INTEGER NOT NULL,
  /* Allowing statid to be NULL, as the "obsmon" table does not currently contain statid info */
  statid TEXT DEFAULT NULL,
  obname TEXT NOT NULL,
  varname TEXT NOT NULL,
  level INTEGER NOT NULL,
  FOREIGN KEY (date, cycle) REFERENCES cycles(date, cycle) ON UPDATE CASCADE ON DELETE CASCADE
  PRIMARY KEY (date, cycle, statid, obname, varname, level)
);
CREATE TRIGGER cannot_update_upper_air_obs
BEFORE UPDATE ON upper_air_obs
BEGIN
  SELECT RAISE(FAIL, "Updates not allowed in table upper_air_obs");
END;

/* (ii) surface observations: obnumber in [1, 4] */
DROP TABLE IF EXISTS surface_obs;
CREATE TABLE surface_obs(
  date INTEGER NOT NULL,
  cycle INTEGER NOT NULL,
  /* Allowing statid to be NULL, as the "obsmon" table does not currently contain statid info */
  statid TEXT DEFAULT NULL,
  obname TEXT NOT NULL,
  varname TEXT NOT NULL,
  FOREIGN KEY (date, cycle) REFERENCES cycles(date, cycle) ON UPDATE CASCADE ON DELETE CASCADE
  PRIMARY KEY (date, cycle, statid, obname, varname)
);
CREATE TRIGGER cannot_update_surface_obs
BEFORE UPDATE ON surface_obs
BEGIN
  SELECT RAISE(FAIL, "Updates not allowed in table surface_obs");
END;

/* (iii) Satellite observations: obnumber=7 */
DROP TABLE IF EXISTS satem_obs;
CREATE TABLE satem_obs(
  date INTEGER NOT NULL,
  cycle INTEGER NOT NULL,
  satname TEXT NOT NULL,
  /* obsmon backend writes sensor names in obname */
  obname TEXT NOT NULL,
  /* obsmon backend writes channel values in level*/
  level INTEGER NOT NULL,
  FOREIGN KEY (date, cycle) REFERENCES cycles(date, cycle) ON UPDATE CASCADE ON DELETE CASCADE
  PRIMARY KEY (date, cycle, satname, obname, level)
);
CREATE TRIGGER cannot_update_satem_obs
BEFORE UPDATE ON satem_obs
BEGIN
  SELECT RAISE(FAIL, "Updates not allowed in table satem_obs");
END;

/* (iv) Scatterometer observations: obnumber=9 */
DROP TABLE IF EXISTS scatt_obs;
CREATE TABLE scatt_obs(
  date INTEGER NOT NULL,
  cycle INTEGER NOT NULL,
  /* satname will be 'undefined' for obsmon v<=3.10.0 */
  satname TEXT NOT NULL,
  /* Allowing statid to be NULL, as the "obsmon" table does not currently contain statid info */
  statid TEXT DEFAULT NULL,
  varname TEXT NOT NULL,
  FOREIGN KEY (date, cycle) REFERENCES cycles(date, cycle) ON UPDATE CASCADE ON DELETE CASCADE
  PRIMARY KEY (date, cycle, statid, varname)
);
CREATE TRIGGER cannot_update_scatt_obs
BEFORE UPDATE ON scatt_obs
BEGIN
  SELECT RAISE(FAIL, "Updates not allowed in table scatt_obs");
END;

/* (v) Radar observations, obnumber=13 */
DROP TABLE IF EXISTS radar_obs;
CREATE TABLE radar_obs(
  date INTEGER NOT NULL,
  cycle INTEGER NOT NULL,
  /* Allowing statid to be NULL, as the "obsmon" table does not currently contain statid info */
  statid TEXT DEFAULT NULL,
  varname TEXT NOT NULL,
  level INTEGER NOT NULL,
  FOREIGN KEY (date, cycle) REFERENCES cycles(date, cycle) ON UPDATE CASCADE ON DELETE CASCADE
  PRIMARY KEY (date, cycle, statid, varname, level)
);
CREATE TRIGGER cannot_update_radar_obs
BEFORE UPDATE ON radar_obs
BEGIN
  SELECT RAISE(FAIL, "Updates not allowed in table radar_obs");
END;

/* (v) Lidar observations, obnumber=15 */
DROP TABLE IF EXISTS lidar_obs;
CREATE TABLE lidar_obs(
  date INTEGER NOT NULL,
  cycle INTEGER NOT NULL,
  obname TEXT NOT NULL,
  varname TEXT NOT NULL,
  level INTEGER NOT NULL,
  FOREIGN KEY (date, cycle) REFERENCES cycles(date, cycle) ON UPDATE CASCADE ON DELETE CASCADE
  PRIMARY KEY (date, cycle, obname, varname, level)
);
CREATE TRIGGER cannot_update_lidar_obs
BEFORE UPDATE ON lidar_obs
BEGIN
  SELECT RAISE(FAIL, "Updates not allowed in table lidar_obs");
END;

/* (vi) Finally, eventual observations that don't belong to any of the above types */
DROP TABLE IF EXISTS unknown_type_obs;
CREATE TABLE unknown_type_obs(
  date INTEGER NOT NULL,
  cycle INTEGER NOT NULL,
  /* Allowing statid to be NULL, as the "obsmon" table does not currently contain statid info */
  statid TEXT DEFAULT NULL,
  obname TEXT NOT NULL,
  varname TEXT DEFAULT NULL,
  satname TEXT DEFAULT NULL,
  level INTEGER DEFAULT NULL,
  FOREIGN KEY (date, cycle) REFERENCES cycles(date, cycle) ON UPDATE CASCADE ON DELETE CASCADE
  PRIMARY KEY (date, cycle, statid, obname, varname, satname, level)
);
CREATE TRIGGER cannot_update_unknown_type_obs
BEFORE UPDATE ON unknown_type_obs
BEGIN
  SELECT RAISE(FAIL, "Updates not allowed in table unknown_type_obs");
END;
