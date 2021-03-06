# Database ----
## entity ----
"
CREATE TABLE `SPECIAL-EPD`.`entity` (
  `ID_SITE` INT UNSIGNED NOT NULL,
  `ID_ENTITY` INT UNSIGNED NOT NULL,
  `site_name` VARCHAR(255) NULL,
  `entity_name` VARCHAR(255) NULL,
  `latitude` DOUBLE NULL,
  `longitude` DOUBLE NULL,
  `elevation` DOUBLE NULL,
  `site_type` VARCHAR(255) NULL,
  `source` VARCHAR(255) NULL,
  `publication` TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `doi` VARCHAR(255) NULL,
  PRIMARY KEY (`ID_ENTITY`, `ID_SITE`),
  UNIQUE INDEX `ID_ENTITY_UNIQUE` (`ID_ENTITY` ASC) VISIBLE
);
"

## external_link ----
"
CREATE TABLE `SPECIAL-EPD`.`external_link` (
  `ID_SITE` INT UNSIGNED NOT NULL,
  `ID_ENTITY` INT UNSIGNED NOT NULL,
  `external_ID_SITE` INT UNSIGNED DEFAULT NULL,
  `external_ID_ENTITY` INT UNSIGNED NOT NULL,
  `external_site_name` VARCHAR(255) DEFAULT NULL,
  `external_entity_name` VARCHAR(255) DEFAULT NULL,
  `external_source VARCHAR(255) NOT NULL`,
  PRIMARY KEY (`ID_ENTITY`, `external_ID_ENTITY`, `external_source`),
  CONSTRAINT `ID_SITE`
    FOREIGN KEY (`ID_SITE`)
    REFERENCES `SPECIAL-EPD`.`entity` (`ID_SITE`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `ID_ENTITY`
    FOREIGN KEY (`ID_ENTITY`)
    REFERENCES `SPECIAL-EPD`.`entity` (`ID_ENTITY`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);
"

## neotoma ----
"
CREATE TABLE `SPECIAL-EPD`.`neotoma` (
  `ID_SITE` INT UNSIGNED NOT NULL,
  `ID_ENTITY` INT UNSIGNED NOT NULL,
  `neotoma_ID_SITE` INT UNSIGNED NOT NULL,
  `neotoma_ID_ENTITY` INT UNSIGNED NOT NULL,
  `neotoma_site_name` VARCHAR(255) NULL,
  `neotoma_entity_name` VARCHAR(255) NULL,
  PRIMARY KEY (`ID_SITE`, `ID_ENTITY`, `neotoma_ID_SITE`, `neotoma_ID_ENTITY`),
  INDEX `ID_ENTITY_idx` (`ID_ENTITY` ASC) VISIBLE,
  CONSTRAINT `ID_SITE`
    FOREIGN KEY (`ID_SITE`)
    REFERENCES `SPECIAL-EPD`.`entity` (`ID_SITE`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `ID_ENTITY`
    FOREIGN KEY (`ID_ENTITY`)
    REFERENCES `SPECIAL-EPD`.`entity` (`ID_ENTITY`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);
"

## date_info ----
"
CREATE TABLE `SPECIAL-EPD`.`date_info` (
  `ID_ENTITY` INT UNSIGNED NOT NULL,
  `ID_DATE_INFO` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `date_type` VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `depth` DOUBLE DEFAULT NULL,
  `thickness` DOUBLE DEFAULT NULL,
  `lab_num` VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `age_c14` DOUBLE DEFAULT NULL,
  `age_calib` DOUBLE DEFAULT NULL,
  `error` DOUBLE DEFAULT NULL,
  `material_dated` VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `age_used` TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `reason_age_not_used` VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `notes` TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`ID_DATE_INFO`),
  KEY `ID_ENTITY_date_info` (`ID_ENTITY`),
  CONSTRAINT `ID_ENTITY_date_info`
    FOREIGN KEY (`ID_ENTITY`)
    REFERENCES `SPECIAL-EPD`.`entity` (`ID_ENTITY`)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);
"

## sample ----
"
CREATE TABLE `SPECIAL-EPD`.`sample` (
  `ID_ENTITY` INT UNSIGNED NOT NULL,
  `ID_SAMPLE` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `depth` DOUBLE DEFAULT NULL,
  `thickness` DOUBLE DEFAULT NULL,
  `chronology_name` VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `age_type` VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `age` INT DEFAULT NULL,
  `age_younger` INT DEFAULT NULL,
  `age_older` INT DEFAULT NULL,
  `count_type` VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `sample_type` VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`ID_SAMPLE`),
  KEY `ID_ENTITY_sample` (`ID_ENTITY`),
  CONSTRAINT `ID_ENTITY_sample`
    FOREIGN KEY (`ID_ENTITY`)
    REFERENCES `SPECIAL-EPD`.`entity` (`ID_ENTITY`)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);
"

## taxon_name ----
"
CREATE TABLE `SPECIAL-EPD`.`taxon_name` (
  `ID_TAXON` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `taxon_name` VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`ID_TAXON`)
);
"

## pollen_count ----
"
CREATE TABLE `SPECIAL-EPD`.`pollen_count` (
  `ID_SAMPLE` INT UNSIGNED NOT NULL,
  `ID_TAXON` INT UNSIGNED NOT NULL,
  `amalgamation_level` INT UNSIGNED NOT NULL DEFAULT 0,
  `count` DOUBLE DEFAULT NULL,
  PRIMARY KEY (`ID_SAMPLE`, `ID_TAXON`, `amalgamation_level`),
  CONSTRAINT `ID_SAMPLE`
    FOREIGN KEY (`ID_SAMPLE`)
    REFERENCES `SPECIAL-EPD`.`sample` (`ID_SAMPLE`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `ID_TAXON`
    FOREIGN KEY (`ID_TAXON`)
    REFERENCES `SPECIAL-EPD`.`taxon_name` (`ID_TAXON`)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);
"

## model_name ----
"
CREATE TABLE `SPECIAL-EPD`.`model_name` (
  `ID_MODEL` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `model_name` varchar(100) NOT NULL,
  PRIMARY KEY (`ID_MODEL`)
);

LOCK TABLES `SPECIAL-EPD`.`model_name` WRITE;
/*!40000 ALTER TABLE `model_name` DISABLE KEYS */;
INSERT INTO `model_name`
  VALUES
    (1,'original_age_model_clean'),
    (2,'polynomial interpolation'),
    (3,'linear interpolation'),
    (4,'-777777'),
    (5,'smoothing spline interpolation'),
    (6,'other'),(7,'BACON_INTCAL13'),
    (8,'BACON_INTCAL20'),
    (9,'-999999'),
    (10,'Fairbanks0107'),
    (11,'CalPal 2007 HULU curve'),
    (12,'Calib 5.0.2'),
    (13,'Clam2.2_INTCAL13'),
    (14,'OxCal P_Sequence'),
    (15,'layer counting'),
    (16,'mixed effects regression'),
    (17,'CLAM'),
    (18,'Poisson-process Bayesian model');
/*!40000 ALTER TABLE `model_name` ENABLE KEYS */;
UNLOCK TABLES;
"

## age_model ----
"
CREATE TABLE `SPECIAL-EPD`.`age_model` (
  `ID_MODEL` INT UNSIGNED NOT NULL,
  `ID_SAMPLE` INT UNSIGNED NOT NULL,
  `mean` INT DEFAULT NULL,
  `median` INT DEFAULT NULL,
  `UNCERT_5` INT DEFAULT NULL,
  `UNCERT_25` INT DEFAULT NULL,
  `UNCERT_75` INT DEFAULT NULL,
  `UNCERT_95` INT DEFAULT NULL,
  PRIMARY KEY (`ID_MODEL`,`ID_SAMPLE`),
  KEY `ID_SAMPLE_age_model` (`ID_SAMPLE`),
  CONSTRAINT `ID_SAMPLE_age_model`
    FOREIGN KEY (`ID_SAMPLE`)
    REFERENCES `sample` (`ID_SAMPLE`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `ID_MODEL`
    FOREIGN KEY (`ID_MODEL`)
    REFERENCES `model_name` (`ID_MODEL`)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);
"
