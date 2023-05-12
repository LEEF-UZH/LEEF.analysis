CREATE TABLE `flowcam__algae_density` (
  `date_flowcam` TEXT,
  `species` TEXT,
  `bottle` TEXT,
  `composition` TEXT,
  `temperature` TEXT,
  `incubator` TEXT,
  `volume_imaged` REAL,
  `dilution_factor` INTEGER,
  `richness` INTEGER,
  `count` INTEGER,
  `density` REAL,
  `timestamp` TEXT
);
CREATE TABLE `flowcytometer__flowcytometer_traits` (
  `sample` TEXT,
  `fsc.a` REAL,
  `ssc.a` REAL,
  `fl1.a` REAL,
  `fl2.a` REAL,
  `fl3.a` REAL,
  `fl4.a` REAL,
  `fsc.h` REAL,
  `ssc.h` REAL,
  `fl1.h` REAL,
  `fl2.h` REAL,
  `fl3.h` REAL,
  `fl4.h` REAL,
  `width` REAL,
  `time` REAL,
  `timestamp` TEXT,
  `bottle` TEXT,
  `dilution_factor` INTEGER,
  `length` REAL,
  `volume` REAL
);
CREATE TABLE `flowcytometer__flowcytometer_density` (
  `timestamp` TEXT,
  `filename` TEXT,
  `bottle` TEXT,
  `date` TEXT,
  `sample` TEXT,
  `volume` INTEGER,
  `total.counts` INTEGER,
  `tot_density_perml` REAL,
  `specname` TEXT,
  `dilution_factor` INTEGER,
  `sample_letter` TEXT,
  `sample_number` INTEGER,
  `species` TEXT,
  `count` INTEGER,
  `density` REAL
);
CREATE TABLE `manualcount__manualcount_density` (
  `timestamp` TEXT,
  `bottle` TEXT,
  `species` TEXT,
  `ml_counted` INTEGER,
  `count` INTEGER,
  `density` INTEGER
);
CREATE TABLE `o2meter__o2meter` (
  `timestamp` TEXT,
  `bottle` TEXT,
  `sensor` INTEGER,
  `date` TEXT,
  `time` TEXT,
  `channel` INTEGER,
  `user` TEXT,
  `sensorid` INTEGER,
  `sensor_name` TEXT,
  `delta_t` REAL,
  `time_unit` TEXT,
  `value` REAL,
  `o2_unit` TEXT,
  `mode` TEXT,
  `phase` REAL,
  `phase_unit` TEXT,
  `amplitude` INTEGER,
  `amplitude_unit` TEXT,
  `temp` REAL,
  `temp_unit` TEXT,
  `pressure` REAL,
  `pressure_unit` TEXT,
  `salinity` REAL,
  `salinity_unit` TEXT,
  `error` INTEGER,
  `cal0` REAL,
  `cal0_unit` TEXT,
  `t0` REAL,
  `t0_unit` TEXT,
  `o2cal2nd` REAL,
  `o2_unit1` TEXT,
  `cal2nd` REAL,
  `cal2nd_unit` TEXT,
  `t2nd` REAL,
  `t2nd_unit` TEXT,
  `calpressure` REAL,
  `calpressure_unit` TEXT,
  `f1` REAL,
  `dphi1` REAL,
  `dksv1` REAL,
  `dphi2` REAL,
  `dksv2` INTEGER,
  `m` REAL,
  `cal_mode` TEXT,
  `signalledcurrent` REAL,
  `user_signal_intensity` INTEGER,
  `referenceledcurrent` REAL,
  `reference_amplitude` REAL,
  `device_serial` TEXT,
  `fwversion` TEXT,
  `swversion` TEXT,
  `sensor_type` TEXT,
  `batchid` TEXT,
  `calibration_date` TEXT,
  `sensor_lot` INTEGER,
  `presens_calibr` INTEGER,
  `battery_voltage` REAL,
  `battery_voltage_unit` TEXT
);
CREATE TABLE `bemovi_mag_25__mean_density_per_ml_cropped` (
  `timestamp` TEXT,
  `date` TEXT,
  `species` TEXT,
  `composition_id` TEXT,
  `bottle` TEXT,
  `temperature_treatment` TEXT,
  `magnification` INTEGER,
  `sample` INTEGER,
  `density` REAL
);
CREATE TABLE `bemovi_mag_25__mean_density_per_ml` (
  `timestamp` TEXT,
  `date` TEXT,
  `species` TEXT,
  `composition_id` TEXT,
  `bottle` TEXT,
  `temperature_treatment` TEXT,
  `magnification` INTEGER,
  `sample` INTEGER,
  `density` REAL
);
CREATE TABLE `bemovi_mag_25__morph_mvt_cropped` (
  `timestamp` TEXT,
  `file` TEXT,
  `mean_grey` REAL,
  `sd_grey` REAL,
  `mean_area` REAL,
  `sd_area` REAL,
  `mean_perimeter` REAL,
  `sd_perimeter` REAL,
  `mean_major` REAL,
  `sd_major` REAL,
  `mean_minor` REAL,
  `sd_minor` REAL,
  `mean_ar` REAL,
  `sd_ar` REAL,
  `mean_turning` REAL,
  `sd_turning` REAL,
  `duration` REAL,
  `n_frames` INTEGER,
  `max_net` REAL,
  `net_disp` INTEGER,
  `net_speed` REAL,
  `gross_disp` REAL,
  `gross_speed` REAL,
  `max_step` REAL,
  `min_step` REAL,
  `sd_step` REAL,
  `sd_gross_speed` REAL,
  `max_gross_speed` REAL,
  `min_gross_speed` REAL,
  `id` TEXT,
  `date` TEXT,
  `bottle` TEXT,
  `composition_id` TEXT,
  `temperature_treatment` TEXT,
  `magnification` INTEGER,
  `dilution_factor` INTEGER,
  `sample` INTEGER,
  `video` INTEGER,
  `species` TEXT,
  `species_probability` REAL,
  `cryptomonas_prob` REAL,
  `debris_and_other_prob` REAL,
  `didinium_prob` REAL,
  `paramecium_bursaria_prob` REAL,
  `stylonychia1_prob` REAL,
  `tetrahymena_prob` REAL,
  `euplotes_prob` INTEGER,
  `paramecium_caudatum_prob` INTEGER,
  `loxocephallus_prob` INTEGER,
  `stylonychia2_prob` INTEGER,
  `coleps_irchel_prob` INTEGER,
  `colpidium_prob` INTEGER,
  `dexiostoma_prob` INTEGER
);
CREATE TABLE `bemovi_mag_25__morph_mvt` (
  `timestamp` TEXT,
  `file` TEXT,
  `mean_grey` REAL,
  `sd_grey` REAL,
  `mean_area` REAL,
  `sd_area` REAL,
  `mean_perimeter` REAL,
  `sd_perimeter` REAL,
  `mean_major` REAL,
  `sd_major` REAL,
  `mean_minor` REAL,
  `sd_minor` REAL,
  `mean_ar` REAL,
  `sd_ar` REAL,
  `mean_turning` REAL,
  `sd_turning` REAL,
  `duration` REAL,
  `n_frames` INTEGER,
  `max_net` REAL,
  `net_disp` INTEGER,
  `net_speed` REAL,
  `gross_disp` REAL,
  `gross_speed` REAL,
  `max_step` REAL,
  `min_step` REAL,
  `sd_step` REAL,
  `sd_gross_speed` REAL,
  `max_gross_speed` REAL,
  `min_gross_speed` REAL,
  `id` TEXT,
  `date` TEXT,
  `bottle` TEXT,
  `composition_id` TEXT,
  `temperature_treatment` TEXT,
  `magnification` INTEGER,
  `dilution_factor` INTEGER,
  `sample` INTEGER,
  `video` INTEGER,
  `species` TEXT,
  `species_probability` REAL,
  `cryptomonas_prob` REAL,
  `debris_and_other_prob` REAL,
  `didinium_prob` REAL,
  `paramecium_bursaria_prob` REAL,
  `stylonychia1_prob` REAL,
  `tetrahymena_prob` REAL,
  `euplotes_prob` INTEGER,
  `paramecium_caudatum_prob` INTEGER,
  `loxocephallus_prob` INTEGER,
  `stylonychia2_prob` INTEGER,
  `dexiostoma_prob` INTEGER,
  `coleps_irchel_prob` INTEGER,
  `colpidium_prob` INTEGER
);
CREATE TABLE `bemovi_mag_16__mean_density_per_ml` (
  `timestamp` TEXT,
  `date` TEXT,
  `species` TEXT,
  `composition_id` TEXT,
  `bottle` TEXT,
  `temperature_treatment` TEXT,
  `magnification` INTEGER,
  `sample` INTEGER,
  `density` REAL
);
CREATE TABLE `bemovi_mag_16__morph_mvt` (
  `timestamp` TEXT,
  `file` TEXT,
  `mean_grey` REAL,
  `sd_grey` REAL,
  `mean_area` REAL,
  `sd_area` REAL,
  `mean_perimeter` REAL,
  `sd_perimeter` REAL,
  `mean_major` REAL,
  `sd_major` REAL,
  `mean_minor` REAL,
  `sd_minor` REAL,
  `mean_ar` REAL,
  `sd_ar` REAL,
  `mean_turning` REAL,
  `sd_turning` REAL,
  `duration` REAL,
  `n_frames` INTEGER,
  `max_net` REAL,
  `net_disp` INTEGER,
  `net_speed` REAL,
  `gross_disp` REAL,
  `gross_speed` REAL,
  `max_step` REAL,
  `min_step` REAL,
  `sd_step` REAL,
  `sd_gross_speed` REAL,
  `max_gross_speed` REAL,
  `min_gross_speed` REAL,
  `id` TEXT,
  `date` TEXT,
  `bottle` TEXT,
  `composition_id` TEXT,
  `temperature_treatment` TEXT,
  `magnification` INTEGER,
  `dilution_factor` INTEGER,
  `sample` INTEGER,
  `video` INTEGER,
  `species` TEXT,
  `species_probability` REAL,
  `didinium_prob` REAL,
  `paramecium_bursaria_prob` REAL,
  `smaller_ciliates_prob` REAL,
  `stylonychia1_prob` REAL,
  `euplotes_prob` INTEGER,
  `paramecium_caudatum_prob` INTEGER,
  `stylonychia2_prob` INTEGER,
  `coleps_irchel_prob` INTEGER,
  `colpidium_prob` INTEGER
);
CREATE TABLE `bemovi_mag_25__mean_density_per_ml_non_cropped` (
  `timestamp` TEXT,
  `date` TEXT,
  `species` TEXT,
  `composition_id` TEXT,
  `bottle` TEXT,
  `temperature_treatment` TEXT,
  `magnification` INTEGER,
  `sample` INTEGER,
  `density` REAL
);
CREATE TABLE `bemovi_mag_25__morph_mvt_non_cropped` (
  `timestamp` TEXT,
  `file` TEXT,
  `mean_grey` REAL,
  `sd_grey` REAL,
  `mean_area` REAL,
  `sd_area` REAL,
  `mean_perimeter` REAL,
  `sd_perimeter` REAL,
  `mean_major` REAL,
  `sd_major` REAL,
  `mean_minor` REAL,
  `sd_minor` REAL,
  `mean_ar` REAL,
  `sd_ar` REAL,
  `mean_turning` REAL,
  `sd_turning` REAL,
  `duration` REAL,
  `n_frames` INTEGER,
  `max_net` REAL,
  `net_disp` INTEGER,
  `net_speed` REAL,
  `gross_disp` REAL,
  `gross_speed` REAL,
  `max_step` REAL,
  `min_step` REAL,
  `sd_step` REAL,
  `sd_gross_speed` REAL,
  `max_gross_speed` REAL,
  `min_gross_speed` REAL,
  `id` TEXT,
  `date` TEXT,
  `bottle` TEXT,
  `composition_id` TEXT,
  `temperature_treatment` TEXT,
  `magnification` INTEGER,
  `dilution_factor` INTEGER,
  `sample` INTEGER,
  `video` INTEGER,
  `species` TEXT,
  `species_probability` REAL,
  `cryptomonas_prob` REAL,
  `debris_and_other_prob` REAL,
  `didinium_prob` REAL,
  `paramecium_bursaria_prob` REAL,
  `stylonychia1_prob` REAL,
  `tetrahymena_prob` REAL,
  `euplotes_prob` INTEGER,
  `paramecium_caudatum_prob` INTEGER,
  `loxocephallus_prob` INTEGER,
  `stylonychia2_prob` INTEGER,
  `dexiostoma_prob` INTEGER,
  `coleps_irchel_prob` INTEGER,
  `colpidium_prob` INTEGER
);
CREATE TABLE `composition` (
  `composition` TEXT,
  `richness` INTEGER,
  `Chlamydomonas` INTEGER,
  `Cryptomonas` INTEGER,
  `Monoraphidium` INTEGER,
  `Cosmarium` INTEGER,
  `Staurastrum1` INTEGER,
  `Staurastrum2` INTEGER,
  `Desmodesmus` INTEGER,
  `Tetrahymena` INTEGER,
  `Colpidium` INTEGER,
  `Loxocephallus` INTEGER,
  `Dexiostoma` INTEGER,
  `Paramecium_caudatum` INTEGER,
  `Stylonychia1` INTEGER,
  `Stylonychia2` INTEGER,
  `Coleps_irchel` INTEGER,
  `Paramecium_bursaria` INTEGER,
  `Euplotes` INTEGER,
  `Didinium` INTEGER
);
CREATE TABLE `experimental_design` (
  `bottle` TEXT,
  `temperature` TEXT,
  `richness` INTEGER,
  `composition` TEXT,
  `incubator` TEXT
);
CREATE TABLE `light_decline_schedule` (
  `timestamp` INTEGER,
  `temperature` INTEGER,
  `light` INTEGER,
  `incubator_program` TEXT,
  `comment` INTEGER
);
CREATE TABLE `immigration_schedule` (
  `timestamp` INTEGER,
  `comment` INTEGER
);
CREATE TABLE `toc__toc` (
  `filename` TEXT,
  `anaysis_time` TEXT,
  `timestamp` INTEGER,
  `bottle` TEXT,
  `position` INTEGER,
  `identification` TEXT,
  `inj_type` TEXT,
  `conc` REAL,
  `cv` REAL,
  `conc_1` REAL,
  `conc_2` REAL,
  `conc_3` REAL,
  `id` INTEGER,
  `cv_12` REAL,
  `cv_13` REAL,
  `cv_23` REAL,
  `conc_12` REAL,
  `conc_13` REAL,
  `conc_23` REAL,
  `conc_org` REAL
);
CREATE TABLE IF NOT EXISTS "flowcam__algae_traits" (
	"particle_id"	INTEGER,
	"area_abd"	REAL,
	"area_filled"	REAL,
	"aspect_ratio"	REAL,
	"average_blue"	REAL,
	"average_green"	REAL,
	"average_red"	REAL,
	"calibration_factor"	REAL,
	"calibration_image"	INTEGER,
	"camera"	INTEGER,
	"capture_x"	INTEGER,
	"capture_y"	INTEGER,
	"ch1_area"	INTEGER,
	"ch1_peak"	INTEGER,
	"ch1_width"	INTEGER,
	"ch2_area"	INTEGER,
	"ch2_peak"	INTEGER,
	"ch2_width"	INTEGER,
	"ch2_ch1_ratio"	INTEGER,
	"circle_fit"	REAL,
	"circularity"	REAL,
	"circularity_hu"	REAL,
	"compactness"	REAL,
	"convex_perimeter"	REAL,
	"convexity"	REAL,
	"date_flowcam"	TEXT,
	"diameter_abd"	REAL,
	"diameter_esd"	REAL,
	"edge_gradient"	REAL,
	"elongation"	REAL,
	"feret_angle_max"	INTEGER,
	"feret_angle_min"	INTEGER,
	"fiber_curl"	REAL,
	"fiber_straightness"	REAL,
	"filter_score"	INTEGER,
	"geodesic_aspect_ratio"	REAL,
	"geodesic_length"	REAL,
	"geodesic_thickness"	REAL,
	"image_file"	TEXT,
	"image_height"	INTEGER,
	"image_width"	INTEGER,
	"image_x"	INTEGER,
	"image_y"	INTEGER,
	"intensity"	REAL,
	"length"	REAL,
	"particles_per_chain"	INTEGER,
	"perimeter"	REAL,
	"ratio_blue_green"	REAL,
	"ratio_red_blue"	REAL,
	"ratio_red_green"	REAL,
	"roughness"	REAL,
	"scatter_area"	INTEGER,
	"scatter_peak"	INTEGER,
	"scatter_width"	INTEGER,
	"sigma_intensity"	REAL,
	"source_image"	INTEGER,
	"sum_intensity"	INTEGER,
	"symmetry"	REAL,
	"time"	TEXT,
	"timestamp_flowcam"	TEXT,
	"transparency"	REAL,
	"volume_abd"	REAL,
	"volume_esd"	REAL,
	"width"	REAL,
	"bottle"	TEXT,
	"volume_imaged"	REAL,
	"dilution_factor"	INTEGER,
	"area_x"	TEXT,
	"area_y"	TEXT,
	"subarea"	TEXT,
	"temperature"	TEXT,
	"richness"	INTEGER,
	"composition"	TEXT,
	"incubator"	TEXT,
	"species"	TEXT,
	"species_probability"	REAL,
	"airbubbles_prob"	REAL,
	"chlamydomonas_prob"	REAL,
	"chlamydomonasclumps_prob"	REAL,
	"coleps_irchel_prob"	REAL,
	"cosmarium_prob"	REAL,
	"cryptomonas_prob"	REAL,
	"debris_prob"	REAL,
	"desmodesmus_prob"	REAL,
	"digestedalgae_prob"	REAL,
	"loxocephallus_prob"	REAL,
	"otherciliate_prob"	REAL,
	"tetrahymena_prob"	REAL,
	"staurastrum1_prob"	INTEGER,
	"colpidium_prob"	INTEGER,
	"colpidiumvacuoles_prob"	INTEGER,
	"monoraphidium_prob"	INTEGER,
	"staurastrum2_prob"	INTEGER,
	"dexiostoma_prob"	INTEGER,
	"timestamp"	TEXT,
	"flowcell"	text,
	"filtration"	text,
	"instrument"	TEXT,
	"diameter_fd"	TEXT,
	"elapsed_time"	TEXT,
	"original_reference_id"	TEXT,
	"sphere_complement"	TEXT,
	"sphere_count"	TEXT,
	"sphere_unknown"	TEXT,
	"sphere_volume"	TEXT,
	"dividingchlamydomonas_prob"	REAL,
	"small_unidentified_prob"	REAL,
	"small_cells_prob"	TEXT,
	"desmodesmusclumps_prob"	TEXT
);

CREATE VIEW o2
AS
SELECT
  *
FROM
  (
   SELECT
     timestamp,
     cast(
          julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) -
          julianday('2021-09-20') AS integer
     ) AS day,
     bottle,
     sensor,
     temp AS 'temperature_actual',
     value AS 'percent_o2',
     'o2meter' AS measurement
   FROM
     o2meter__o2meter
  )
INNER JOIN
  (
   SELECT
     bottle,
     temperature,
     richness,
     composition,
     incubator
   FROM
	 experimetal_design
  )
USING
  (bottle)
/* o2(timestamp,day,bottle,sensor,temperature_actual,percent_o2,measurement,temperature,richness,composition,incubator) */;
CREATE VIEW density
AS
SELECT
  *
FROM
  (
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('2021-09-20') as integer ) AS day,
     bottle,
     'bemovi_mag_16' AS measurement,
     species,
     density
   FROM
     bemovi_mag_16__mean_density_per_ml
   UNION ALL
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('2021-09-20') as integer ) AS day,
     bottle,
     'bemovi_mag_25' AS measurement,
     species,
     density
   FROM
     bemovi_mag_25__mean_density_per_ml
   UNION ALL
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('2021-09-20') as integer ) AS day,
     bottle,
     'bemovi_mag_25_cropped' AS measurement,
     species,
     density
   FROM
     bemovi_mag_25__mean_density_per_ml_cropped
   UNION ALL
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('2021-09-20') as integer ) AS day,
     bottle,
     'bemovi_mag_25_non_cropped' AS measurement,
     species,
     density
   FROM
     bemovi_mag_25__mean_density_per_ml_non_cropped
   UNION ALL
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('2021-09-20') as integer ) AS day,
     bottle,
     'flowcam' AS measurement,
     species,
     density
   FROM
     flowcam__algae_density
   UNION ALL
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('2021-09-20') as integer ) AS day,
     bottle,
     'flowcytometer' AS measurement,
     species,
     density
   FROM
     flowcytometer__flowcytometer_density
   UNION ALL
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('2021-09-20') as integer ) AS day,
     bottle,
     'manualcount' AS measurement,
     species,
     density
   FROM
     manualcount__manualcount_density
  )
INNER JOIN
  (
   SELECT
     bottle,
     temperature,
     richness,
     composition,
     incubator
   FROM
	 experimetal_design
  )
USING
  (bottle)
/* density(timestamp,day,bottle,measurement,species,density,temperature,richness,composition,incubator) */;
CREATE VIEW immigration_event
AS
SELECT
  *
FROM
  (
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('2021-09-20') as integer ) AS day,
     comment
   FROM
     immigration_schedule
  )
/* immigration_event(timestamp,day,comment) */;
CREATE VIEW light_decline
AS
SELECT
  *
FROM
  (
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('2021-09-20') as integer ) AS day,
     temperature,
     light
   FROM
     light_decline_schedule
  )
/* light_decline(timestamp,day,temperature,light) */;
CREATE VIEW toc
AS
SELECT
  *
FROM
  (
   SELECT
     timestamp,
     cast(
          julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) -
          julianday('2021-09-20') AS integer
     ) AS day,
     bottle,
     inj_type AS 'type',
	 conc AS 'concentration',
	 cv AS 'cv'
   FROM
     toc__toc
   WHERE
	 bottle IS NOT NULL
	)
INNER JOIN
  (
   SELECT
     bottle,
     temperature,
     richness,
     composition,
     incubator
   FROM
	   experimetal_design
  )
USING
  (bottle)
/* toc(timestamp,day,bottle,type,concentration,cv,temperature,richness,composition,incubator) */;

CREATE VIEW experimetal_design
AS
SELECT
  *
FROM
  experimental_design;
