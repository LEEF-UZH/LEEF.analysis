# Number of species bemovi

```sql
SELECT 
	"bemovi_mag_16" AS measurement,
	timestamp,
	bottle,
	COUNT(species) AS no_species
FROM 
	bemovi_mag_16__mean_density_per_ml
GROUP BY
	timestamp,
	bottle
UNION ALL	
SELECT 
	"bemovi_mag_25" AS measurement,
	timestamp,
	bottle,
	COUNT(species) AS no_species
FROM 
	bemovi_mag_25__mean_density_per_ml
GROUP BY
	timestamp,
	bottle
UNION ALL	
SELECT 
	"bemovi_mag_25" AS measurement,
	timestamp,
	bottle,
	COUNT(species) AS no_species
FROM 
	bemovi_mag_25__mean_density_per_ml_cropped
GROUP BY
	timestamp,
	bottle
UNION ALL	
SELECT 
	"flowcam" AS measurement,
	timestamp,
	bottle,
	COUNT(species) AS no_species
FROM 
	flowcam__algae_density
GROUP BY
	timestamp,
	bottle
UNION ALL	
SELECT 
	"flowcytometer" AS measurement,
	timestamp,
	bottle,
	COUNT(name) AS no_species
FROM 
	flowcytometer__flowcytometer
GROUP BY
	timestamp,
	bottle
UNION ALL	
SELECT 
	"manualcount" AS measurement,
	timestamp,
	bottle,
	COUNT(species) AS no_species
FROM 
	manualcount__manualcount
GROUP BY
	timestamp,
	bottle
```

