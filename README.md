# mnrisks
Helper functions and data from Minnesota's 2011 NEI air modeling results.  


1. Install
2. Load model inputs
3. Block group spatial averages
4. Add data


## 1. Install
```r
install.packages("tibble")

devtools::install_github("dkvale/mnrisks")
```


## 2. Load model inputs

```r
library(mnrisks)
library(knitr)
```

__Load all modeling receptors__  
```r
all_receptors <- receptors()

kable(head(all_receptors))
```

| receptor|geoid        |     lat|     long|    utm_x|   utm_y|county |  fips| total_bg_receptors| metro|
|--------:|:------------|-------:|--------:|--------:|-------:|:------|-----:|------------------:|-----:|
|    46318|270017701001 | 46.7195| -93.1178| 490997.4| 5174001|AITKIN | 27001|                 86|     0|
|    46383|270017701001 | 46.7195| -93.0785| 494000.8| 5173997|AITKIN | 27001|                 86|     0|
|    46514|270017701001 | 46.7465| -93.1178| 491001.9| 5177001|AITKIN | 27001|                 86|     0|
|    46526|270017701001 | 46.7461| -93.3142| 475999.8| 5176998|AITKIN | 27001|                 86|     0|
|    46579|270017701001 | 46.7457| -93.4320| 467001.5| 5176996|AITKIN | 27001|                 86|     0|
|    46591|270017701001 | 46.7465| -93.0786| 493996.2| 5176997|AITKIN | 27001|                 86|     0|  

__Load a single block group's receptors__  
```r
points <- get_receptors(bg_id = c(271090017012))

plot(points$lat, points$long, pch = 16)

kable(head(points))
```

| receptor|geoid        |     lat|     long|    utm_x|   utm_y|county  |  fips| total_bg_receptors| metro|
|--------:|:------------|-------:|--------:|--------:|-------:|:-------|-----:|------------------:|-----:|
|     4597|271090017012 | 44.0699| -92.4932| 540584.5| 4879761|OLMSTED | 27109|                  3|     0|
|     4602|271090017012 | 44.0709| -92.4917| 540704.0| 4879873|OLMSTED | 27109|                  3|     0|
|     4606|271090017012 | 44.0714| -92.4965| 540319.2| 4879926|OLMSTED | 27109|                  3|     0|


__Block group Census 2010 data__  
```r
bg_census <- get_blockgroups()

kable(head(bg_census))

```
| fid|        geoid| metro| county_fips| population| poverty150_count|     area| percent_in_poverty| population_density|county |
|---:|------------:|-----:|-----------:|----------:|----------------:|--------:|------------------:|------------------:|:------|
|   1| 270030501154|     1|       27003|       1495|              264| 23395500|          17.658863|           197.9400|ANOKA  |
|   2| 270030501153|     1|       27003|       1146|              145| 11626200|          12.652705|           305.3315|ANOKA  |
|   3| 270030502181|     1|       27003|       1810|               13| 11347200|           0.718232|           494.0997|ANOKA  |
|   4| 270030502182|     1|       27003|       1341|                0|  6212700|           0.000000|           668.6105|ANOKA  |
|   5| 270030502191|     1|       27003|       1318|               21|  7044300|           1.593323|           579.5652|ANOKA  |
|   6| 270030502192|     1|       27003|       1862|                0|  2190600|           0.000000|          2632.9424|ANOKA  |



## 3. Calculate Block group spatial averages using the area coverage of each receptor

__View the block group areas for each receptor__
```r
receptor_areas <- receptor_bg_areas()

kable(head(receptor_areas))
```

|geoid        | receptor| area_wts|
|:------------|--------:|--------:|
|270531260001 |    22213|   0.0005|
|270531260001 |    22550|   0.2202|
|270531260001 |    22393|   0.3994|
|270531260001 |    22530|   0.3786|
|270531260001 |    22354|   0.0013|


The `spatial_bg_avg` function will calculate block group averages by joining the area fractions above 
to MNRISKS receptor results.

```r
library(readr)
library(dplyr)

setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/Mnrisks/R/mnrisks2011")

df <- read_csv("data\\onRoad - Inhalation - DieselPM risk ratio.csv")

names(df)[7] <- "Inhalation Cancer Risk"


# For a single pollutant
chrysene <- filter(df, `COPC Name` == "Chrysene")

bg_avg <- chrysene %>% 
          group_by(`Block Group`) %>%
          summarize(avg_inh_cancer_risk = spatial_bg_avg(values       = chrysene$`Inhalation Cancer Risk`, 
                                                         receptors    = chrysene$Receptor,
                                                         bg_geoids    = `Block Group`,
                                                         results_only = TRUE))

kable(head(arrange(bg_avg, -sum_of_area_wts)))


# For multiple pollutants
bg_avg <- df %>% 
          group_by(`Block Group`, `COPC Name`) %>%
          summarize(Pollutant = `COPC Name`[1],
                    avg_inh_cancer_risk = spatial_bg_avg(values       = filter(df, `COPC Name` == Pollutant)$`Inhalation Cancer Risk`, 
                                                         receptors    = filter(df, `COPC Name` == Pollutant)$Receptor,
                                                         bg_geoids    = `Block Group`,
                                                         results_only = TRUE))
                                                         
kable(head(bg_avg))
```

|geoid        | sum_of_area_wts| bg_avg_Inhalation Cancer Risk|
|:------------|---------------:|-----------------------------:|
|270531054001 |          0.8071|                         1e-07|
|270531054002 |          0.0842|                         1e-07|
|270531044002 |          0.0220|                         1e-07|
|270530059012 |          0.0122|                         1e-07|
|270531261001 |          0.0094|                         1e-07|
|270531261003 |          0.0001|                         1e-07|






## 4. Add data

```r

save(new_data, file = "data/new_data.rdata")

devtools::document()
```
