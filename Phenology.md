Obtaining and analyzing phenology data
======================================

``` r
library(paceR)
load_pace_packages()
```

I assume that you have already [set up the connections to the database](README.md)

Santa Rosa Phenlology Data
--------------------------

First get Santa Rosa phenology data from the database using the saved View.

``` r
ph <- getv_Phenology(paceR_db)
  
# Filter to obtain Santa Rosa data only
ph <- ph %>% filter(Project == "SR")
```

Fortunately, Santa Rosa doesn't mix categorical scores, percents, and counts. Therefore, we can drop the percents and counts (which aren't used) and focus only on the scores.

``` r
# Remove unnecessary columns
ph <- ph %>% select(-PhenologyPercent, -PhenologyCount, -ScientificName, -RecordDate)

# Let's look at the data
ph
#> Source: local data frame [230,487 x 11]
#> 
#>    Project SiteName PhenologyDate TreeID TreeLabel             SpeciesName
#>      (chr)    (chr)        (date)  (int)     (chr)                   (chr)
#> 1       SR       CP    2006-02-01     21        NA Allophylus occidentalis
#> 2       SR       CP    2006-02-01     39        NA        Bursera simaruba
#> 3       SR       CP    2006-02-01     40        NA        Bursera simaruba
#> 4       SR       CP    2006-02-01     88        NA               Ficus sp.
#> 5       SR       CP    2006-02-01    105        NA   Guettarda macrosperma
#> 6       SR       CP    2006-02-01    106        NA   Guettarda macrosperma
#> 7       SR       CP    2006-02-01    107        NA   Guettarda macrosperma
#> 8       SR       CP    2006-02-01    110        NA   Guettarda macrosperma
#> 9       SR       CP    2006-02-01    112        NA   Guettarda macrosperma
#> 10      SR       CP    2006-02-01    139        NA          Luehea candida
#> ..     ...      ...           ...    ...       ...                     ...
#>    FoodPart Measurement PhenologyScore ResearcherName Comments
#>       (chr)       (chr)          (chr)          (chr)    (chr)
#> 1    Flower       Cover              0       Carnegie       NA
#> 2    Flower       Cover              0       Carnegie       NA
#> 3    Flower       Cover              0       Carnegie       NA
#> 4    Flower       Cover              0       Carnegie       NA
#> 5    Flower       Cover              0       Carnegie       NA
#> 6    Flower       Cover              0       Carnegie       NA
#> 7    Flower       Cover              0       Carnegie       NA
#> 8    Flower       Cover              0       Carnegie      low
#> 9    Flower       Cover              0       Carnegie     high
#> 10   Flower       Cover              0       Carnegie       NA
#> ..      ...         ...            ...            ...      ...
```

The data are stored in "long" format in the database, where each distinct measurement is a row. If you want to see them in a "wide" format, where all measurements for a given tree/session are in one row (like the spreadsheet in which they are collected), we can reshape it like so:

``` r
# First unite the "FoodPart" and "Measurement" columns
ph_wide <- ph %>% unite(FoodPartMeasurement, c(FoodPart, Measurement))

# Now spread PhenologyScore using FoodPartMeasurement as the key
ph_wide <- ph_wide %>% spread(FoodPartMeasurement, PhenologyScore)

# Do a bit of column rearranging
ph_wide <- ph_wide %>% select(1:6, 9:15, ResearcherName, Comments)

# Look at data
ph_wide
#> Source: local data frame [37,996 x 15]
#> 
#>    Project SiteName PhenologyDate TreeID TreeLabel             SpeciesName
#>      (chr)    (chr)        (date)  (int)     (chr)                   (chr)
#> 1       SR       CP    2006-02-01     21        NA Allophylus occidentalis
#> 2       SR       CP    2006-02-01     39        NA        Bursera simaruba
#> 3       SR       CP    2006-02-01     40        NA        Bursera simaruba
#> 4       SR       CP    2006-02-01     88        NA               Ficus sp.
#> 5       SR       CP    2006-02-01    105        NA   Guettarda macrosperma
#> 6       SR       CP    2006-02-01    106        NA   Guettarda macrosperma
#> 7       SR       CP    2006-02-01    107        NA   Guettarda macrosperma
#> 8       SR       CP    2006-02-01    110        NA   Guettarda macrosperma
#> 9       SR       CP    2006-02-01    112        NA   Guettarda macrosperma
#> 10      SR       CP    2006-02-01    139        NA          Luehea candida
#> ..     ...      ...           ...    ...       ...                     ...
#>    Flower Bud_Cover Flower_Cover Flower_Maturity Fruit_Cover
#>               (chr)        (chr)           (chr)       (chr)
#> 1                 0            0               0           0
#> 2                 0            0               0           0
#> 3                 0            0               0           0
#> 4                 0            0               0           0
#> 5                 0            0               0           0
#> 6                 0            0               0           0
#> 7                 0            0               0           0
#> 8                 0            0               0           0
#> 9                 0            0               0           0
#> 10                0            0               0           0
#> ..              ...          ...             ...         ...
#>    Fruit_Maturity Leaf_Cover Leaf_Maturity ResearcherName Comments
#>             (chr)      (chr)         (chr)          (chr)    (chr)
#> 1               0          2             0       Carnegie       NA
#> 2               0          1             0       Carnegie       NA
#> 3               0          1             0       Carnegie       NA
#> 4               0          4             0       Carnegie       NA
#> 5               0          4             0       Carnegie       NA
#> 6               0          4             0       Carnegie       NA
#> 7               0          4             0       Carnegie       NA
#> 8               0          4             0       Carnegie      low
#> 9               0          3             0       Carnegie     high
#> 10              0          3             0       Carnegie       NA
#> ..            ...        ...           ...            ...      ...
```
