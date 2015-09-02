paceR
=====

![ACG Costa Rica](ACG.jpg)

`paceR` is a collection of functions that make it easy to get data from the University of Calgary's PACE Database into R for further analysis.

To use the tools, you must have access to the PACE Database. If you don't know how to do this, you can ask [Fernando](mailto:facampos@ucalgary.ca), [Urs](mailto:urs.kalbitzer@ucalgary.ca), or [John](mailto:jaddicot@ucalgary.ca).

Preparation
-----------

To use this package, you first need to install `devtools` with:

``` r
    install.packages("devtools")
```

Then, you can install the latest development version of `paceR` from github:

``` r
  library(devtools)
  devtools::install_github("camposfa/paceR")
```

After you have installed the package once, you can simply load it in the future using:

``` r
  library(paceR)
```

This package makes heavy use of the data manipulation packages [stringr](http://cran.r-project.org/package=stringr), [lubridate](http://cran.r-project.org/package=lubridate), [tidyr](http://cran.r-project.org/package=tidyr), and [dplyr](http://cran.r-project.org/package=dplyr). If not already installed, `paceR` will install these packages automatically. It also provides a convenient function to load them all into your R session, if you want to use them for other tasks:

``` r
  load_pace_packages()
```

Connecting to the PACE Database
-------------------------------

To get data from PACE, you must create an SSH tunnel. If you have set up your SSH key, you can do it like this:

``` r
system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
```

With the tunnel created, we can now connect to the database(s). The primary database is called "monkey". There is also a secondary database called "paceR" that has a variety of convenient views that are designed to be used with this package. We'll create connections to both.

``` r
  # Connect to monkey database
  pace_db <- src_mysql(group = "PACE", user = "camposf", dbname = "monkey", password = NULL)
  
  # Connect to paceR database  
  paceR_db <- src_mysql(group = "PACE", user = "camposf", dbname = "paceR", password = NULL)
```

It should go without saying, but the above lines are just an example--if copied verbatim, they will **not** work for anyone but me (Fernando). You must first set up the SSH key and then modify these lines for your particular account.

Getting data from the database
------------------------------

Once you get the connection worked out, you now can pull data from the database. Whenever you download data, you must pass the name of the database connection that you're using. To do this correctly, it is crucial that you understand a major design decision that affects how the functions can be used.

### Downloading data using saved views (RECOMMENDED METHOD!)

The best way to get data from the database is to use the convenient saved "views". These are stored in the paceR database, and they should be called using the functions that begin with `getv_`

``` r
# Get a Individuals data
(i2 <- getv_Individual(paceR_db))
#> Source: local data frame [2,228 x 18]
#> 
#>    IndividualID Project PrimateSpecies     NameOf CodeName DateOfBirth
#> 1             1      SR           CCAP     2Tufts     2TUF  1988-01-01
#> 2             2      SR           CCAP        A-1     A-1-        <NA>
#> 3             3      SR           CCAP        Abu     ABU-  2005-04-25
#> 4             6      SR           CCAP         Al     AL--  1985-01-01
#> 5             7      SR           CCAP    Alfredo     ALFR  1997-01-01
#> 6             8      SR           CCAP      Alien     ALIE  1996-01-02
#> 7             9      SR           CCAP        Amy     AMY-  1989-01-01
#> 8            10      SR           CCAP      Amy96     AM96  1996-01-01
#> 9            11      SR           CCAP Babaganouj     BABA  1992-01-01
#> 10           12      SR           CCAP   BabyFace     BABY  1991-01-01
#> ..          ...     ...            ...        ...      ...         ...
#> Variables not shown: Sex (chr), BirthdateSource (chr), Mother (chr),
#>   GroupAtBirthName (chr), GroupAtBirthCode (chr), DateOfFirstSighting
#>   (date), DayDifference (int), AgeClassAtFirstSighting (chr),
#>   GroupAtFirstSightingName (chr), GroupAtFirstSightingCode (chr),
#>   Phenotype (chr), ProjectID (int)

# Get a condensed version
(i2 <- getv_Individual(paceR_db, full = FALSE))
#> Source: local data frame [2,228 x 5]
#> 
#>    IndividualID     NameOf Project DateOfBirth     Sex
#> 1             1     2Tufts      SR  1988-01-01  Female
#> 2             2        A-1      SR        <NA>    Male
#> 3             3        Abu      SR  2005-04-25  Female
#> 4             6         Al      SR  1985-01-01    Male
#> 5             7    Alfredo      SR  1997-01-01    Male
#> 6             8      Alien      SR  1996-01-02    Male
#> 7             9        Amy      SR  1989-01-01  Female
#> 8            10      Amy96      SR  1996-01-01 Unknown
#> 9            11 Babaganouj      SR  1992-01-01    Male
#> 10           12   BabyFace      SR  1991-01-01    Male
#> ..          ...        ...     ...         ...     ...
```

### Downloading raw database tables (NOT RECOMMENDED!)

If you want to download **raw database tables**, then you should use function `get_pace_tbl()`. All tables are stored in the "monkey" database, and so when you use this function, you must pass the connection to the "monkey" database in addition to the name of the table that you want to download. For example:

``` r
# Get the raw individuals table
(i <- get_pace_tbl(pace_db, "tblIndividual"))
#> Source: local data frame [2,228 x 20]
#> 
#>    ID ProjectID PrimateSpeciesID     NameOf CodeName DateOfBirth
#> 1   1         1                1     2Tufts     2TUF  1988-01-01
#> 2   2         1                1        A-1     A-1-          NA
#> 3   3         1                1        Abu     ABU-  2005-04-25
#> 4   6         1                1         Al     AL--  1985-01-01
#> 5   7         1                1    Alfredo     ALFR  1997-01-01
#> 6   8         1                1      Alien     ALIE  1996-01-02
#> 7   9         1                1        Amy     AMY-  1989-01-01
#> 8  10         1                1      Amy96     AM96  1996-01-01
#> 9  11         1                1 Babaganouj     BABA  1992-01-01
#> 10 12         1                1   BabyFace     BABY  1991-01-01
#> .. ..       ...              ...        ...      ...         ...
#> Variables not shown: BirthdateSource (chr), SexID (int), MotherID (int),
#>   MatrilineID (int), GroupAtBirthID (int), DateOfFirstSighting (chr),
#>   DayDifference (int), AgeClassAtFirstSightingID (int),
#>   GroupAtFirstSightingID (int), VisionPhenotypeID (int), Comments (chr),
#>   Comments_2 (chr), CommentsJFA (chr), CommentsGenetics (chr)

# Get the raw deaths table
(d <- get_pace_tbl(pace_db, "tblIndividualDeath"))
#> Source: local data frame [1,055 x 7]
#> 
#>    ID IndividualID DateOfDeath CauseOfDeathID SourceOfInformation
#> 1   0         7152  1984-05-29            157            DODKnown
#> 2   1            1  1997-03-15              5                  NA
#> 3   2            2  2004-02-13              5                  NA
#> 4   3            6  1997-02-28              5                  NA
#> 5   4            8  2004-02-20              5                  NA
#> 6   5            9  1997-02-24              5                  NA
#> 7   6           10  1997-01-28              5                  NA
#> 8   7           12  1997-03-15              5                  NA
#> 9   8           15  2000-05-23              5                  NA
#> 10  9           16  1997-03-15              5                  NA
#> .. ..          ...         ...            ...                 ...
#> Variables not shown: DateOfDeathFromCensus (chr), Comments (chr)
```

Note that the "foreign keys" (i.e., the columns that end with "ID") are just uninformative numbers! To make use of the data, you might need to join table by their ID relevant fields.

``` r
# Join the individuals and deaths tables
id <- left_join(i, d, by = c("ID" = "IndividualID"))
```

You can see that there are many more ID fields that would need to be joined. It can very inconvenient to work with the data this way!
