paceR
=====

`paceR` is a collection of functions that make it easy to get data from the University of Calgary's PACE Database into R for further analysis.

*There are no data included in this package.*

To use the tools, you must have access to the PACE Database. If you don't know how to do this, you can ask [[mailto:facampos@ucalgary.ca](mailto:facampos@ucalgary.ca)](Fernando), [[[mailto:urs.kalbitzer@ucalgary.ca](mailto:urs.kalbitzer@ucalgary.ca)](Urs), or [[mailto:jaddicot@ucalgary.ca](mailto:jaddicot@ucalgary.ca)](John).

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

This package makes heavy use of the data manipulation packages [stringr](http://cran.r-project.org/package=stringr), [lubridate](http://cran.r-project.org/package=lubridate), [tidyr](http://cran.r-project.org/package=tidyr), and [dplyr](http://cran.r-project.org/package=dplyr). If not already installed, `plhdbR` will install these packages automatically. It also provides a convenient function to load them all into your R session, if you want to use them for other tasks:

``` r
  load_pace_packages()
```

Getting Data from PACE
----------------------

To get data from PACE, you must create an SSH tunnel. If you have set up your SSH key, you can do it like this (but modify for your connection):

``` r
system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", dbname = "monkey", password = NULL)
```

Now we can pull data out.

``` r

# Get the full individuals table
get_individuals(pace_db)
#> Source: local data frame [2,223 x 16]
#> 
#>    IndividualID ProjectName PrimateSpecies     NameOf CodeName DateOfBirth
#> 1             1  Santa Rosa           CCAP     2Tufts     2TUF  1988-01-01
#> 2             2  Santa Rosa           CCAP        A-1     A-1-          NA
#> 3             3  Santa Rosa           CCAP        Abu     ABU-  2005-04-25
#> 4             6  Santa Rosa           CCAP         Al     AL--  1985-01-01
#> 5             7  Santa Rosa           CCAP    Alfredo     ALFR          NA
#> 6             8  Santa Rosa           CCAP      Alien     ALIE  1996-01-02
#> 7             9  Santa Rosa           CCAP        Amy     AMY-  1989-01-01
#> 8            10  Santa Rosa           CCAP      Amy96     AM96  1996-01-01
#> 9            11  Santa Rosa           CCAP Babaganouj     BABA  1992-01-01
#> 10           12  Santa Rosa           CCAP   BabyFace     BABY  1991-01-01
#> ..          ...         ...            ...        ...      ...         ...
#> Variables not shown: BirthdateSource (chr), Sex (chr), Mother (chr),
#>   MatrilineID (int), GroupAtBirth (chr), DateOfFirstSighting (chr),
#>   DayDifference (int), AgeClassAtFirstSighting (chr), GroupAtFirstSighting
#>   (chr), VisionPhenotype (chr)

# Get a condensed version
get_individuals(pace_db, full = FALSE)
#> Source: local data frame [2,223 x 6]
#> 
#>    IndividualID     NameOf ProjectName DateOfBirth Sex   Mother
#> 1             1     2Tufts  Santa Rosa  1988-01-01   F       NA
#> 2             2        A-1  Santa Rosa          NA   M       NA
#> 3             3        Abu  Santa Rosa  2005-04-25   F   Timone
#> 4             6         Al  Santa Rosa  1985-01-01   M       NA
#> 5             7    Alfredo  Santa Rosa          NA   M       NA
#> 6             8      Alien  Santa Rosa  1996-01-02   M KathyLee
#> 7             9        Amy  Santa Rosa  1989-01-01   F       NA
#> 8            10      Amy96  Santa Rosa  1996-01-01   U      Amy
#> 9            11 Babaganouj  Santa Rosa  1992-01-01   M       NA
#> 10           12   BabyFace  Santa Rosa  1991-01-01   M       NA
#> ..          ...        ...         ...         ... ...      ...

# Get any table from PACE without warning messages
# Note that foreign key IDs aren't set!!
get_pace_tbl(pace_db, "tblIndividualDeath")
#> Source: local data frame [1,055 x 7]
#> 
#>    ID IndividualID DateOfDeath CauseOfDeathID SourceOfInformation
#> 1   1            1  1997-03-15              5                  NA
#> 2   2            2  2004-02-13              5                  NA
#> 3   3            6  1997-02-28              5                  NA
#> 4   4            8  2004-02-20              5                  NA
#> 5   5            9  1997-02-24              5                  NA
#> 6   6           10  1997-01-28              5                  NA
#> 7   7           12  1997-03-15              5                  NA
#> 8   8           15  2000-05-23              5                  NA
#> 9   9           16  1997-03-15              5                  NA
#> 10 10           17  1989-04-18              5                  NA
#> .. ..          ...         ...            ...                 ...
#> Variables not shown: DateOfDeathFromCensus (chr), Comments (chr)
```
