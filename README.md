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

Getting Data from PACE
----------------------

To get data from PACE, you must create an SSH tunnel. If you have set up your SSH key, you can do it like this:

``` r
system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", dbname = "monkey", password = NULL)
```

It should go without saying, but the above lines are just an example--if copied verbatim, they will **not** work for anyone but me (Fernando). You must first set up the SSH key and then modify these lines for your particular account.

Once you get the connection worked out, you now can pull data from the database.

``` r

# Get the full individuals table
get_individuals(pace_db)
#> Source: local data frame [2,227 x 18]
#> 
#>    ProjectID IndividualID ProjectName PrimateSpecies     NameOf CodeName
#> 1          1            1  Santa Rosa           CCAP     2Tufts     2TUF
#> 2          1            2  Santa Rosa           CCAP        A-1     A-1-
#> 3          1            3  Santa Rosa           CCAP        Abu     ABU-
#> 4          1            6  Santa Rosa           CCAP         Al     AL--
#> 5          1            7  Santa Rosa           CCAP    Alfredo     ALFR
#> 6          1            8  Santa Rosa           CCAP      Alien     ALIE
#> 7          1            9  Santa Rosa           CCAP        Amy     AMY-
#> 8          1           10  Santa Rosa           CCAP      Amy96     AM96
#> 9          1           11  Santa Rosa           CCAP Babaganouj     BABA
#> 10         1           12  Santa Rosa           CCAP   BabyFace     BABY
#> ..       ...          ...         ...            ...        ...      ...
#> Variables not shown: DateOfBirth (date), BirthdateSource (chr), Sex (chr),
#>   Mother (chr), MatrilineID (int), GroupAtBirth (chr), GroupAtBirthCode
#>   (chr), DateOfFirstSighting (date), DayDifference (int),
#>   AgeClassAtFirstSighting (chr), GroupAtFirstSighting (chr),
#>   VisionPhenotype (chr)

# Get a condensed version
get_individuals(pace_db, full = FALSE)
#> Source: local data frame [2,227 x 5]
#> 
#>    IndividualID     NameOf ProjectName DateOfBirth Sex
#> 1             1     2Tufts  Santa Rosa         MDT   F
#> 2             2        A-1  Santa Rosa         MDT   M
#> 3             3        Abu  Santa Rosa         MDT   F
#> 4             6         Al  Santa Rosa         MST   M
#> 5             7    Alfredo  Santa Rosa         MDT   M
#> 6             8      Alien  Santa Rosa         MST   M
#> 7             9        Amy  Santa Rosa         MST   F
#> 8            10      Amy96  Santa Rosa         MST   U
#> 9            11 Babaganouj  Santa Rosa         MST   M
#> 10           12   BabyFace  Santa Rosa         MST   M
#> ..          ...        ...         ...         ... ...

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
