Sys.setenv(TZ = 'UTC')
list.of.packages <- list("devtools", "roxygen2", "microbenchmark", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(unlist(new.packages))
lapply(list.of.packages, require, character.only = T)

# Install the latest version of the package
# Can skip if current
devtools::install_github("camposfa/paceR")

# Load the package
library(paceR)

# Load data manupulation packages, i.e., the Hadleyverse
load_pace_packages()

# Create the SSH tunnel (modify for your connection)
# If corrupt, in terminal:
# ps -ax | grep ssh
# kill <pid>
system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", dbname = "monkey", password = NULL)

# Get the individuals table
ind <- get_individuals(pace_db)

# Get a condensed version
ind <- get_individuals(pace_db, full = FALSE)

# Get any table from PACE without warning messages
# Note that foreign key IDs aren't set!!
deaths <- get_pace_tbl(pace_db, "tblIndividualDeath")

bio <- get_biography(pace_db)

get_infanticide_risk(pace_db)



# ---- Benchmarking_Views_VS_R_Functions ----------------------------------

# Q1: Is there a speed benefit to using views rather than joining in R?
res1 <- microbenchmark(

  get_individuals(pace_db),
  get_pace_tbl(pace_db, "paceR_Individual"),

  get_monthly_census(pace_db),
  get_pace_tbl(pace_db, "paceR_CensusMonthly"),

  times = 20
)

print(res1)
autoplot(res1)
# A1: Yes.
# For "Individuals" it's about 7 times faster to get the view than to do the joins in R.
# For "CensusMonthly" it's about 2 times faster to get the view than to do the joins in R.




# Q2: Does chaining to a view require executing the entire view?
get_pace_tbl(pace_db, "paceR_Individual", collect = FALSE) %>%
  filter(Project == "SR") %>%
  explain()
# A2: No, it simply adds to the query




# Q3: Does chaining a filter to a view speed it up or slow it down?
res2 <- microbenchmark(

  get_pace_tbl(pace_db, "paceR_CensusMonthly"),

  get_pace_tbl(pace_db, "paceR_CensusMonthly") %>%
    filter(GroupCode == "CP"),

  get_pace_tbl(pace_db, "paceR_CensusMonthly", collect = FALSE) %>%
    filter(GroupCode == "CP") %>%
    collect(),

  times = 50
)
print(res2)
autoplot(res2)
# A3: Filtering provides a major speed improvement when collection is done later.
# If collection is done first, filtering doesn't affect the speed much (in this case).
# In other words, it's faster to do the filtering in the database rather than in R.



