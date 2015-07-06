Sys.setenv(TZ = 'UTC')
list.of.packages <- list("devtools", "roxygen2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(unlist(new.packages))
lapply(list.of.packages, require, character.only = T)

# Install the latest version of the package
# Can skip if current
devtools::install_github("camposfa/paceR")

# Load the package
library(paceR)

# Load data manupulation packages, i.e., the Hadleyverse
load_pace_packages()

# Create the SSH tunnel (modify for your connection)
system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", dbname = "monkey", password = NULL)

# Get the individuals table
ind <- get_individuals(pace_db)

# Get a condensed version
ind <- get_individuals(pace_db, full = FALSE)

# Get any table from PACE without warning messages
# Note that foreign key IDs aren't set!!
deaths <- get_pace_tbl(pace_db, "tblIndividualDeath")

