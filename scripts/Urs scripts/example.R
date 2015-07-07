Sys.setenv(TZ = 'UTC')
list.of.packages <- list("plyr", "lubridate", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(unlist(new.packages))
lapply(list.of.packages, require, character.only = T)

system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", dbname = "monkey", password = NULL)


AgeSexClass <- getAgeSexClass(pace_db)

Individual <- getIndividual(pace_db)
