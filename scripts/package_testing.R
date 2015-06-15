Sys.setenv(TZ = 'UTC')
list.of.packages <- list("paceR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(unlist(new.packages))
lapply(list.of.packages, require, character.only = T)

load_pace_packages()

system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", dbname = "monkey", password = NULL)


