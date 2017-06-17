Sys.setenv(TZ = 'UTC')

library(tidyverse)
library(tidytext)
library(paceR)

system('ssh -f camposf@pacelab.ucalgary.ca -L 3307:localhost:3306 -N')
pace_db <- src_mysql(group = "PACE", user = "camposf", dbname = "monkey", password = NULL)
paceR_db <- src_mysql(group = "PACE", user = "camposf", dbname = "paceR", password = NULL)

ph <- getv_Phenology(paceR_db, project = "SR")


temp <- ph %>%
  select(Comments) %>%
  filter(!is.na(Comments)) %>%
  unnest_tokens(words, Comments)

temp %>%
  count(words, sort = TRUE) %>%
  View()

ph %>%
  select(Comments) %>%
  filter(!is.na(Comments)) %>%
  filter(str_detect(Comments, c("dead", "die", "dyin", "buried", "kill",
                                "muer", "muri", "label"))) %>%
  View()
