# Collect the timestamps
library(rvest)
library(dplyr)
library(lubridate)

# Merge files to create complete df
date_df <- read.csv('data/published_date.csv') %>% 
  select(-X)
incomplete_df <- readRDS('data/refinery29_cols.rda') 
df <- merge(date_df, incomplete_df, by = c('Titles', 'Links'))

# Correct date
df$Pub_date[7] <- '5/26/20'
df$Pub_date[31] <- '5/5/20'
df$Pub_date <- df$Pub_date %>% as.Date(format = '%m/%d/%y') 

saveRDS(df, 'data/refinery29_cols_time.rda')





