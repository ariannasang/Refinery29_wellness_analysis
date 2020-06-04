# Data Cleaning -- Feel Good Diaries
source('necessary_libs.R')

df <- readRDS('data/refinery29.rda')

df <- df %>%
  transmute(Titles, Links, raw_txt = Text)

# Create new columns 
38-sum(str_detect(df$raw_txt, 'Age:')) # We expect 1 NA
cat_age <- str_extract(df$raw_txt, 'Age:.[[:digit:]]+')
df$age <- str_extract(cat_age, '[[:digit:]]+') %>%
  as.numeric()

38-sum(str_detect(df$raw_txt, 'Salary:')) # We expect 5 NAs
cat_salary <- str_extract(df$raw_txt, 'Salary:.{0,20}\\$[[:digit:]]+,[[:digit:]]+')
cat_salary <- str_replace_all(cat_salary, '[[:punct:]]|[[:alpha:]]', '')
df$salary <- str_extract(cat_salary,'[[:digit:]]+') %>%
  as.numeric()

38-sum(str_detect(df$raw_txt, 'Occupation:')) # We expect 0 NAs
cat_occ <- str_extract(df$raw_txt, 'Occupation:.{50}')
cat_occ <- ifelse(str_detect(cat_occ, 'Salary:.+|Day.+|,.+|and.+|\\(.+'),
                  str_replace(cat_occ, 'Salary:.+|Day.+|,.+|and.+|\\(.+', ''),
                  cat_occ)
df$occupation <- str_replace(cat_occ, 'Occupation:.','')
df$occupation[29] <- 'App Developer'
df$occupation[8] <- 'Writer'

38-sum(str_detect(df$raw_txt, 'Location:')) # We expect 0 NAs
cat_loc <- str_extract(df$raw_txt, 'Location:.{0,30}')
cat_loc <- ifelse(str_detect(cat_loc, 'Oc.*|Day.+|Sal.+'), 
                  str_replace(cat_loc, 'Oc.*|Day.+|Sal.+', ''), 
                  cat_loc)
df$location <- str_replace(cat_loc, 'Location:.', "")

38-sum(str_detect(df$raw_txt, 'Weekly.[tT]otal:')) # We expect 3 NAs
cat_weekTotal <- str_extract(df$raw_txt, 'Weekly.[tT]otal:.{1,10}[[:digit:]]+')
cat_weekTotal <- str_extract(cat_weekTotal, '[0-9\\,.]+') 
df$weekTotal <- str_replace(cat_weekTotal, ',', '') %>%
  as.numeric()
df$weekTotal[38] <- 833.39 # Recovered 1 NA

# Clean text
combinations <- c('([a-z])([A-Z])','([0-9])([A-Z])','([A-z])([0-9])','([!.;:,])([A-z0-9])',
                  '([A-z0-9])([!.;,])')

for (c in combinations) {
  df$raw_txt <- unlist(lapply(df$raw_txt, str_replace_all, c, '\\1 \\2'))
}

template_words <- c('Today:', 'Age:', 'Location:', 'Occupation:', 'Salary:', 'Day One',
                    'Day Two', 'Day Three', 'Day Four', 'Day Five', 'Day Six', 'Day Seven', 
                    'a . m . ', 'Daily Total:', 'p . m . ', 'Weekly Total:', 'Reflection:')

for (w in template_words) {
  df$raw_txt <- unlist(lapply(df$raw_txt, str_replace_all, w, ' '))
}

df$raw_txt <- unlist(lapply(df$raw_txt, str_replace_all, '[[:punct:]]|\\d|\\$', ' '))
df$text <- unlist(lapply(df$raw_txt, str_replace_all, ' +', ' '))

# Change structure
saveRDS(df, 'data/refinery29_cols.rda')

