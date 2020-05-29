# Data Collection-- Feel Good Diaries

library(rvest)
library(dplyr)
library(stringr)


refinery29_link <- 'https://www.refinery29.com/en-us/feel-good-diaries?utm_source=facebook.com&utm_medium=adsales&utm_campaign=ES_VenusVeraBradley/Venus&fbclid=IwAR0dbdfif3nApRxN4nrqJI-e7I0wfNaGJliSVc5-hg7n-hjatc3_hrxyo_M'
refinery29 <- read_html(refinery29_link)

# Collect titles
titles <- refinery29 %>%
  html_nodes('.title span') %>%
  html_text() 

# Collect links
raw_links <-refinery29 %>%
  html_nodes('a') %>%
  html_attr('href') 

raw_page2 <- raw_links[38]
raw_links <- raw_links[7:37]

# Collect titles from page2
raw_page2 <- paste("https://refinery29.com", raw_page2, collapse="")
page2_link <- sub(" ",'', raw_page2)
page2 <- read_html(page2_link)

titles2 <- page2 %>%
  html_nodes('.title span') %>%
  html_text()
titles2 <- titles2[2:8]

# Collect links from page2
raw_links2 <- page2 %>%
  html_nodes('a') %>%
  html_attr('href') 
raw_links2 <- raw_links2[8:14]

# Concatenate links and titles
all_raw_links <- c(raw_links, raw_links2)
all_raw_links <- paste('https://refinery29.com', all_raw_links)
all_links <- unlist(lapply(all_raw_links, str_replace_all, ' ', ''))
all_titles <- c(titles, titles2)

# Get text from each article
get_text <- function(link) {
  html <- read_html(link)
  txt <- html %>% 
    html_nodes('.section-text') %>%
    html_text() 
  
  return(txt) 
}
raw_text <- lapply(all_links, get_text)
text <- lapply(raw_text, paste, collapse = '')

# Make into dataframe
a <- all_titles %>%
  as.data.frame() 
names(a) <- c('Titles')
b <- all_links %>% 
  as.data.frame()
names(b) <- c('Links')

df <- cbind(a,b)
df$Text <- unlist(text)

write.csv(df, 'refinery29.csv')
saveRDS(df, 'refinery29.rda')

