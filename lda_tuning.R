# LDA tuning

library(ldatuning)
library(topicmodels)
library(tidytext)
library(dplyr)
library(textstem)
library(textmineR)
library(future)
library(parallel)



df <- readRDS("data/refinery29_cols_time.rda") %>% 
  mutate(id = row_number())

# Create document term matrix
pre_dtm <- df %>%
  transmute(id = row_number(), text) %>%
  unnest_tokens(word, text, token = 'words')

pre_dtm$word <- lemmatize_words(pre_dtm$word)

pre_dtm_ct <- pre_dtm %>%
  anti_join(stop_words, by='word') %>%
  group_by(id, word) %>% 
  count()

dtm <- cast_dtm(pre_dtm_ct, id, word, n)

# Tune model-- none of this works

mynodes <- c( 'localhost', 'localhost', 'localhost' )
system.time(
  makeCluster( mynodes, type='PSOCK' )
)


system.time(
  cl <- parallel::makePSOCKcluster(3, setup_strategy = "sequential")
)

system.time(
  cl <- future::makeClusterPSOCK(3, outfile = NULL, verbose = TRUE)
)
system.time(
  result <- FindTopicsNumber(
    dtm,
    topics = seq(from = 2, to = 15, by = 1),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 3L,
    verbose = TRUE
  ) 
)
