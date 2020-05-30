# Feel good diaries -- LDA model tuning
library(dplyr)
library(stringr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(textstem)
library(wesanderson)
library(ggthemes)
library(text2vec)
library(textmineR)


df <- readRDS('data/refinery29_cols.rda')

pre_dtm <- df %>%
  transmute(id = row_number(),
            text)
  

pre_dtm$word <- lemmatize_words(pre_dtm$word)

pre_dtm_ct <- pre_dtm %>%
  anti_join(stop_words, by='word') %>%
  group_by(id, word) %>%
  count() 

dtm <- CreateDtm(pre_dtm_ct$word, 
                 doc_names = pre_dtm_ct$id, 
                 ngram_window = c(1, 2))

# Explore the basic frequency
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)
# Eliminate words appearing less than 2 times or in more than half of the
# documents
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
dtm = dtm
