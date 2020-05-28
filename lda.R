# Feel Good Diaries - LDA Topic Modelling

library(dplyr)
library(stringr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(textstem)
library(wesanderson)
library(ggthemes)

df <- readRDS("data/refinery29_cols.rda") 

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

# Set params
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

lda <- LDA(dtm, k = 5, method='Gibbs', control=
             list(nstart=nstart, seed = seed, best=best, burnin = burnin, 
                  iter = iter, thin=thin))


# Word topic probabilities
topics <- tidy(lda, matrix = 'beta')

top_terms <- topics %>%
  mutate(topic = factor(topic,
                        levels = c(1,2,3,4,5),
                        labels = c('Beverages',
                                   'Emotional Reflection',
                                   'Meal Prep', 
                                   'Daily Routine',
                                   'Staying Connected to Family'))) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  labs(title = "Top 5 Topics Discussed in the Feel Good Diaries",
       subtitle = 'with the 10 most-defining words for each topic',
       x = 'words',
       y = 'beta (per topic probability)') + 
  scale_fill_brewer(palette='RdPu')+
  theme(plot.background = element_rect(fill = "lavenderblush2"),
        panel.background = element_rect(fill = 'lavenderblush2'),
        strip.background = element_rect(fil = 'lavenderblush3'))

