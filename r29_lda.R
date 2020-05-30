library(dplyr)
library(stringr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(textstem)

df <- read.csv("r29data_salary_age_date.csv") %>%
  transmute(Titles, Links, raw_txt = Text, Age = AgeN, 
            Career, Salary = SalaryN, date)

# Clean text and remove redundancies

df$raw_txt2 <- unlist(lapply(df$raw_txt, str_extract, "Today:.+"))
pattern_rm <- "Refinery29 does not test out the services .+"
df$raw_txt2 <- unlist(lapply(df$raw_txt2, str_replace, pattern_rm," "))
df$raw_txt2 <- unlist(lapply(df$raw_txt2, str_replace, '^Today:'," "))

template_words <- c('Location:', 'Occupation:', 'Age:', 'Day One', 
                    'Daily Total:', 'Day Two', 'Day Three', 'Salary:',
                    'Day Four', 'Day Five', 'Day Six', 'Day Seven', 
                    'Weekly Total:', 'Reflection:', 'a.m', 'p.m', 'isn’t', 
                    'it’s','i’ve', 'i’m')

for (w in template_words) {
  df$raw_txt2 <- unlist(lapply(df$raw_txt2,str_replace_all, w, ' '))
}

df$raw_txt2 <- unlist(lapply(df$raw_txt2, str_replace_all, '[[:digit:]]', ''))
df <- df %>% 
  mutate(text = raw_txt2,
         id = row_number()) %>% 
  select(id, text) 


# Create custom stop words and remove
rm <- c('a.m', 'p.m', 'isn’t', 
        'it’s','i’ve', 'i’m', 'time', 'day', 'free', 'day',
        'week', 'feel', 'home','morning','class','friend',
        'total')

for (w in rm) {
  stop_words <- add_row(stop_words, word = w, lexicon = 'custom')
}

# Create document term matrix
pre_dtm_w <- df %>%
  unnest_tokens(word, text, token = 'words') %>%
  anti_join(stop_words) 

# Remove puncuation
pre_dtm_w$word <- unlist(lapply(pre_dtm_w$word, str_replace_all, '[[:punct:]]', ''))

pre_dtm_ct <- pre_dtm_w %>%
  group_by(id, word) %>%
  count() 

pre_dtm <- left_join(pre_dtm_w, pre_dtm_ct, by = c('id', 'word'))

pre_dtm$word <- lemmatize_words(pre_dtm$word) 

dtm <- cast_dtm(pre_dtm, id, word, n)

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

# Remove words from dtm

# View the topics
topics <- tidy(lda, matrix = 'beta')
  
top_terms <- topics %>%
  mutate(topic = factor(topic, 
                        levels = c(1,2,3,4,5),
                        labels = c('Daily Routine',
                                   'Relaxation',
                                   'Morning Routine',
                                   'Busy Family',
                                   'Busy Parent'))) %>%
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
  labs(title = 'The 5 Most Frequent Topics Appearing in the Feel Good Diaries',
       subtitle = 'with the Top 10 Terms Defining each Topic ')


