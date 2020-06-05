# LDA topic modeling -- document topic probabilities

source('necessary_libs.R')

lda.14 <- readRDS('lda_model_14.rda')

documents <- tidy(lda.14, matrix = 'gamma')

date_df <- readRDS('data/refinery29_cols_time.rda') %>% 
  transmute(document = row_number(), 
            Pub_date)
doc_date <- merge(documents, date_df, by = 'document')

topic_names <- c('Subscription Services',
                 'Quality Time w Partner',
                 'Fun Leisure Activities',
                 'School/Community Projects',
                 'Routine & Structure',
                 'City Dorm Life', 
                 'Healthy Diet & Exercise', 
                 'Community Engagement',
                 'Affordable Self-Care',
                 'COVID-19 Pandemic',
                 'Luxurious Spa Treatments',
                 'Social Activities & Going Out',
                 'Travel & Getaways',
                 'Family Life')

doc_date %>%
  mutate(topic = factor(topic, 
                        levels = c(1:14),
                        labels = topic_names)) %>%
  group_by(topic) %>%
  mutate(score = sum(gamma)) %>%
  distinct(score) %>%
  arrange(desc(score)) %>%
  mutate(pct_score = score/38) %>%
  ggplot(aes(x = reorder(topic, -score), y = pct_score, fill = reorder(topic,-score))) +
  geom_col() + 
  labs(title = 'Routine & Structure is addressed overwhelming more than all other topics,',
       subtitle = 'and makes up about 60% of the entire series.', 
       x = 'Topics',
       y = 'amount addressed in the entire series (%)', 
       fill= 'Topic') + 
  theme(plot.background = element_rect(fill = "lavenderblush2"),
        panel.background = element_rect(fill = 'lavenderblush2'),
        legend.background = element_rect(fil = 'lavenderblush3'),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

doc_date %>%
  mutate(topic = factor(topic, 
                        levels = c(1:14),
                        labels = topic_names)) %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  distinct(topic) %>% 
  ggplot(aes(x = topic, fill = topic)) + 
  geom_bar() + 
  labs(title = 'Routine & Structure is the top topic in 35/38 articles.',
       subtitle = 'The 10 omitted topics were never a top topic.', 
       y= 'No. of times a topic is a top topic in an article',
       x = 'Topic') +
  theme(plot.background = element_rect(fill = "lavenderblush2"),
        panel.background = element_rect(fill = "lavenderblush2"),
        legend.position = "none")



doc_date <- doc_date %>%
  mutate(topic = factor(topic, 
                        levels = c(1:14),
                        labels = topic_names))

# Time series analysis 
doc_date %>%
  arrange(Pub_date) %>%
  group_by(topic) %>%
  ggplot(aes(x = Pub_date, y = gamma, color = topic)) +
  geom_line() + 
  labs(title = 'Topics Discussed Over Time',
       x = 'Publish Date',
       y = 'Gamma (probability topic is discussed)', 
       subtitle = 'Most topics (except Routine & Structure) seem to be unpredictably discussed over time.',
       color = 'Topics') + 
  theme(plot.background = element_rect(fill = "lavenderblush2"),
        panel.background = element_rect(fill = "lavenderblush2"),
        legend.background = element_rect(fill = "lavenderblush3"))

doc_date %>%
  arrange(Pub_date) %>%
  group_by(topic) %>%
  ggplot(aes(x = Pub_date, y = gamma, color = topic)) +
  geom_line() + 
  labs(title = 'Topics Discussed Over Time',
       x = 'Publish Date',
       y = 'Gamma (probability topic is discussed)', 
       subtitle = 'Most topics (except Routine & Structure) seem to be unpredictably discussed over time.',
       color = 'Topics') + 
  theme(plot.background = element_rect(fill = "lavenderblush2"),
        panel.background = element_rect(fill = "lavenderblush2"),
        legend.background = element_rect(fill = "lavenderblush3"))


doc_date %>%
  arrange(Pub_date) %>%
  group_by(topic) %>%
  mutate(acc_gamma = cumsum(gamma)) %>%
  ggplot(aes(x = Pub_date, y = acc_gamma, color = topic)) +
  geom_line()


topic_over_time_plot <- function(x) {
  doc_date %>%
    arrange(Pub_date) %>%
    filter(topic == x) %>%
    ggplot(aes(x = Pub_date, y = gamma, color = topic)) +
    geom_line()
}

acc_topic_over_time_plot <- function(x) {
  doc_date %>%
    arrange(Pub_date) %>%
    filter(topic == x) %>%
    mutate(acc_gamma = cumsum(gamma)) %>%
    ggplot(aes(x = Pub_date, y = acc_gamma, color = topic)) +
    geom_line()
}

topic_over_time_plot('COVID-19 Pandemic')
acc_topic_over_time_plot('COVID-19 Pandemic')

topic_over_time_plot('Routine & Structure')
acc_topic_over_time_plot('Routine & Structure')

topic_over_time_plot('Travel & Getaways')
acc_topic_over_time_plot('Travel & Getaways')


topic_over_time_plot('Luxurious Spa Treatments')
topic_over_time_plot('Fun Leisure Activities')
topic_over_time_plot('Family Life')

doc_date %>%
  filter(topic == 'Family Life' |
           topic == 'COVID-19 Pandemic' |
           topic == 'Routine & Structure') %>%
  arrange(Pub_date) %>%
  group_by(topic) %>%
  ggplot(aes(x = Pub_date, y = gamma, color = topic)) +
  geom_line()

topics.14 %>%
  filter(topic == 10) %>%
  arrange(desc(beta)) %>% 
  mutate(rank = row_number()) %>% View()
