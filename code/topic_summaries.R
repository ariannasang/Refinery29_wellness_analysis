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





doc_date %>%
  mutate(topic = factor(topic, 
                        levels = seq(1,8,1),
                        labels = topic_names)) %>%
  filter(topic == 'Family support' | 
           topic == 'Routine & structure') %>%
  group_by(topic) %>%
  arrange(Pub_date) %>%
  mutate(gamma_over_time = cumsum(gamma)) %>%
  ggplot(aes(x = Pub_date, y = gamma, color = as.factor(topic))) +
  geom_line() + 
  labs(title = 'Family support increases during quarantine. Routine and structure
       decreases during quarantine.')

doc_date %>%
  group_by(topic) %>%
  mutate(gamma_over_time = cumsum(gamma))%>% 
  filter(topic ==1 )
ungroup() %>%
  ggplot(aes(x = Pub_date, y = gamma_over_time, color = as.factor(topic))) +
  geom_line()

doc_date %>%
  group_by(topic) %>%
  summarise(tot_gamma = sum(gamma)) %>%
  arrange(desc(tot_gamma))
