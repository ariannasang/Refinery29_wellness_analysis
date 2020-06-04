# Topic modeling - LDA per topic word probabilities

source('code/necessary_libs.R')
source('code/topic_modeling_functions.R')

df <- readRDS('data/refinery29_cols_time.rda')

dtm <- create_dtm_dgc(df)
m <- create_dtm_dgc(df, T)

# Tune models -- Try models between 13-15
LDAtuning_perplexity(dtm, 2, 38, 1) #14 - 23
LDAtuning_metrics(dtm, 2, 38, 1) #8 - 15
LDAtuning_coherence(m, 2, 38, 1) # 13, 20

# Set params (same as tuning functions)
seed <- 1234
burnin <- 180 
iter <- 200
alpha <- .1

# 13 topics 
lda.13<- LDA(dtm, k = 13, method='Gibbs', control=
               list( seed = seed, burnin = burnin, 
                     iter = iter, alpha = alpha))

topics.13 <- tidy(lda.13, matrix = 'beta') 
observe_topics(topics.13, 13) 
avg_maxTopicBeta(topics.13) # 0.0219
med_maxTopicsBeta(topics.13) # 0.0187

# 14 topics - better than 13 -- best number of topics
lda.14<- LDA(dtm, k = 14, method='Gibbs', control=
               list( seed = seed, burnin = burnin, 
                     iter = iter, alpha = alpha))

topics.14 <- tidy(lda.14, matrix = 'beta') 
observe_topics(topics.14, 14) 
avg_maxTopicBeta(topics.14) # 0.0223
med_maxTopicsBeta(topics.14) # 0.0207

# 15 topics - worse than 14
lda.15<- LDA(dtm, k = 15, method='Gibbs', control=
               list( seed = seed, burnin = burnin, 
                     iter = iter, alpha = alpha))

topics.15 <- tidy(lda.15, matrix = 'beta') 
observe_topics(topics.15, 15) 
avg_maxTopicBeta(topics.15) # 0.0218
med_maxTopicsBeta(topics.15) # 0.0204

# Save the tuned model 
saveRDS(lda.14, 'lda_model_14.rda')

# Name the topics
observe_topics(topics.14, 14)
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

pretty_topics(topics.14, 14, topic_names)



