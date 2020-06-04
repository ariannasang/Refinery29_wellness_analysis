# Topic modeling functions

# Set params
seed <- 1234
burnin <- 180 
iter <- 200
iterations <- 200 
alpha <- .1

create_dtm_dgc <- function(df, dgCMatrix = F) {
  ####################################################################
  # Converts dataframe with text column into a document term matrix  #  
  # or a dgCMatrix.                                                  #
  #                                                                  #                                  
  # Param: df -- a document term matrix                              #
  #        dgCMatrix -- if F (default), returns dtm                  #
  #                     if T returns dgCMatrix                       #
  ####################################################################
  
  # Transform df to pre_dtm format
  pre_dtm <- df %>%
    transmute(id = row_number(), text) %>%
    unnest_tokens(word, text, token = 'words') %>%
    mutate(word = lemmatize_words(word)) %>%
    anti_join(stop_words, by = 'word') 
  
  # Create values column for document term matrix
  pre_dtm_ct <- pre_dtm %>%
    group_by(id, word) %>%
    count() %>%
    rename(nword_in_id = n) 
  
  # dtm format 
  if (dgCMatrix == F){
    obj  <- cast_dtm(pre_dtm_ct, id, word, nword_in_id)
  }
  
  # dgCmatrix format 
  if (dgCMatrix == T) {
    obj <- cast_sparse(pre_dtm_ct, id, word, nword_in_id)
  }
  
  
  return(obj)
}

LDAtuning_metrics <- function(dtm, from = 2, to = 15, by = 1) {
  #################################################################
  # Determines n-topic scores and plot to determine optimal       #
  # number of topics for dtm.                                     #
  #                                                               #                                  
  # Params: dtm -- a document term matrix                         #
  #         from -- min number of topics (default is 2)           #
  #         to -- max number of topics (default is 15)            # 
  #         by -- number of intervals to choose topics            #
  #              (default is 1)                                   #
  #################################################################
  
  result <- FindTopicsNumber(
    dtm,
    topics = seq(from, to, by),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = seed), # changed seed
    mc.cores = 3L,
    verbose = TRUE
  )
  
  FindTopicsNumber_plot(result)
  return(result)
}

LDAtuning_coherence <- function(dgcMatrix, from = 2, to = 15, by = 1){
  
  #################################################################
  # Determines coherence scores and returns a plot to determine   #
  # optimal  number of topics for dgcMatrix                       #
  #                                                               #                                  
  # Params: dgcMatrix -- a dgcMatrix                              #
  #         from -- min number of topics (default is 2)           #
  #         to -- max number of topics (default is 15)            # 
  #         by -- number of intervals to choose topics            #
  #              (default is 1)                                   #
  #################################################################
  
  k_list <- seq(from, to, by)
  
  model_dir <- paste0("models_", 
                      digest::digest(colnames(dgcMatrix), algo = "sha1"))
  model_list <- TmParallelApply(X = k_list, FUN = function(k){
    
    m <- FitLdaModel(dtm = dgcMatrix, 
                     k = k, 
                     iterations = iterations, 
                     burnin = burnin,
                     alpha = alpha,
                     optimize_alpha = TRUE,
                     calc_likelihood = FALSE,
                     calc_coherence = TRUE,
                     calc_r2 = FALSE,
                     cpus = 1, 
                     seed = seed) #-- remove seed
    m$k <- k
    
    m
  }, 
  cpus = 2) 
  
  # Get average coherence for each model
  coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                              coherence = sapply(model_list, 
                                                 function(x) mean(x$coherence)), 
                              stringsAsFactors = FALSE)
  
  
  
  plot(coherence_mat, type = "o", 
       main = 'Coherence scores for Topic Models', 
       xlab = 'No. of Topics',
       ylab = 'Coherence score')
  return(coherence_mat)
}

LDAtuning_perplexity <- function(dtm, from= 2, to= 15, by=1) {
  k_list <- seq(from, to, by) 
  burnin = 180
  iter = 200
  perplexity_df <- data.frame()
  for (i in k_list) {
    
    fitted <- LDA(dtm, k = i, method = "Gibbs",
                  control = list(burnin = burnin, iter = iter) )
    perplexity_df[i,1] <- perplexity(fitted, newdata = dtm)
    
  }
  
  # plot perplexity
   p <- perplexity_df %>%
    mutate(ntopics = row_number()) %>%
    rename(perplexity = V1) %>% 
    filter(!is.na(perplexity))
   
   p %>% 
    ggplot(aes(x = ntopics, y = perplexity)) + 
    geom_line() + 
    labs(title = 'Perplexity per Number of Topics',
         x = 'No. of Topics',
         y = 'Perplexity score') 
  
  return(p)
}


observe_topics <- function(model, ntopics){
  
  top_terms <- model %>%
    group_by(topic) %>%
    top_n(15, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered() + 
    labs(title = sprintf('%s Modeled Topics', ntopics))
  
}

pretty_topics <- function(model, ntopics, topic_names) {
  top_terms <- model %>%
    mutate(topic = factor(topic, 
                          levels= seq(1,ntopics, by =1),
                          labels = topic_names)) %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  mycolors <- colorRampPalette(brewer.pal(9, "RdPu"))(ntopics)
  
  top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered() + 
    labs(title = sprintf("%s Topics Discussed in the Feel Good Diaries", ntopics),
         subtitle = 'with the 10 most-defining words for each topic',
         x = 'words',
         y = 'beta (per topic probability)') + 
    scale_fill_manual(values = mycolors )+
    theme(plot.background = element_rect(fill = "lavenderblush2"),
          panel.background = element_rect(fill = 'lavenderblush2'),
          strip.background = element_rect(fil = 'lavenderblush3'))
  
}

avg_maxTopicBeta <- function(tidy_topics) {
  tidy_topics %>%
    group_by(topic) %>%
    summarise(max_beta = max(beta)) %>%
    summarise(mean(max_beta))
}

med_maxTopicsBeta <- function(tidy_topics) {
  tidy_topics %>%
    group_by(topic) %>%
    summarise(max_beta = max(beta)) %>%
    summarise(median(max_beta))
}
