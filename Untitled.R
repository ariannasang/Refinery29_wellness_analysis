library(doParallel)

cl <- makeClusterPSOCK(detectCores(logical = TRUE) - 1) # leave one CPU spare...
registerDoParallel(cl)

clusterEvalQ(cl, {
  library(topicmodels)
})

result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 3L,
  verbose = TRUE
)
