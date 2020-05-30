# Create cores

library(parallel)
library(future)

# Test 1
cl <- future::makeClusterPSOCK(3, outfile = NULL, verbose = TRUE)
parSapply(cl, 1:5, sqrt)

parLapply(cl, 2:4,
          function(exponent)
            2^exponent)
