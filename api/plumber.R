library(plumber)

# 'api.R' is the location of the file shown above
pr("api.R") %>%
  pr_run(host = "0.0.0.0", debug = TRUE, port = 8000, docs = TRUE)
