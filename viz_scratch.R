library(tidyverse)

source("ERR_scratch.R")

g <- 4
g_max <- 5

((2^g) - 1 ) / (2^g_max)

n <- 10
docs <- runif(n)
p <- .9

res <- tibble(n = 1:10) %>% 
  rowwise() %>% 
  mutate(
    dcg = dcg(n, docs),
    err = err(n, docs),
    sat = prob_satisfied(n, docs),
    rbp = rbp(n, docs, p)
  ) %>% 
  pivot_longer(-n, "measure")

ggplot(res, aes(n, value, color = measure)) +
  geom_path() +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

