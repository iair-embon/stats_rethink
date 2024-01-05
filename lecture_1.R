library(tidyverse)


d <-
  tibble(p1 = 0,
         p2 = rep(1:0, times = c(1, 3)),
         p3 = rep(1:0, times = c(2, 2)),
         p4 = rep(1:0, times = c(3, 1)),
         p5 = 1)

head(d)

d %>% 
  set_names(1:5) %>% 
  mutate(x = 1:4) %>% 
  pivot_longer(-x, names_to = "possibility") %>% 
  mutate(value = value %>% as.character()) %>% 
  
  ggplot(aes(x = x, y = possibility, fill = value)) +
  geom_point(shape = 21, size = 5) +
  scale_fill_manual(values = c("white", "navy")) +
  scale_x_discrete(NULL, breaks = NULL) +
  theme(legend.position = "none")

library(flextable)

tibble(draw    = 1:3,
       marbles = 4) %>% 
  mutate(possibilities = marbles ^ draw) %>% 
  flextable()


(
  d <-
    tibble(position = c((1:4^1) / 4^0, 
                        (1:4^2) / 4^1, 
                        (1:4^3) / 4^2),
           draw     = rep(1:3, times = c(4^1, 4^2, 4^3)),
           fill     = rep(c("b", "w"), times = c(1, 3)) %>% 
             rep(., times = c(4^0 + 4^1 + 4^2)))
)
  


(d <- tibble(toss = c("w", "l", "w", "w", "w", "l", "w", "l", "w")))

(
  d <-
    d %>% 
    mutate(n_trials  = 1:9,
           n_success = cumsum(toss == "w"))
)


############ empieza con el modelo de water land, 2.3.2.1

dbinom(x= 6, size = 9, prob = .5)


tibble(prob = seq(from = 0, to = 1, by = .01)) %>% 
  ggplot(aes(x = prob, y = dbinom(x = 6, size = 9, prob = prob))) +
  geom_line() +
  labs(x = "probability",
       y = "binomial likelihood") +
  theme(panel.grid = element_blank())


#### 2.4.2 grid aproximation

## define grid

p_grid <- seq(from = 0, to = 1, length.out = 100)

# define prior
prior <- rep(1,100)
#prior <- ifelse(p_grid < 0.5, 0 , 1)
#prior <- exp(-5*abs(p_grid - 0.5))

# compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9 , prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior
posterior <- unstd.posterior / sum(unstd.posterior)

# plot posterior
plot(p_grid, posterior, type = 'b', xlab = 'probability of water', 
     ylab = 'posterior probability')
mtext('100 points')


