source("code/bias_components_data_prep.R")

propensities = readRDS("data/propensities.rds") %>%
  mutate(src2 = factor(source == "CPS", levels = c(T, F), labels = c("CPS", "Nonprob")))

samp_mins = 

weights = propensities %>% filter(src2 != "CPS") %>%
  mutate(weight = c(1-pscore)/pscore)


ggplot(propensities, aes(x=pscore, .02*..density.., fill=src2)) + 
  geom_vline(xintercept = 0.5, linetype = "dotted") +
  geom_histogram(alpha = .5, binwidth = .02, position = "identity") + 
  facet_wrap(~sample_id, ncol=2) +
  scale_y_continuous("% of Sample", labels = scales::percent_format()) +
  scale_x_continuous("Probability of inclusion in nonprobability sample") +
  theme_bw()


ggplot(weights, aes(x=weight)) +
  geom_histogram(binwidth=.1) +
  facet_wrap(~sample_id, ncol = 2, scales = "free_x")
