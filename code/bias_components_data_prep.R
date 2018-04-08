library(tidyverse)
library(gridExtra)
library(forcats)
library(stringr)
library(scales)
library(foreign)


# Note CH3 and CH4 are based on different runs of the procedure that have
# slightly different numbers due to randomization
all_estimates = readRDS("data/est_pos_full.RDS") %>%
  filter(est != "y_bar_pop") %>%
  rename(sample = sample_id) %>%
  filter(est %in% c("y_bar_pop_unconfounded", "y_bar_pop_unconfounded_cs", "y_bar_samp_confounded", "y_bar_samp_unconfounded")) %>%
  group_by(sample, est) %>%
  mutate(draw = 1:n()) %>%
  gather(y_var, value, starts_with("y_")) %>% 
  mutate(value = value * 100) %>%
  spread(est, value) %>%
  mutate(delta_hat_net = y_bar_samp_confounded - y_bar_pop_unconfounded,
         delta_hat_exch = y_bar_samp_confounded - y_bar_samp_unconfounded,
         delta_hat_pos = y_bar_pop_unconfounded_cs - y_bar_pop_unconfounded, 
         delta_hat_comp = y_bar_samp_unconfounded - y_bar_pop_unconfounded_cs, 
         check = delta_hat_exch + delta_hat_pos + delta_hat_comp) %>%
  select(sample, draw, y_var, starts_with("delta_hat")) %>%
  gather(est, value, starts_with("delta")) %>%
  mutate(y_var = factor(y_var, 
                        levels = c("y_always_vote_local",
                                   "y_civic_assoc_yes",
                                   "y_recreational_assoc_yes", 
                                   "y_school_group_yes",
                                   "y_talk_neighbor_weekly",
                                   "y_trust_neighbors_all_most"),
                        labels = c("Vote local",
                                   "Civic assoc",
                                   "Rec assoc",
                                   "Community group",
                                   "Talk neighbors",
                                   "Trust neighbors")),
         est = factor(est, 
                      levels=c("delta_hat_net", "delta_hat_exch", "delta_hat_pos", "delta_hat_comp"),
                      labels=c("Net Bias", "Exchangeability", "Positivity", "Composition")
         ))

y_var_summary = all_estimates %>% group_by(est, y_var, draw) %>%
  summarise(value_mean = mean(value)) %>%
  summarise(avg_bias_post_median = median(value_mean),
            avg_bias_post_ucl95 = quantile(value_mean, 0.975),
            avg_bias_post_lcl95 = quantile(value_mean, 0.025),
            avg_abs_bias_post_median = median(abs(value_mean))) %>%
  ungroup() %>%
  left_join( filter(., est == "Net Bias") %>%
               select(y_var, avg_net_bias = avg_bias_post_median)) %>% 
  mutate(y_var = fct_reorder(y_var, -avg_net_bias))

sample_summary = all_estimates %>% group_by(est, sample, draw) %>%
  #filter(y_var != "Vote local") %>%
  #filter(sample != "I", sample != "MTk") %>%
  summarise(avg_abs_bias = mean(abs(value))) %>%
  summarise(avg_abs_bias_post_median = median(avg_abs_bias),
            avg_abs_bias_post_ucl95 = quantile(avg_abs_bias, 0.975),
            avg_abs_bias_post_lcl95 = quantile(avg_abs_bias, 0.025)) %>%
  ungroup() %>%
  left_join( filter(., est == "Net Bias") %>%
               select(sample, avg_abs_net_bias = avg_abs_bias_post_median)) %>%
  mutate(sample = fct_reorder(sample, -avg_abs_net_bias))


common_support = readRDS("data/common_support.RDS") %>% 
  select(sample_id, pct_common_support) %>%
  mutate(pct_common_support = (1 - pct_common_support) * 100) %>%
  rename(sample = sample_id) %>%
  group_by(sample) %>%
  summarise(pct_common_support_ucl = quantile(pct_common_support, 0.975),
            pct_common_support_lcl = quantile(pct_common_support, 0.025), 
            pct_common_support = median(pct_common_support))


### Summaries of all individual estimates
estimates = all_estimates %>%
  rename(estimate = est) %>%
  group_by(y_var, sample, estimate) %>%
  summarise(post_median = median(value),
            post_mean = mean(value),
            ucl_50 = quantile(value, 0.75),
            lcl_50 = quantile(value, 0.25),
            ucl_95 = quantile(value, 0.975),
            lcl_95 = quantile(value, 0.025)) %>%
  ungroup() %>%
  mutate(y_var_label = y_var,
         y_var_long_label = factor(y_var, 
                                   levels = c("Vote local",
                                              "Civic assoc",
                                              "Rec assoc",
                                              "Community group",
                                              "Talk neighbors",
                                              "Trust neighbors"),
                                   labels = c("Always vote in local elections",
                                              "Participated in civic association",
                                              "Participated in rececreational association",
                                              "Participated in school group",
                                              "Talk to neighbors weekly",
                                              "Trust all/most neighbors"))) %>%
  
  left_join(sample_summary %>% 
              filter(est == "Net Bias") %>% 
              select(sample, sample_avg_abs_net_bias = avg_abs_bias_post_median)) %>%
  left_join(y_var_summary %>% 
              filter(est == "Net Bias") %>% 
              select(y_var, y_avg_abs_net_bias = avg_abs_bias_post_median)) %>% 
  mutate(sample_ordered = fct_reorder(sample, -sample_avg_abs_net_bias),
         y_var_ordered = fct_reorder(y_var, -y_avg_abs_net_bias))


## Global ggplot2 settings
mytheme = theme_bw() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.margin = margin(t=-.3, unit="cm"),
        #panel.grid = element_blank(),
        axis.title = element_text(size=9),
        axis.text = element_text(size = 7),
        strip.text = element_text(size=8)) 

lookup = estimates %>% 
  mutate(post_median = round(post_median, 1)) %>%
  split(.$sample) %>% map(~split(., .$y_var) %>% map(~select(., estimate, post_median) %>% spread(estimate, post_median)))





