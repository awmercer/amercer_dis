library(tidyverse)
library(stringr)
library(forcats)
library(ggrepel)
library(ggalt)

## Global ggplot2 settings
mytheme = theme_bw() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.margin = margin(t=-.3, unit="cm"),
        panel.grid = element_blank(),
        axis.title = element_text(size=9), 
        strip.text = element_text(size=8)) 


# Note CH3 and CH4 are based on different runs of the procedure that have
# slightly different numbers due to randomization
full_pos = readRDS("data/est_pos_full_ch4.RDS")

pop_vals = filter(full_pos, est == "y_bar_pop") %>% select(starts_with("y_")) %>% unique() %>%
  gather(y_var, y_bar_pop)

pos_long = filter(full_pos, est != "y_bar_pop") %>%
  filter(est %in% c("y_bar_samp_bayesboot", "y_bar_propwt", "y_bar_pred", "y_bar_drrbc", "y_bar_drpsc")) %>%
  gather(y_var, value, starts_with("y_")) %>%
  left_join(pop_vals) %>%
    mutate(y_var = factor(y_var, 
                        levels = c("y_always_vote_local", 
                                   "y_civic_assoc_yes",
                                   "y_recreational_assoc_yes",
                                   "y_school_group_yes", 
                                   "y_talk_neighbor_weekly",
                                   "y_trust_neighbors_all_most"
                        ),
                        labels = c("Always vote local",
                                   "Civic assoc",
                                   "Rec/Sports assoc",
                                   "School group",
                                   "Talk neighbors",
                                   "Trust neighbors")
  ),
  est = factor(est, 
               levels = c("y_bar_samp_bayesboot",
                          "y_bar_propwt",
                          "y_bar_pred",
                          "y_bar_drrbc",
                          "y_bar_drpsc"
                          ),
               labels = c("Unweighted",
                          "PW",
                          "OR",
                          "OR-RBC",
                          "OR-PSC")
  ),
  y_bar_pop = y_bar_pop * 100,
  value = value * 100
  ) 


pos_summary = pos_long %>% 
  group_by(sample_id, y_var, est) %>%
  summarise(pop_value = mean(y_bar_pop),
            pos_mean = mean(value),
            pos_sd = sd(value),
            pos_var = var(value),
            bias = mean(value - pop_value),
            rmse = sqrt(mean((value - pop_value)^2)),
            ucl95 = quantile(value, 0.975),
            lcl95 = quantile(value, 0.025),
            int_width = ucl95 - lcl95,
            pr_underest = mean(value < pop_value),
            abs_bias = abs(bias)
  ) %>%
  ungroup() %>%
  # Calculate change in absolute bias & rmse relative to unweighted
  left_join(filter(., est == "Unweighted") %>%
              select(sample_id, y_var, 
                     unwt_pos_mean = pos_mean, 
                     unwt_bias = bias, 
                     unwt_rmse=rmse, 
                     unwt_pos_sd = pos_sd,
                     unwt_pos_var = pos_var)) %>%
  mutate(change_abs_bias = abs(bias) - abs(unwt_bias),
         change_rmse = rmse - unwt_rmse,
         change_pos_sd = pos_sd - unwt_pos_sd,
         change_pos_mean = pos_mean - unwt_pos_mean,
         deff = pos_var/unwt_pos_var
         ) %>%
  # Get rank of each estimator by sample and variable with respect to
  # bias and RMSE
  left_join(filter(., est == "Unweighted") %>%
              select(sample_id, bias) %>%
              group_by(sample_id) %>%
              summarise(samp_avg_abs_bias_unwt = mean(abs(bias)))) %>%
  left_join(filter(., est == "Unweighted") %>%
              select(y_var, bias) %>%
              group_by(y_var) %>%
              summarise(y_var_avg_abs_bias_unwt = mean(abs(bias)))) %>%
  mutate(sample_id_ordered = fct_reorder(sample_id, samp_avg_abs_bias_unwt, mean),
         y_var_ordered = fct_reorder(y_var, y_var_avg_abs_bias_unwt, mean)) 







est_colors = c("#E41A1C", "#FF7F00", "#984EA3", "#4DAF4A", "#377EB8")


