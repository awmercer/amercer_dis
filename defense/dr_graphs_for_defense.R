source("code/dr_estimation_data_prep.R")


# Mean absolute bias by sample

sample_summary = pos_summary %>% 
  group_by(sample_id, est) %>%
  summarise(mean_abs_bias = mean(abs_bias),
            mean_change_vs_unwt = mean(change_abs_bias),
            mean_deff = mean(deff), 
            mean_unwt_abs_bias = mean(abs(unwt_bias)),
            mean_rmse = mean(rmse), 
            mean_change_rmse = mean(change_rmse))

y_var_summary = pos_summary %>% 
  group_by(y_var, est) %>%
  summarise(mean_abs_bias = mean(abs_bias),
            mean_change_vs_unwt = mean(change_abs_bias),
            mean_deff = mean(deff), 
            mean_unwt_abs_bias = mean(abs(unwt_bias)),
            mean_rmse = mean(rmse), 
            mean_change_rmse = mean(change_rmse))


sample_summary %>%
group_by(sample_id) %>%
  mutate(min_val = min(mean_abs_bias)) %>%
  ggplot(aes(x = fct_reorder(sample_id, min_val),
                           y=mean_abs_bias, 
                           color = est, 
                           shape = est)) +
  geom_point(size = 3, stroke = 1.5, alpha = .7) +
  theme_bw() +
  scale_shape_manual(name = "Estimate", values = c(19, 0, 1, 2, 5)) +
  scale_size_manual(name = "Estimate", values = c(1, 1, 1, 1, 1)) +
  scale_color_manual(name = "Estimate", values = est_colors)  +
  coord_flip() +
  theme(legend.position = "bottom", axis.title.y = element_blank()) +
  scale_y_continuous("Mean absolute bias")


  

sample_summary %>%
  filter(est != "Unweighted") %>%
  group_by(sample_id) %>%
  mutate(min_val = min(mean_change_vs_unwt)) %>%
ggplot(aes(x = fct_reorder(sample_id, min_val),
                           y=mean_change_vs_unwt, 
                           color = est, 
                           shape = est)) +
  geom_point(size = 4.5, stroke = 2, alpha = .7) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  scale_shape_manual(name = "Estimate", values = c(0, 1, 2, 5)) +
  scale_size_manual(name = "Estimate", values = c(1, 1, 1, 1, 1)) +
  scale_color_manual(name = "Estimate", values = est_colors[2:5])  +
  coord_flip() +
  scale_y_continuous("Mean change in absolute bias vs. unweighted (percentage points)") +
  theme(legend.position = "bottom", axis.title.y = element_blank(), 
        text = element_text(size = 20))
  

ggsave("defense/change_mean_abs_bias.pdf", width = 13, height=6)

sample_summary %>%
  filter(est != "Unweighted") %>%
  group_by(sample_id) %>%
  mutate(min_val = min(mean_deff)) %>%
  ggplot(aes(x = fct_reorder(sample_id, min_val),
             y=mean_deff, 
             color = est, 
             shape = est)) +
  geom_point(size = 4.5, stroke = 2, alpha = .7) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_bw() +
  scale_shape_manual(name = "Estimate", values = c(0, 1, 2, 5)) +
  scale_size_manual(name = "Estimate", values = c(1, 1, 1, 1, 1)) +
  scale_color_manual(name = "Estimate", values = est_colors[2:5])  +
  coord_flip() +
  theme(legend.position = "bottom", axis.title.y = element_blank(), 
        text = element_text(size = 20)) +
  scale_y_continuous("Mean design effect")

ggsave("defense/design_effect.pdf", width = 13, height=6)




y_var_summary %>%
  filter(est != "Unweighted") %>%
  group_by(y_var) %>%
  mutate(min_val = min(mean_change_vs_unwt)) %>%
  ggplot(aes(x = fct_reorder(y_var, min_val),
             y=mean_change_vs_unwt, 
             color = est, 
             shape = est)) +
  geom_point(size = 4.5, stroke = 2, alpha = .7) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  scale_shape_manual(name = "Estimate", values = c(0, 1, 2, 5)) +
  scale_size_manual(name = "Estimate", values = c(1, 1, 1, 1, 1)) +
  scale_color_manual(name = "Estimate", values = est_colors[2:5])  +
  coord_flip() +
  scale_y_continuous("Mean change in absolute bias vs. unweighted (percentage points)") +
  theme(legend.position = "bottom", axis.title.y = element_blank(), 
        text = element_text(size = 20))
  
y_var_summary %>%
  filter(est != "Unweighted") %>%
  group_by(y_var) %>%
  mutate(min_val = min(mean_deff)) %>%
  ggplot(aes(x = fct_reorder(y_var, min_val),
             y=mean_deff, 
             color = est, 
             shape = est)) +
  geom_point(size = 4.5, stroke = 2, alpha = .7) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_bw() +
  scale_shape_manual(name = "Estimate", values = c(0, 1, 2, 5)) +
  scale_size_manual(name = "Estimate", values = c(1, 1, 1, 1, 1)) +
  scale_color_manual(name = "Estimate", values = est_colors[2:5])  +
  coord_flip() +
  scale_y_continuous("Average design effect") +
  theme(legend.position = "bottom", axis.title.y = element_blank(), 
        text = element_text(size = 20))


y_var_summary %>%
  filter(est != "Unweighted") %>%
  group_by(y_var) %>%
  mutate(min_val = min(mean_change_rmse)) %>%
  ggplot(aes(x = fct_reorder(y_var, min_val),
             y=mean_change_rmse, 
             color = est, 
             shape = est)) +
  geom_point(size = 4.5, stroke = 2, alpha = .7) +
  theme_bw() +
  scale_shape_manual(name = "Estimate", values = c(0, 1, 2, 5)) +
  scale_size_manual(name = "Estimate", values = c(1, 1, 1, 1, 1)) +
  scale_color_manual(name = "Estimate", values = est_colors[2:5])  +
  coord_flip() +
  scale_y_continuous("Mean change in RMSE vs. unweighted (percentage points)") +
  theme(legend.position = "bottom", axis.title.y = element_blank(), 
        text = element_text(size = 20))



