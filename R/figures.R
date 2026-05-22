# 0. Load libraries ----

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(openxlsx2)
library(ggpubr)

# 1. Load data ----

## 1.1 Materials ----

FDA_llm_curated <- openxlsx2::read_xlsx("results/FDA/approval_notifications_llm_results_ensembled_260212.xlsx")
merged_table_test <- openxlsx2::read_xlsx("results/FDA/approval_notifications_test.xlsx")
merged_table_test <- merged_table_test %>% left_join(FDA_llm_curated %>% select(ID, qwen3_14b, deepseek_r1_8b, phi4, ensemble), by = "ID")
merged_table_test <- merged_table_test %>% mutate(manual_eval = as.factor(manual_eval),
                                                  qwen3_14b = as.factor(qwen3_14b),
                                                  deepseek_r1_8b = as.factor(deepseek_r1_8b),
                                                  phi4 = as.factor(phi4),
                                                  ensemble = as.factor(ensemble))


protocolSection_llm <- read_xlsx("results/ClinicalTrials/protocolSection_251230_llm_ensemble.xlsx")
protocolSection_llm_test_manual_eval <- read_xlsx("results/ClinicalTrials/protocolSection_251230_llm_test_manual_eval_260216.xlsx")
protocolSection_llm_test_manual_eval <- protocolSection_llm_test_manual_eval %>% left_join(protocolSection_llm %>% select(nctId, qwen3_8b, deepseek_r1_8b, phi4, ensemble), by = "nctId")
protocolSection_llm_test_manual_eval <- protocolSection_llm_test_manual_eval %>% mutate(manual_eval = as.factor(manual_eval),
                                                                                        qwen3_8b = as.factor(qwen3_8b),
                                                                                        deepseek_r1_8b = as.factor(deepseek_r1_8b),
                                                                                        phi4 = as.factor(phi4),
                                                                                        ensemble = as.factor(ensemble))



protocolSection_WhyStopped_llm <- read_xlsx("results/ClinicalTrials/protocolSection_WhyStopped_llm_reviewed.xlsx")
protocolSection_WhyStopped_lmm_eval <- read_xlsx("results/ClinicalTrials/protocolSection_WhyStopped_llm_eval_260217.xlsx")
protocolSection_WhyStopped_lmm_eval <- protocolSection_WhyStopped_lmm_eval %>% left_join(protocolSection_WhyStopped_llm %>% select(nctId, qwen3_14b, deepseek_r1_8b, phi4, ensemble), by = "nctId")
protocolSection_WhyStopped_lmm_eval <- protocolSection_WhyStopped_lmm_eval %>% mutate(manual_eval = as.factor(manual_eval),
                                                                                      qwen3_14b = as.factor(qwen3_14b),
                                                                                      deepseek_r1_8b = as.factor(deepseek_r1_8b),
                                                                                      phi4 = as.factor(phi4),
                                                                                      ensemble = as.factor(ensemble))


eval_metrics <- function(data, y_true, y_pred) {
  eval_metrics <- tibble(
    Accuracy = data %>% yardstick::precision(truth = .data[[y_true]], estimate = .data[[y_pred]], estimator = "macro") %>% pull(.estimate), 
    Sensitivity = data %>% yardstick::sensitivity(truth = .data[[y_true]], estimate = .data[[y_pred]], estimator = "macro") %>% pull(.estimate), 
    Precision = data %>% yardstick::precision(truth = .data[[y_true]], estimate = .data[[y_pred]], estimator = "macro") %>% pull(.estimate), 
    F1 = data %>% yardstick::f_meas(
      truth = .data[[y_true]],
      estimate = .data[[y_pred]],
      event_level = "second",
      estimator = "macro"
    ) %>% pull(.estimate),
    MCC = data %>% yardstick::mcc(truth = .data[[y_true]], estimate = .data[[y_pred]]) %>% pull(.estimate),
  )
  return(eval_metrics)
}


# 2. Figures ----

height <- 11.69
width <- 8.27

## 2.1 Supplementary Figures ----


clinitrials_comb_conf_matrix <- yardstick::conf_mat(protocolSection_llm_test_manual_eval, truth = "manual_eval", estimate = "ensemble") %>% autoplot(type = "heatmap") + # Use tiles for the heatmap cells
  scale_fill_gradient(low = "white", high = "slateblue") + labs(title = "Evaluation of LLM performance for single-drug/combination of clinical trials")

bind_rows(
  protocolSection_llm_test_manual_eval %>% eval_metrics(y_true = "manual_eval", y_pred = "qwen3_8b"),
  protocolSection_llm_test_manual_eval %>% eval_metrics(y_true = "manual_eval", y_pred = "deepseek_r1_8b"),
  protocolSection_llm_test_manual_eval %>% eval_metrics(y_true = "manual_eval", y_pred = "phi4"),
  protocolSection_llm_test_manual_eval %>% eval_metrics(y_true = "manual_eval", y_pred = "ensemble")
) %>% round(2) %>%
  mutate(
    Model = c("Qwen3:8b", "Deepseek-r1:8b", "phi4", "ensemble"),
    .before = Accuracy) %>%
  ggtexttable(rows = NULL, theme = ttheme("light", base_size = 16)) %>% 
  tab_add_title(text = "LLM performance for Fingle-drug/combination\nof clinical trials", face = "bold", size = 18) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "mm")) -> clintrials_perf

WhyStopped_conf_matrix <- yardstick::conf_mat(protocolSection_WhyStopped_lmm_eval, truth = "manual_eval", estimate = "ensemble") %>% autoplot(type = "heatmap") + # Use tiles for the heatmap cells
  scale_fill_gradient(low = "white", high = "slateblue") + labs(title = "Evaluation of LLM performance for WhyStopped labels")

bind_rows(
  protocolSection_WhyStopped_lmm_eval %>% eval_metrics(y_true = "manual_eval", y_pred = "qwen3_14b"),
  protocolSection_WhyStopped_lmm_eval %>% eval_metrics(y_true = "manual_eval", y_pred = "deepseek_r1_8b"),
  protocolSection_WhyStopped_lmm_eval %>% eval_metrics(y_true = "manual_eval", y_pred = "phi4"),
  protocolSection_WhyStopped_lmm_eval %>% eval_metrics(y_true = "manual_eval", y_pred = "ensemble")
) %>% round(2) %>%
  mutate(
    Model = c("Qwen3:14b", "Deepseek-r1:8b", "phi4", "ensemble"),
    .before = Accuracy) %>%
  ggtexttable(rows = NULL, theme = ttheme("light", base_size = 16)) %>% 
  tab_add_title(text = "LLM performance for for WhyStopped labels", face = "bold", size = 18) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "mm")) -> WhyStopped_perf

FDA_conf_matrix <- yardstick::conf_mat(merged_table_test, truth = "manual_eval", estimate = "ensemble") %>% autoplot(type = "heatmap") + # Use tiles for the heatmap cells
  scale_fill_gradient(low = "white", high = "slateblue") +
  labs(title = "Evaluation of LLM performance for FDA approval notifications")

bind_rows(
  merged_table_test %>% eval_metrics(y_true = "manual_eval", y_pred = "qwen3_14b"),
  merged_table_test %>% eval_metrics(y_true = "manual_eval", y_pred = "deepseek_r1_8b"),
  merged_table_test %>% eval_metrics(y_true = "manual_eval", y_pred = "phi4"),
  merged_table_test %>% eval_metrics(y_true = "manual_eval", y_pred = "ensemble")
) %>% round(2) %>%
  mutate(
    Model = c("Qwen3:14b", "Deepseek-r1:8b", "phi4", "ensemble"),
    .before = Accuracy) %>%
  ggtexttable(rows = NULL, theme = ttheme("light", base_size = 16)) %>% 
  tab_add_title(text = "LLM performance for FDA approval notifications", face = "bold", size = 18) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "mm")) -> FDA_perf




plot_grid(clinitrials_comb_conf_matrix, clintrials_perf, WhyStopped_conf_matrix, WhyStopped_perf, FDA_conf_matrix, FDA_perf , ncol = 2, rel_widths = c(0.4,0.6), labels = c("a", "", "b", "", "c", ""))
ggsave("figures/SuppFig01.pdf", height = height, width = 1.66*width)
