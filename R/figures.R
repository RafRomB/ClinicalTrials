# 0. Load libraries ----

library(ggplot2)
library(ggpubr)
library(cowplot)

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



# 2. Figures ----

height <- 11.69
width <- 8.27

## 2.1 Supplementary Figures ----

FDA_conf_matrix <- yardstick::conf_mat(merged_table_test, truth = "manual_eval", estimate = "ensemble") %>% autoplot(type = "heatmap") + # Use tiles for the heatmap cells
  scale_fill_gradient(low = "white", high = "slateblue") +
  labs(title = "Evaluation of LLM performance for FDA approval notifications")

clinitrials_comb_conf_matrix <- yardstick::conf_mat(protocolSection_llm_test_manual_eval, truth = "manual_eval", estimate = "ensemble") %>% autoplot(type = "heatmap") + # Use tiles for the heatmap cells
  scale_fill_gradient(low = "white", high = "slateblue") + labs(title = "Evaluation of LLM performance for single-drug/combination of clinical trials")

WhyStopped_conf_matrix <- yardstick::conf_mat(protocolSection_WhyStopped_lmm_eval, truth = "manual_eval", estimate = "ensemble") %>% autoplot(type = "heatmap") + # Use tiles for the heatmap cells
  scale_fill_gradient(low = "white", high = "slateblue") + labs(title = "Evaluation of LLM performance for WhyStopped labels")

plot_grid(FDA_conf_matrix, NULL, clinitrials_comb_conf_matrix, NULL, WhyStopped_conf_matrix, NULL, ncol = 2, rel_widths = c(0.4,0.6), labels = c("a", "", "b", "", "c", ""))
ggsave("figures/SuppFig01.pdf", height = height, width = 2*width)
