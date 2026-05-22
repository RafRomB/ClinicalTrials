# 0. Load libraries ----

library(tidyverse)
library(cowplot)
library(ggpubr)
library(ggsankey)

library(openxlsx2)

# 1. Load data ----

## 1.1 Studies ----

Studies <- read_xlsx("results/AACT/Studies.xlsx")
successful <- read_xlsx("results/FDA/approval_notifications_llm_results_combinations_final_260218.xlsx")
non_successful <- read_xlsx("results/ClinicalTrials/protocolSection_WhyStopped_llm_reviewed_safety_efficacy.xlsx")

### 1.1.2 Curation ----

# Compare data with data extracted directly from ClinicalTrials.gov API

Studies_API <- read_xlsx("results/ClinicalTrials/Studies_API.xlsx")

tmp_df <- Studies_API %>% select(nct_id, Phase) %>% left_join(Studies %>% select(nct_id, phase), by = "nct_id")
tmp_df[which(tmp_df$Phase != tmp_df$phase),]


Studies <- Studies %>% mutate(
  phase = replace_when(phase, phase == "PHASE1" ~ "PHASE1/PHASE2")
)

# 2. Basic Statistics FDA Approved vs Non Approved ----

Studies <- Studies %>%
  mutate(
    # Recode FDA_Approved (your existing step)
    FDA_Approved = if_else(FDA_Approved, "Yes", "No"),
    FDA_Approved = factor(FDA_Approved, levels = c("Yes", "No")),
    
    # Missingness as signal
    results_posted        = factor(if_else(!is.na(results_first_submitted_date), "Yes", "No"),
                                   levels = c("Yes", "No")),
    # why_stopped_present   = factor(if_else(!is.na(why_stopped), "Yes", "No"),
    #                                levels = c("Yes", "No")),
    
    # Recode 0/1 flags to Yes/No factors so they plot the same way as phase
    across(c(has_dmc, is_fda_regulated_drug, is_fda_regulated_device,
             is_unapproved_device, has_expanded_access, fdaaa801_violation),
           ~ factor(if_else(. == 1, "Yes", "No"), levels = c("Yes", "No")))
  )


## Helpers ----

width <- 10
heigth <- 5.63

missing_to_categorical <- function(data,var,missing_str = "NA"){
  data <- data %>% mutate({{ var }} := replace_na(as.character({{var}}), missing_str))
  return(data)
}




plot_categorical <- function(data, var, var_label = rlang::as_label(enquo(var)), caption = NULL, font_size = 12) {

  n_missing <- sum(is.na(dplyr::pull(data, {{ var }})))
  if (n_missing > 0) message(paste0(var_label, " has ", n_missing, " missing values (excluded from plot)"))

  data %>%
    filter(!is.na({{var}})) %>%
    count(FDA_Approved, {{ var }}) %>%
    group_by(FDA_Approved) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ggplot(aes(x = {{ var }}, y = pct, group = FDA_Approved)) +
    geom_col(aes(fill = FDA_Approved, color = FDA_Approved),
             position = "dodge", alpha = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%", pct)),
              position = position_dodge(width = 0.9), vjust = -0.4, size = 3) +
    scale_fill_brewer(palette = "Set2", name = "FDA Approved") +
    scale_color_brewer(palette = "Set2", name = "FDA Approved") +
    labs(title = paste(var_label, "distribution"),
         x = var_label, y = "% within FDA Approval group",
         caption = caption) +
    theme_minimal_hgrid(font_size = font_size) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.caption = element_text(face = "italic", size = font_size))
}

test_categorical <- function(data, var) {
  tbl <- table(data$FDA_Approved, data[[deparse(substitute(var))]])
  list(method = "Fisher (simulated p)", 
         result = fisher.test(tbl, simulate.p.value = TRUE, B = 10000),
         table  = tbl)
}

plot_forest <- function(fisher_results, var_label = "",
                        sort_by_or = TRUE, show_sig = TRUE, font_size = 12, caption = "default") {
  
  if (caption == "default") {
    caption <- "One-vs-rest comparisons. OR > 1 favors approval."
  }
  
  df <- fisher_results %>%
    # Drop rows where OR or CI is non-finite (can happen with extreme tables)
    filter(is.finite(odds_ratio), is.finite(ci_low), is.finite(ci_high))
  
  if (nrow(df) < nrow(fisher_results)) {
    warning(sprintf("%d level(s) dropped due to non-finite OR/CI",
                    nrow(fisher_results) - nrow(df)))
  }
  
  if (sort_by_or) {
    df <- df %>% arrange(odds_ratio) %>%
      mutate(level = factor(level, levels = level))
  } else {
    df <- df %>% mutate(level = factor(level, levels = level))
  }
  
  # Color points by direction of effect (above/below OR=1)
  df <- df %>% mutate(
    direction = case_when(
      ci_low > 1  ~ "Higher odds",
      ci_high < 1 ~ "Lower odds",
      TRUE        ~ "Not significant"
    ),
    direction = factor(direction,
                       levels = c("Higher odds", "Not significant", "Lower odds"))
  )
  
  p <- ggplot(df, aes(x = odds_ratio, y = level)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(xmin = ci_low, xmax = ci_high, color = direction),
                   width = 0.25, linewidth = 0.7, alpha = 0.8) +
    geom_point(aes(color = direction, fill = direction),
               size = 3, shape = 21, stroke = 0.8, alpha = 0.8) +
    scale_x_log10() +
    scale_color_manual(values = c("Higher odds"     = "#66C2A5",
                                  "Not significant" = "grey60",
                                  "Lower odds"      = "#FC8D62"),
                       drop = FALSE, name = "Effect") +
    scale_fill_manual(values = c("Higher odds"     = "#66C2A5",
                                 "Not significant" = "grey60",
                                 "Lower odds"      = "#FC8D62"),
                      drop = FALSE, name = "Effect") +
    labs(title = if (nzchar(var_label)) paste(var_label, "— odds ratios for FDA approval") else "Odds ratios for FDA approval",
         x = "Odds ratio (log scale, 95% CI)",
         y = NULL,
         caption = caption) +
    theme_minimal_vgrid(font_size = font_size) +
    theme(plot.caption = element_text(face = "italic", size = font_size, hjust = 1))
  
  if (show_sig && "Sig" %in% names(df)) {
    p <- p + geom_text(aes(label = Sig, x = ci_high),
                       hjust = -0.4, size = 6, color = "grey30")
  }
  p
}

pairwise_fisher_by_level <- function(data, var, padj = "BH",
                                     cc = c("always", "if_zero", "none")) {
  cc <- match.arg(cc)
  levels_var <- na.omit(unique(data[[var]]))
  
  res <- map_dfr(levels_var, function(lvl) {
    # Force a 2x2 even if a column would otherwise be missing
    exposure <- factor(data[[var]] == lvl, levels = c(FALSE, TRUE))
    tbl <- table(data$FDA_Approved, exposure)
    
    offset <- switch(cc,
                     always  = 0.5,
                     if_zero = if (any(tbl == 0)) 0.5 else 0,
                     none    = 0
    )
    
    # Rows: FDA_Approved (Yes, No). Cols: exposure (FALSE, TRUE).
    a <- tbl[1, 2] + offset  # Yes, == lvl
    b <- tbl[1, 1] + offset  # Yes, != lvl
    c <- tbl[2, 2] + offset  # No,  == lvl
    d <- tbl[2, 1] + offset  # No,  != lvl
    or_sample <- (a * d) / (b * c)
    
    # 95% Wald CI on log(OR)
    se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
    ci_low    <- exp(log(or_sample) - 1.96 * se_log_or)
    ci_high   <- exp(log(or_sample) + 1.96 * se_log_or)
    
    ft <- fisher.test(tbl)
    
    tibble(level = lvl,
           p_value = ft$p.value,
           odds_ratio = or_sample,
           ci_low = ci_low,
           ci_high = ci_high,
           zero_cell = any(tbl == 0))
  })
  
  res %>%
    mutate(p_adj = p.adjust(p_value, method = padj),
           Sig   = case_when(
             p_adj < 0.0001 ~ "****",
             p_adj < 0.001  ~ "***",
             p_adj < 0.01   ~ "**",
             p_adj < 0.05   ~ "*",
             TRUE           ~ "ns"
           ))
}


plot_numeric <- function(data, var, var_label = rlang::as_label(enquo(var)), log_y = FALSE, font_size = 12,
                         plot_type = c("boxplot", "violin")) {

  plot_type <- match.arg(plot_type)
  n_missing <- sum(is.na(dplyr::pull(data, {{ var }})))
  if (n_missing > 0) message(paste0(var_label, " has ", n_missing, " missing values (excluded from plot)"))

  geom_layer <- if (plot_type == "violin") {
    geom_violin(alpha = 0.5, width = 0.8, quantile.linetype = 1, quantile.linewidth = 1)
  } else {
    geom_boxplot(alpha = 0.5, outlier.alpha = 0.3, width = 0.5)
  }

  p <- data %>%
    filter(!is.na({{ var }})) %>%
    ggplot(aes(x = FDA_Approved, y = {{ var }},
               fill = FDA_Approved, color = FDA_Approved)) +
    geom_layer +
    scale_fill_brewer(palette = "Set2", name = "FDA Approved") +
    scale_color_brewer(palette = "Set2", name = "FDA Approved") +
    labs(title = paste(var_label, "by FDA approval"),
         x = "FDA Approved", y = var_label) +
    theme_cowplot(font_size = font_size)
  if (log_y) p <- p + scale_y_log10()
  p
}



summary_numeric <- function(data, var) {
  data %>%
    filter(!is.na({{ var }})) %>%
    group_by(FDA_Approved) %>%
    summarise(
      n      = n(),
      median = median({{ var }}),
      min = min({{var}}),
      Q1     = quantile({{ var }}, 0.25),
      Q3     = quantile({{ var }}, 0.75),
      max = max({{var}}),
      mean   = mean({{ var }}),
      sd     = sd({{ var }}),
      .groups = "drop"
    )
}

test_numeric <- function(data, var) {
  wilcox.test(rlang::eval_tidy(enquo(var), data) ~ data$FDA_Approved)
}

# 3. Trial Characteristics: Grouped Bar Plots + Fisher's Exact Tests ----

## 3.1 Phase distribution ----

fishersp <- test_categorical(Studies, var = phase)$result$p.value
phase_barp <- plot_categorical(Studies, phase, "Phase", caption = sprintf("Fisher's exact test p=%.4f", fishersp), font_size = 14)

phase_fisher <- pairwise_fisher_by_level(Studies, "phase", cc = "always")
phase_forestp <- plot_forest(phase_fisher, var_label = "Phase", font_size = 14) + coord_cartesian(clip = "off")

plot_grid(phase_barp, phase_forestp, labels = "auto", label_size = 20)
ggsave("figures/phase_distribution.png", dpi = 300, height = 1.5*heigth, width = 1.5*width)

## 3.2 Overall status ----

overall_status_p <-
  plot_categorical(Studies, overall_status, "Overall status") +
    facet_wrap(~FDA_Approved)


## 3.3 Why Stopped ----


non_successful <- non_successful %>% mutate(ensemble = str_to_upper(ensemble),
                                            ensemble = as.factor(ensemble))
why_stopped_p <- 
  non_successful %>%
    count(ensemble) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ggplot(aes(x = ensemble, y = pct)) +
    geom_col(color = "#FC8D62", fill = "#FC8D62",alpha = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%", pct)),
              vjust = -0.4, size = 4) +
    labs(title = "Why study stopped",
         x = "WhyStopped", y = "% within non approved studies") +
    theme_minimal_hgrid(font_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_grid(overall_status_p, why_stopped_p, rel_widths = c(0.8, 0.2), labels = "auto", label_size = 20)
ggsave("figures/why_stopped.png", dpi = 300, height = 1.5*heigth, width = 1.5*width)


## 3.4 Study type ----

plot_categorical(Studies, study_type, "Study type")

## 3.5 Source class ----

fishersp <- test_categorical(Studies, var = source_class)$result$p.value
sourceClass_barp <- plot_categorical(Studies, source_class, "Source class", caption = sprintf("Fisher's exact test p=%.4f", fishersp))

sourceClass_fisher <- pairwise_fisher_by_level(Studies, "source_class", cc = "always")
phase_forestp <- plot_forest(sourceClass_fisher, var_label = "Source class") + coord_cartesian(clip = "off")

plot_grid(sourceClass_barp, phase_forestp, labels = "auto", label_size = 20)
ggsave("figures/source_class.png", dpi = 300, width = 1.5*width, height = 1.5*heigth)


## 3.6 Regulatory flags ----

### has_dmc
Studies <- missing_to_categorical(Studies, has_dmc, "Missing") %>%
  mutate(
    has_dmc = factor(has_dmc, levels = c("Yes", "No", "Missing"))
  )

fishersp <- test_categorical(Studies, var = has_dmc)$result$p.value
hasDmc_barp <- plot_categorical(Studies, has_dmc, "Has DMC", caption = sprintf("Fisher's exact test p=%.4f", fishersp))


hasDmc_fisher <- pairwise_fisher_by_level(Studies, "has_dmc", cc = "always")
hasDmc_forestp <- plot_forest(hasDmc_fisher, var_label = "Has DMC", 
                              caption = "One-vs-rest comparisons. OR > 1 favors approval.\nOR for 'Missing' reflects reporting completeness rather than the underlying flag.") +
  coord_cartesian(clip = "off")

plot_grid(hasDmc_barp, hasDmc_forestp, labels = "auto", label_size = 20)
ggsave("figures/has_dmc.png", dpi = 300, height = 1.5*heigth, width = 1.5*width)

### is_fda_regulated_drug
Studies <- missing_to_categorical(Studies, is_fda_regulated_drug, "Missing") %>%
  mutate(
    is_fda_regulated_drug = factor(is_fda_regulated_drug, levels = c("Yes", "No", "Missing"))
  )

fishersp <- test_categorical(Studies, var = is_fda_regulated_drug)$result$p.value
is_fda_regulated_drug_barp <- plot_categorical(Studies, is_fda_regulated_drug, "FDA-regulated drug", caption = sprintf("Fisher's exact test p=%.4f", fishersp))

is_fda_regulated_drug_fisher <- pairwise_fisher_by_level(Studies, "is_fda_regulated_drug", cc = "always")
is_fda_regulated_drug_forestp <- plot_forest(is_fda_regulated_drug_fisher, var_label = "FDA-regulated drug",
                                             caption = "One-vs-rest comparisons. OR > 1 favors approval.\nOR for 'Missing' reflects reporting completeness rather than the underlying flag.") +
  coord_cartesian(clip = "off")

plot_grid(is_fda_regulated_drug_barp, is_fda_regulated_drug_forestp, labels = "auto", label_size = 20)
ggsave("figures/isFDAregulated.png", dpi = 300, width = 1.5*width, height = 1.5*heigth)


### has_expanded_access
Studies <- missing_to_categorical(Studies, has_expanded_access, "Missing") %>%
  mutate(
    has_expanded_access = factor(has_expanded_access, levels = c("Yes", "No", "Missing"))
  )

fishersp <- test_categorical(Studies, var = has_expanded_access)$result$p.value
has_expanded_access_barp <- plot_categorical(Studies, has_expanded_access, "Expanded access", caption = sprintf("Fisher's exact test p=%.4f", fishersp))

has_expanded_access_fisher <- pairwise_fisher_by_level(Studies, "has_expanded_access", cc = "always")
has_expanded_access_forestp <- plot_forest(has_expanded_access_fisher, var_label = "Expanded access",
                                           caption = "One-vs-rest comparisons. OR > 1 favors approval.\nOR for 'Missing' reflects reporting completeness rather than the underlying flag.") +
  coord_cartesian(clip = "off")

plot_grid(has_expanded_access_barp, has_expanded_access_forestp, labels = "auto", label_size = 20)
ggsave("figures/has_expanded_access.png", dpi = 300, height = 1.5*heigth, width = 1.5*width)

combined_binary_forest <- function(data, vars, var_labels = NULL,
                                      levels_to_show = c("Yes", "Missing")) {
  if (is.null(var_labels)) var_labels <- vars
  names(var_labels) <- vars
  
  map_dfr(vars, function(v) {
    res <- pairwise_fisher_by_level(data, v)
    res %>%
      filter(level %in% levels_to_show) %>%
      mutate(level = paste0(var_labels[v], " — ", level))
  }) %>%
    plot_forest(var_label = "Regulatory information", sort_by_or = TRUE)
}

binary_flags <- c("has_dmc", "is_fda_regulated_drug",
                  "has_expanded_access")
flag_labels <- c("Has DMC", "FDA-regulated drug",
                 "Expanded access")

combined_binary_forest(Studies, binary_flags, flag_labels) + coord_cartesian(clip = "off")
ggsave("figures/regulatory_information.png", dpi = 300, height = 1.5*heigth, width = 1.5*width)


## 3.7 Enrollment ----

wilc_p <- test_numeric(Studies %>% filter(enrollment_type == "ACTUAL"), enrollment)

wilc_p$p.value

enrollment_p <- 
  plot_numeric(Studies %>% filter(enrollment_type == "ACTUAL"), enrollment, var_label = "Actual enrollment", log_y = TRUE) +
    labs(y = "Enrollment count (log scale)",
         caption = sprintf("Wilcoxon rank sum test p=%.4f", wilc_p$p.value)) +
    theme(plot.caption = element_text(face = "italic", size = 12))

enrollment_tbl <- 
  summary_numeric(Studies %>% filter(enrollment_type == "ACTUAL"), enrollment) %>%
    rename("FDA Approved" = FDA_Approved,
           Median = median,
           Min = min,
           Max = max,
           Mean = mean,
           SD = sd) %>% mutate_if(is.numeric, round, 2) %>%
    ggtexttable(rows = NULL, theme = ttheme("light", base_size = 12)) %>% 
    tab_add_title(text = "Enrollment", face = "bold", size = 16) 

enrollment_p_tbl <- 
  plot_grid(enrollment_p, enrollment_tbl, ncol = 1, rel_heights = c(0.8, 0.2))


## 3.8 Number of arms ----

(wilc_p <- test_numeric(Studies, number_of_arms))

n_arms_p <- 
  plot_numeric(Studies, number_of_arms, var_label = "Number of arms") +
    labs(caption = sprintf("Wilcoxon rank sum test p=%.4f", wilc_p$p.value)) +
    theme(plot.caption = element_text(face = "italic", size = 12))

n_arms_tbl <- 
  summary_numeric(Studies, number_of_arms) %>%
    rename("FDA Approved" = FDA_Approved,
           Median = median,
           Min = min,
           Max = max,
           Mean = mean,
           SD = sd) %>% mutate_if(is.numeric, round, 2) %>%
    ggtexttable(rows = NULL, theme = ttheme("light", base_size = 12)) %>% 
    tab_add_title(text = "Number of arms", face = "bold", size = 16) 

n_arms_p_tbl <- 
  plot_grid(n_arms_p, n_arms_tbl, ncol = 1, rel_heights = c(0.8,0.2))


## Categorized number of arms

arms_data <- Studies %>%
  filter(!is.na(number_of_arms)) %>%
  mutate(arms_cat = factor(
    if_else(number_of_arms >= 4, "4+", as.character(number_of_arms)),
    levels = c("1", "2", "3", "4+")
  ))


fishersp <- test_categorical(arms_data, var = arms_cat)$result$p.value
arms_barp <- plot_categorical(arms_data, arms_cat, "Number of arms", caption = sprintf("Fisher's exact test p=%.4f", fishersp)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

arms_fisher <- pairwise_fisher_by_level(arms_data, "arms_cat", cc = "always")
arms_forestp <- plot_forest(arms_fisher, var_label = "Number of arms") + coord_cartesian(clip = "off")

plot_grid(enrollment_p_tbl, n_arms_p_tbl, arms_barp, arms_forestp, nrow = 1, labels = "auto", label_size = 20)
ggsave("figures/enrollment_n_arms.png", dpi = 300, height = 2*heigth, width = 3*width)


## 3.9 Trial duration (start to primary completion) ----

Studies <- missing_to_categorical(Studies, start_date_type, "Missing") %>%
  mutate(
    start_date_type = factor(start_date_type, levels = c("ACTUAL", "ESTIMATED", "Missing"))
  )

fishersp <- test_categorical(Studies, var = start_date_type)$result$p.value
start_date_type_barp <- 
  plot_categorical(Studies, start_date_type, "Start date type", caption = sprintf("Fisher's exact test p=%.4f", fishersp), font_size = 14)

start_date_type_fisher <- pairwise_fisher_by_level(Studies, "start_date_type", cc = "always")
start_date_type_forp <- 
  plot_forest(
    start_date_type_fisher,
    var_label = "Start date type",
    font_size = 14,
    caption = "One-vs-rest comparisons. OR > 1 favors approval.\nOR for 'Missing' reflects reporting completeness rather than the underlying flag."
  ) + coord_cartesian(clip = "off")


Studies <- missing_to_categorical(Studies, primary_completion_date_type, "Missing") %>%
  mutate(
    start_date_type = factor(start_date_type, levels = c("ACTUAL", "ESTIMATED", "Missing"))
  )

fishersp <- test_categorical(Studies, var = primary_completion_date_type)$result$p.value
primary_completion_date_type_barp <- 
  plot_categorical(Studies, primary_completion_date_type, "Primary completion date type", caption = sprintf("Fisher's exact test p=%.4f", fishersp), font_size = 14)
primary_completion_date_type_fisher <- pairwise_fisher_by_level(Studies, "primary_completion_date_type", cc = "always")
primary_completion_date_type_forp <-
  plot_forest(
    primary_completion_date_type_fisher,
    var_label = "Primary completion date type",
    font_size = 14,
    caption = "One-vs-rest comparisons. OR > 1 favors approval.\nOR for 'Missing' reflects reporting completeness rather than the underlying flag."
  ) + coord_cartesian(clip = "off")


plot_grid(
  start_date_type_barp,
  start_date_type_forp,
  primary_completion_date_type_barp,
  primary_completion_date_type_forp,
  ncol = 2,
  labels = "auto", label_size = 20
)
ggsave("figures/start_primary_completion_date_type.png", dpi = 300, height = 2.5*heigth, width = 2.5*width)

Studies <- Studies %>%
  mutate(
    trial_duration_years = as.numeric(primary_completion_date - start_date) / 365.25,
    trial_duration_years = if_else(
      start_date_type == "ACTUAL" & primary_completion_date_type == "ACTUAL",
      trial_duration_years, NA_real_
    )
  )



wilc_p <- test_numeric(Studies, trial_duration_years)

wilc_p$p.value
trial_duration_p <- 
  plot_numeric(Studies, trial_duration_years, var_label = "Trial duration (years)") +
    labs(caption = sprintf("Wilcoxon rank sum test p=%.4f", wilc_p$p.value)) +
    theme(plot.caption = element_text(face = "italic", size = 12))
  
trial_duration_tbl <- 
  summary_numeric(Studies, trial_duration_years) %>%
    rename("FDA Approved" = FDA_Approved,
           Median = median,
           Min = min,
           Max = max,
           Mean = mean,
           SD = sd) %>% mutate_if(is.numeric, round, 2) %>%
    ggtexttable(rows = NULL, theme = ttheme("light", base_size = 12)) %>% 
    tab_add_title(text = "Trial duration (years)", face = "bold", size = 16) 

trial_duration_p_tbl <- plot_grid(trial_duration_p, trial_duration_tbl, ncol = 1, rel_heights = c(0.7,0.3))

wilc_by_phase <- tibble(
  phase = c("PHASE1/PHASE2", "PHASE2", "PHASE2/PHASE3", "PHASE3"),
  p.value = c(
    test_numeric(Studies %>% filter(phase == "PHASE1/PHASE2"), trial_duration_years)$p.value,
    test_numeric(Studies %>% filter(phase == "PHASE2"), trial_duration_years)$p.value,
    test_numeric(Studies %>% filter(phase == "PHASE2/PHASE3"), trial_duration_years)$p.value,
    test_numeric(Studies %>% filter(phase == "PHASE3"), trial_duration_years)$p.value
    )
)

#wilc_by_phase$adj.p.val <- p.adjust(wilc_by_phase$p.value, method = "BH")

wilc_by_phase$label <- sprintf("p=%.4f", wilc_by_phase$p.value)

trial_duration_b_byphase <- 
  plot_numeric(Studies, trial_duration_years, var_label = "Trial duration (years)") +
    labs(caption = "Wilcoxon rank sum test") +
    theme(plot.caption = element_text(face = "italic", size = 12)) +
    facet_wrap(~phase) +
    geom_text(
      data = wilc_by_phase,
      aes(x = Inf, y = -Inf, label = label),
      hjust = 1.05,
      vjust = -0.5,
      size = 4,
      inherit.aes = FALSE, fontface = "italic"
    )

plot_grid(trial_duration_p_tbl, trial_duration_b_byphase, nrow = 1, rel_widths = c(0.4,0.6), labels = "auto", label_size = 20)
ggsave("figures/trial_duration_years.png", dpi = 300, height = 1.5*heigth, width = 1.5*width)

## 3.10 Results posted ----

fishersp <- test_categorical(Studies, var = results_posted)$result$p.value
resultsPosted_barp <- plot_categorical(Studies, results_posted, "Results posted", caption = sprintf("Fisher's exact test p=%.4f", fishersp))


## 3.11 Time from primary completion to results submission ----


Studies <- Studies %>%
  mutate(
    time_to_results_days = as.numeric(results_first_submitted_date - primary_completion_date),
    # Restrict to ACTUAL primary completion
    time_to_results_days = if_else(primary_completion_date_type == "ACTUAL",
                                   time_to_results_days, NA_real_)
  )

Studies %>%
  filter(!is.na(time_to_results_days), time_to_results_days < 0) %>%
  select(nct_id, phase, start_date, primary_completion_date,
         primary_completion_date_type, results_first_submitted_date,
         time_to_results_days, FDA_Approved) %>%
  arrange(time_to_results_days)

# nct_id         phase start_date primary_completion_date primary_completion_date_type results_first_submitted_date time_to_results_days FDA_Approved
# 1 NCT02399085        PHASE2 2016-03-29              2022-11-14                       ACTUAL                   2019-11-27                -1083          Yes
# 2 NCT02340221        PHASE3 2015-04-09              2021-06-29                       ACTUAL                   2018-12-18                 -924           No
# 3 NCT03517449        PHASE3 2018-06-11              2022-03-01                       ACTUAL                   2021-10-19                 -133          Yes
# 4 NCT02784171 PHASE2/PHASE3 2016-11-11              2024-10-11                       ACTUAL                   2024-09-18                  -23          Yes


Studies <- Studies %>%
  mutate(time_to_results_days = if_else(time_to_results_days < 0,
                                        NA_real_, time_to_results_days))



wilc_p <- test_numeric(Studies, time_to_results_days)

wilc_p$p.value

time_to_results_p <- 
  plot_numeric(Studies, time_to_results_days, var_label = "Time to results (days)") +
    labs(caption = sprintf("Wilcoxon rank sum test p=%.4f", wilc_p$p.value)) +
    theme(plot.caption = element_text(face = "italic", size = 12))

time_to_results_tbl <- 
  summary_numeric(Studies, time_to_results_days) %>%
    rename("FDA Approved" = FDA_Approved,
           Median = median,
           Min = min,
           Max = max,
           Mean = mean,
           SD = sd) %>% mutate_if(is.numeric, round, 2) %>%
    ggtexttable(rows = NULL, theme = ttheme("light", base_size = 12)) %>% 
    tab_add_title(text = "Time to results (days)", face = "bold", size = 16) 

time_to_results_p_tbl <- plot_grid(time_to_results_p, time_to_results_tbl, ncol = 1, rel_heights = c(0.7,0.3))

results_time_to_results <- plot_grid(resultsPosted_barp, time_to_results_p_tbl, ncol = 1, labels = "auto", label_size = 20)


wilc_by_phase <- tibble(
  phase = c("PHASE1/PHASE2", "PHASE2", "PHASE2/PHASE3", "PHASE3"),
  p.value = c(
    test_numeric(Studies %>% filter(phase == "PHASE1/PHASE2"), time_to_results_days)$p.value,
    test_numeric(Studies %>% filter(phase == "PHASE2"), time_to_results_days)$p.value,
    test_numeric(Studies %>% filter(phase == "PHASE2/PHASE3"), time_to_results_days)$p.value,
    test_numeric(Studies %>% filter(phase == "PHASE3"), time_to_results_days)$p.value
  )
)

#wilc_by_phase$adj.p.val <- p.adjust(wilc_by_phase$p.value, method = "BH")

wilc_by_phase$label <- sprintf("p=%.4f", wilc_by_phase$p.value)


time_to_results_p_byphase <- 
  plot_numeric(Studies, time_to_results_days, var_label = "Time to results (days)") +
  facet_wrap(~phase) +
  labs(caption = "Wilcoxon rank sum test") +
  theme(plot.caption = element_text(face = "italic")) +
    geom_text(
      data = wilc_by_phase,
      aes(x = Inf, y = -Inf, label = label),
      hjust = 1.05,
      vjust = -0.5,
      size = 4,
      inherit.aes = FALSE, fontface = "italic"
    )

plot_grid(results_time_to_results, time_to_results_p_byphase, nrow = 1, rel_widths = c(0.4, 0.6), labels = c("", "c"), label_size = 20)
ggsave("figures/time_to_results.png", dpi = 300, height = 1.75*heigth, width = 1.75*width)


## 3.12 Era effect (start year) ----

Studies <- Studies %>% mutate(start_year = year(start_date))

Studies[which(is.na(Studies$start_year)),]


wilc_p <- test_numeric(Studies, start_year)

wilc_p$p.value
start_year_p <- 
  plot_numeric(Studies, start_year, var_label = "Start year") +
    labs(caption = sprintf("Wilcoxon rank sum test p=%.4f", wilc_p$p.value)) +
    theme(plot.caption = element_text(face = "italic", size = 12))

start_year_tbl <- 
  summary_numeric(Studies, start_year) %>%
    rename("FDA Approved" = FDA_Approved,
           Median = median,
           Min = min,
           Max = max,
           Mean = mean,
           SD = sd) %>% mutate_if(is.numeric, round, 2) %>%
    ggtexttable(rows = NULL, theme = ttheme("light", base_size = 12)) %>% 
    tab_add_title(text = "Start year", face = "bold", size = 16) 

start_year_p_tbl <- plot_grid(start_year_p, start_year_tbl, ncol = 1, rel_heights = c(0.7,0.3))


start_date_hist <- 
  Studies %>%
    filter(!is.na(start_year)) %>%
    ggplot(aes(x = start_year, fill = FDA_Approved, color = FDA_Approved)) +
    geom_histogram(position = "identity", alpha = 0.4, binwidth = 2) +
    scale_fill_brewer(palette = "Set2", name = "FDA Approved") +
    scale_color_brewer(palette = "Set2", name = "FDA Approved") +
    labs(title = "Trial start year distribution", x = "Start year", y = "Count") +
    theme_minimal_hgrid(font_size = 12)


yes <- Studies %>% filter(FDA_Approved == "Yes", !is.na(start_year)) %>% pull(start_year)
no  <- Studies %>% filter(FDA_Approved == "No",  !is.na(start_year)) %>% pull(start_year)

ks <- ks.test(yes, no, simulate.p.value = TRUE, B = 10000)

start_date_dens <- 
  Studies %>%
    filter(!is.na(start_year)) %>%
    ggplot(aes(x = start_year, fill = FDA_Approved, color = FDA_Approved)) +
    geom_density(position = "identity", alpha = 0.4) +
    scale_fill_brewer(palette = "Set2", name = "FDA Approved") +
    scale_color_brewer(palette = "Set2", name = "FDA Approved") +
    labs(title = "Trial start year distribution", x = "Start year", y = "% within FDA Approval group",
         caption = sprintf("Monte-Carlo two-sample Kolmogorov-Smirnov test p=%.4f", ks$p.value)) +
    theme_minimal_hgrid(font_size = 12) + theme(plot.caption = element_text(face = "italic"))

start_date_year <- plot_grid(start_date_hist, start_date_dens, ncol = 1)


plot_grid(start_year_p_tbl, start_date_year, nrow = 1, rel_widths = c(0.4,0.6),labels = "auto", label_size = 20)
ggsave("figures/start_date.png", dpi = 300, height = 1.75*heigth, width = 1.75*width)


