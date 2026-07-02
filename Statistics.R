library(readxl)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rstatix)
library(ez)
library(psych)

data <- read_excel("PANASData_Testable.xlsx")
positive_items <- c(
  "Interested","Excited","Strong","Enthusiastic",
  "Proud","Alert","Inspired","Determined",
  "Attentive","Active"
)

negative_items <- c(
  "Distressed","Upset","Guilty","Scared",
  "Hostile","Irritable","Ashamed",
  "Nervous","Jittery","Afraid"
)

data <- data %>%
  mutate(WebcamMode = recode(WebcamMode, "On" = "Non-mirrored"))

data <- data %>%
  rowwise() %>%
  mutate(
    PA_score = sum(c_across(all_of(positive_items))),
    NA_score = sum(c_across(all_of(negative_items)))
  ) %>%
  ungroup()

# DESCRIPTIVES
desc <- data %>%
  group_by(WebcamMode) %>%
  summarise(
    Mean_PA_score = mean(PA_score),
    SD_PA_score = sd(PA_score),
    Mean_NA_score = mean(NA_score),
    SD_NA_score = sd(NA_score)
  )


# VISUALISATIONS

# Reshape to long format for plotting
plot_data <- data %>%
  select(Participant, WebcamMode, PA_score, NA_score) %>%
  pivot_longer(
    cols = c(PA_score, NA_score),
    names_to = "AffectType",
    values_to = "Score"
  ) %>%
  mutate(
    AffectType = recode(AffectType,
                        PA_score = "Positive affect",
                        NA_score = "Negative affect"),
    WebcamMode = factor(WebcamMode, levels = c("Off", "Non-mirrored", "Mirrored"))
  )

summary_data <- plot_data %>%
  group_by(WebcamMode, AffectType) %>%
  summarise(
    Mean = mean(Score),
    SE = sd(Score) / sqrt(n()),
    .groups = "drop"
  )

ggplot(summary_data, aes(x = interaction(AffectType, WebcamMode, sep = "\n"),
                         y = Mean, fill = AffectType)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  scale_fill_manual(values = c("Positive affect" = "#9DC3E6",
                               "Negative affect" = "#F4B183")) +
  labs(title = "Mean PANAS affect scores", x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

# REPEATED-MEASURES ANOVA
anova_PA <- ezANOVA(
  data = data,
  dv = PA_score,
  wid = Participant,
  within = WebcamMode,
  detailed = TRUE
)

anova_NA <- ezANOVA(
  data = data,
  dv = NA_score,
  wid = Participant,
  within = WebcamMode,
  detailed = TRUE
)



# FRIEDMAN TESTS
friedman_PA <- friedman_test(
  data,
  PA_score ~ WebcamMode | Participant
)

friedman_NA <- friedman_test(
  data,
  NA_score ~ WebcamMode | Participant
)


# EFFECT SIZES
friedman_effsize(
  data,
  PA_score ~ WebcamMode | Participant
)

friedman_effsize(
  data,
  NA_score ~ WebcamMode | Participant
)

# CRONBACH'S ALPHA
alpha(data[positive_items], check.keys = TRUE)
alpha(data[negative_items])

# ITEM-LEVEL FRIEDMAN TEST
items_to_test <- c("Ashamed", "Nervous", "Alert", "Attentive")

friedman_results <- lapply(items_to_test, function(item) {
  formula <- as.formula(paste(item, "~ WebcamMode | Participant"))
  test <- friedman.test(formula, data = data)
  
  data.frame(
    Item = item,
    statistic = test$statistic,
    df = test$parameter,
    p = test$p.value
  )
})


#Differences between Positive and negative components per condition

conditions <- unique(data$WebcamMode)

pa_vs_na_results <- lapply(conditions, function(cond) {
  sub <- data %>% filter(WebcamMode == cond)
  
  diffs <- sub$PA_score - sub$NA_score
  n <- length(diffs)
  
  t_test  <- t.test(sub$PA_score, sub$NA_score, paired = TRUE)
  wilcox  <- wilcox.test(sub$PA_score, sub$NA_score, paired = TRUE)
  
  # Cohen's d for paired samples: mean difference / SD of differences
  cohens_d <- mean(diffs) / sd(diffs)
  
  # Matched-pairs rank-biserial correlation for Wilcoxon
  # r = (W+ - W-) / (W+ + W-), derived from the V statistic
  ranks <- rank(abs(diffs[diffs != 0]))
  signs <- sign(diffs[diffs != 0])
  W_pos <- sum(ranks[signs > 0])
  W_neg <- sum(ranks[signs < 0])
  rank_biserial <- (W_pos - W_neg) / (W_pos + W_neg)
  
  data.frame(
    WebcamMode = cond,
    Mean_PA = mean(sub$PA_score),
    Mean_NA = mean(sub$NA_score),
    t_statistic = t_test$statistic,
    t_df = t_test$parameter,
    t_p = t_test$p.value,
    cohens_d = cohens_d,
    wilcoxon_V = wilcox$statistic,
    wilcoxon_p = wilcox$p.value,
    rank_biserial_r = rank_biserial
  )
})

pa_vs_na_results <- do.call(rbind, pa_vs_na_results)
pa_vs_na_results$t_p_adj <- p.adjust(pa_vs_na_results$t_p, method = "holm")
pa_vs_na_results$wilcoxon_p_adj <- p.adjust(pa_vs_na_results$wilcoxon_p, method = "holm")

print(pa_vs_na_results)

friedman_results <- do.call(rbind, friedman_results)
friedman_results$p_adj <- p.adjust(friedman_results$p, method = "holm")

print(desc)
print(anova_PA)
print(anova_NA)
print(friedman_PA)
print(friedman_NA)
print(friedman_results)

#Building the tables

#CLEANING THE TABLES
#--------------------------------------------------------------------
# --- Descriptives ---
desc_clean <- desc %>%
  transmute(
    Condition = WebcamMode,
    `Mean PA` = round(Mean_PA_score, 2),
    `SD PA` = round(SD_PA_score, 2),
    `Mean NA` = round(Mean_NA_score, 2),
    `SD NA` = round(SD_NA_score, 2)
  )

# --- PA vs NA paired comparison ---
pa_vs_na_clean <- pa_vs_na_results %>%
  transmute(
    Condition = WebcamMode,
    `Mean PA` = round(Mean_PA, 2),
    `Mean NA` = round(Mean_NA, 2),
    `t` = round(t_statistic, 2),
    `df` = t_df,
    `p (adj)` = round(t_p_adj, 3),
    `Cohen's d` = round(cohens_d, 2)
  )

# --- ANOVA (PA) ---
anova_PA_clean <- anova_PA$ANOVA %>%
  transmute(
    Effect,
    `df num` = DFn,
    `df den` = DFd,
    `F` = round(F, 2),
    `p` = round(p, 3),
    `ges` = round(ges, 3)
  )

# --- ANOVA (NA) ---
anova_NA_clean <- anova_NA$ANOVA %>%
  transmute(
    Effect,
    `df num` = DFn,
    `df den` = DFd,
    `F` = round(F, 2),
    `p` = round(p, 3),
    `ges` = round(ges, 3)
  )

# --- Item-level Friedman ---
friedman_clean <- friedman_results %>%
  transmute(
    Item,
    `chi-squared` = round(statistic, 2),
    df,
    `p (adj)` = round(p_adj, 3)
  )

#WRITING THE KABLE CODE
#---------------------------------------------------------------------------

desc_latex <- kable(desc_clean, format = "latex", booktabs = TRUE,
                    caption = "Mean PANAS scores by webcam condition",
                    label = "tab:desc") %>%
  kable_styling(latex_options = c("hold_position"))

pa_vs_na_latex <- kable(pa_vs_na_clean, format = "latex", booktabs = TRUE,
                        caption = "Paired comparison of positive and negative affect by condition",
                        label = "tab:pa_vs_na") %>%
  kable_styling(latex_options = c("hold_position"))

anova_PA_latex <- kable(anova_PA_clean, format = "latex", booktabs = TRUE,
                        caption = "Repeated-measures ANOVA: Positive affect",
                        label = "tab:anova_pa") %>%
  kable_styling(latex_options = c("hold_position"))

anova_NA_latex <- kable(anova_NA_clean, format = "latex", booktabs = TRUE,
                        caption = "Repeated-measures ANOVA: Negative affect",
                        label = "tab:anova_na") %>%
  kable_styling(latex_options = c("hold_position"))

friedman_latex <- kable(friedman_clean, format = "latex", booktabs = TRUE,
                        caption = "Item-level Friedman tests (Holm-corrected)",
                        label = "tab:item_friedman") %>%
  kable_styling(latex_options = c("hold_position"))


#Saving the tables
writeLines(as.character(desc_latex), "desc_table.tex")
writeLines(as.character(pa_vs_na_latex), "pa_vs_na_table.tex")
writeLines(as.character(anova_PA_latex), "anova_PA_table.tex")
writeLines(as.character(anova_NA_latex), "anova_NA_table.tex")
writeLines(as.character(friedman_latex), "friedman_table.tex")