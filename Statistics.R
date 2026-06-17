
library(readxl)
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
ggplot(data,
       aes(WebcamMode,
           PA_score,
           group = Participant)) +
  geom_line(alpha = .5) +
  geom_point(size = 2)

ggplot(data,
       aes(WebcamMode,
           NA_score,
           group = Participant)) +
  geom_line(alpha = .5) +
  geom_point(size = 2)

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

friedman_results <- do.call(rbind, friedman_results)
friedman_results$p_adj <- p.adjust(friedman_results$p, method = "holm")

print(desc)
print(anova_PA)
print(anova_NA)
print(friedman_PA)
print(friedman_NA)
print(friedman_results)

