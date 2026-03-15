############################################################
# 04_descriptive_summary_nondce.R
#
# Purpose:
# Summarize non-DCE survey responses from raw Qualtrics export
#
# Input:
# - Igl Benedikt - DCMS CafeteriaMeatChoices_15. März 2026_07.02.csv
#
# Output:
# - respondent_level_nondce.csv
# - descriptives_categorical.csv
# - descriptives_numeric.csv
############################################################

library(dplyr)

# ----------------------------------------------------------
# 1) Read raw Qualtrics export
# ----------------------------------------------------------
raw <- read.csv(
  "Igl Benedikt - DCMS CafeteriaMeatChoices_15. März 2026_07.02.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# ----------------------------------------------------------
# 2) Remove first two Qualtrics metadata rows
# ----------------------------------------------------------
dat <- raw[-c(1, 2), ]
rownames(dat) <- NULL

# ----------------------------------------------------------
# 3) Keep only relevant respondents
# ----------------------------------------------------------
dat_use <- dat %>%
  filter(
    Finished == "1",
    Consent == "1",
    is.na(Q_TerminateFlag) | Q_TerminateFlag != "Screened"
  )

# ----------------------------------------------------------
# 4) Select non-DCE variables
# ----------------------------------------------------------
keep_vars <- c(
  "ResponseId",
  "Consent",
  "Veggi/Vegan",
  "Cafeteria Usage",
  "Typical Meal Choice",
  "Importance of Price",
  "Attention Nutrition",
  "NutriScore",
  "Meat consumption",
  "Attitude plantpbased",
  "Diet importance",
  "Percieved Healthines",
  "Environment Attitude",
  "Protein perception",
  "Age",
  "Gender",
  "Student Status",
  "Field of Study",
  "Final Feedback",
  "Q_TerminateFlag"
)

keep_vars <- keep_vars[keep_vars %in% names(dat_use)]

resp <- dat_use %>%
  select(all_of(keep_vars))

write.csv(resp, "respondent_level_nondce.csv", row.names = FALSE)

# ----------------------------------------------------------
# 5) Helper for categorical summaries
# ----------------------------------------------------------
summarise_categorical <- function(df, varname) {
  x <- df[[varname]]
  out <- as.data.frame(table(x, useNA = "ifany"), stringsAsFactors = FALSE)
  names(out) <- c("Response", "Count")
  out$Share <- round(out$Count / sum(out$Count), 3)
  out$Variable <- varname
  out %>%
    select(Variable, Response, Count, Share)
}

# ----------------------------------------------------------
# 6) Create categorical summary table
# ----------------------------------------------------------
categorical_vars <- c(
  "Veggi/Vegan",
  "Cafeteria Usage",
  "Typical Meal Choice",
  "Importance of Price",
  "Attention Nutrition",
  "NutriScore",
  "Meat consumption",
  "Attitude plantpbased",
  "Diet importance",
  "Percieved Healthines",
  "Environment Attitude",
  "Protein perception",
  "Gender",
  "Student Status",
  "Field of Study",
  "Q_TerminateFlag"
)

categorical_vars <- categorical_vars[categorical_vars %in% names(resp)]

desc_cat <- bind_rows(lapply(categorical_vars, function(v) {
  summarise_categorical(resp, v)
}))

write.csv(desc_cat, "descriptives_categorical.csv", row.names = FALSE)

# ----------------------------------------------------------
# 7) Numeric summary table
# ----------------------------------------------------------
numeric_vars <- c("Age")
numeric_vars <- numeric_vars[numeric_vars %in% names(resp)]

desc_num <- bind_rows(lapply(numeric_vars, function(v) {
  x <- suppressWarnings(as.numeric(resp[[v]]))
  data.frame(
    Variable = v,
    N = sum(!is.na(x)),
    Mean = round(mean(x, na.rm = TRUE), 2),
    SD = round(sd(x, na.rm = TRUE), 2),
    Min = round(min(x, na.rm = TRUE), 2),
    Max = round(max(x, na.rm = TRUE), 2)
  )
}))

write.csv(desc_num, "descriptives_numeric.csv", row.names = FALSE)

cat("Saved files:\n")
cat("- respondent_level_nondce.csv\n")
cat("- descriptives_categorical.csv\n")
cat("- descriptives_numeric.csv\n")