############################################################
# 02_preprocess_qualtrics_to_estimation_long.R
#
# Goal:
# 1) Read Qualtrics export
# 2) Remove Qualtrics meta rows
# 3) Identify which DCE block each respondent answered
# 4) Merge with qualtrics_design_wide.csv
# 5) Create estimation-ready long dataset
############################################################

library(dplyr)
library(tidyr)
library(stringr)

# ----------------------------------------------------------
# 1) Read files
# ----------------------------------------------------------
qualtrics_raw <- read.csv(
  "Igl Benedikt - DCMS CafeteriaMeatChoices_15. März 2026_07.02.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

design_wide <- read.csv(
  "qualtrics_design_wide.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# ----------------------------------------------------------
# 2) Remove first two Qualtrics meta rows
# ----------------------------------------------------------
qualtrics <- qualtrics_raw[-c(1, 2), ]
rownames(qualtrics) <- NULL

# ----------------------------------------------------------
# 3) Identify DCE response columns
# ----------------------------------------------------------
dce1_cols <- grep("^([1-6])_DCE 1$", names(qualtrics), value = TRUE)
dce2_cols <- grep("^([1-6])_DCE 2$", names(qualtrics), value = TRUE)
dce3_cols <- grep("^([1-6])_DCE 3$", names(qualtrics), value = TRUE)
dce4_cols <- grep("^([1-6])_DCE 4$", names(qualtrics), value = TRUE)

count_nonmissing <- function(df, cols) {
  if (length(cols) == 0) return(rep(0, nrow(df)))
  rowSums(df[, cols, drop = FALSE] != "" & !is.na(df[, cols, drop = FALSE]))
}

qualtrics <- qualtrics %>%
  mutate(
    n_dce1 = count_nonmissing(., dce1_cols),
    n_dce2 = count_nonmissing(., dce2_cols),
    n_dce3 = count_nonmissing(., dce3_cols),
    n_dce4 = count_nonmissing(., dce4_cols),
    n_dce_total = n_dce1 + n_dce2 + n_dce3 + n_dce4,
    answered_block = case_when(
      n_dce1 > 0 & n_dce2 == 0 & n_dce3 == 0 & n_dce4 == 0 ~ 1L,
      n_dce2 > 0 & n_dce1 == 0 & n_dce3 == 0 & n_dce4 == 0 ~ 2L,
      n_dce3 > 0 & n_dce1 == 0 & n_dce2 == 0 & n_dce4 == 0 ~ 3L,
      n_dce4 > 0 & n_dce1 == 0 & n_dce2 == 0 & n_dce3 == 0 ~ 4L,
      TRUE ~ NA_integer_
    )
  )

# ----------------------------------------------------------
# 4) Keep only respondents with valid DCE block assignment
# ----------------------------------------------------------
qualtrics_dce <- qualtrics %>%
  filter(!is.na(answered_block), n_dce_total > 0)

if ("ResponseId" %in% names(qualtrics_dce)) {
  qualtrics_dce$resp_id <- qualtrics_dce$ResponseId
} else {
  qualtrics_dce$resp_id <- paste0("R", seq_len(nrow(qualtrics_dce)))
}

# ----------------------------------------------------------
# 5) Reshape Qualtrics choices into long task format
# ----------------------------------------------------------
extract_block_choices <- function(df, block_num, cols) {
  if (length(cols) == 0) return(NULL)
  
  df %>%
    select(resp_id, answered_block, all_of(cols)) %>%
    filter(answered_block == block_num) %>%
    pivot_longer(
      cols = all_of(cols),
      names_to = "task_col",
      values_to = "choice"
    ) %>%
    mutate(
      TaskInBlock = as.integer(str_extract(task_col, "^[1-6]")),
      choice = suppressWarnings(as.integer(choice))
    ) %>%
    select(resp_id, answered_block, TaskInBlock, choice) %>%
    filter(!is.na(choice))
}

choices_long <- bind_rows(
  extract_block_choices(qualtrics_dce, 1, dce1_cols),
  extract_block_choices(qualtrics_dce, 2, dce2_cols),
  extract_block_choices(qualtrics_dce, 3, dce3_cols),
  extract_block_choices(qualtrics_dce, 4, dce4_cols)
)

# ----------------------------------------------------------
# 6) Map TaskInBlock using design_wide
# ----------------------------------------------------------
design_wide <- design_wide %>%
  group_by(Block) %>%
  arrange(Block, TaskID) %>%
  mutate(TaskInBlock = row_number()) %>%
  ungroup()

merged_wide <- choices_long %>%
  rename(Block = answered_block) %>%
  left_join(design_wide, by = c("Block", "TaskInBlock"))

# ----------------------------------------------------------
# 7) Convert to estimation-ready long format
# ----------------------------------------------------------
estimation_long <- bind_rows(
  
  merged_wide %>%
    transmute(
      resp_id, Block, TaskID, TaskInBlock, TaskType, CompType,
      Alt = "A",
      chosen = ifelse(choice == 1, 1, 0),
      DishFamily = A_DishFamily,
      ProteinType = A_ProteinType,
      Price = A_Price,
      NutriScore = A_NutriScore,
      HighProtein = A_HighProtein,
      ImageURL = A_ImageURL,
      NutriImageURL = A_NutriImageURL,
      HPImageURL = A_HPImageURL
    ),
  
  merged_wide %>%
    transmute(
      resp_id, Block, TaskID, TaskInBlock, TaskType, CompType,
      Alt = "B",
      chosen = ifelse(choice == 2, 1, 0),
      DishFamily = B_DishFamily,
      ProteinType = B_ProteinType,
      Price = B_Price,
      NutriScore = B_NutriScore,
      HighProtein = B_HighProtein,
      ImageURL = B_ImageURL,
      NutriImageURL = B_NutriImageURL,
      HPImageURL = B_HPImageURL
    ),
  
  merged_wide %>%
    transmute(
      resp_id, Block, TaskID, TaskInBlock, TaskType, CompType,
      Alt = "C",
      chosen = ifelse(choice == 3, 1, 0),
      DishFamily = C_DishFamily,
      ProteinType = C_ProteinType,
      Price = C_Price,
      NutriScore = C_NutriScore,
      HighProtein = C_HighProtein,
      ImageURL = C_ImageURL,
      NutriImageURL = C_NutriImageURL,
      HPImageURL = C_HPImageURL
    )
  
) %>%
  arrange(resp_id, Block, TaskInBlock, Alt)

# ----------------------------------------------------------
# 8) Add model variables
# ----------------------------------------------------------
estimation_long <- estimation_long %>%
  mutate(
    Nutri_A = ifelse(NutriScore == "A", 1, 0),
    Nutri_B = ifelse(NutriScore == "B", 1, 0),
    Veg     = ifelse(ProteinType == "Veg", 1, 0),
    Vegan   = ifelse(ProteinType == "Vegan", 1, 0),
    chid    = paste(resp_id, TaskID, sep = "_")
  )

# ----------------------------------------------------------
# 9) Save outputs
# ----------------------------------------------------------
write.csv(merged_wide, "qualtrics_merged_wide.csv", row.names = FALSE)
write.csv(estimation_long, "qualtrics_estimation_long.csv", row.names = FALSE)

cat("Saved files:\n")
cat("- qualtrics_merged_wide.csv\n")
cat("- qualtrics_estimation_long.csv\n")