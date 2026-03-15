############################################################
# 01_generate_design_and_qualtrics_tasks.R
#
# DCE design generator for cafeteria meal choice experiment
#
# Output files:
# - design_long_for_estimation.csv
# - qualtrics_design_wide.csv
# - qualtrics_loopmerge_minimal.csv
# - qualtrics_tasks_clean.csv
############################################################

library(dplyr)
library(tidyr)
library(tibble)

set.seed(123)

# ----------------------------------------------------------
# 0) GitHub / URL settings
# ----------------------------------------------------------
github_base <- "https://raw.githubusercontent.com/IBenCodin/mensa-dce-images/main/"
food_base   <- paste0(github_base, "food/")
label_base  <- paste0(github_base, "labels/")

# ----------------------------------------------------------
# 1) Global settings
# ----------------------------------------------------------
N_TOTAL_TASKS  <- 24
TASKS_PER_RESP <- 6
N_BLOCKS       <- N_TOTAL_TASKS / TASKS_PER_RESP   # = 4
POOL_PER_CELL  <- 300

dish_families <- c("Gyros", "Pasta", "Curry")

price_levels <- c(4.0, 4.5, 5.0)

# NutriScore A/B/C, A rare
nutri_levels <- c("A", "B", "C")
nutri_probs  <- c(0.07, 0.465, 0.465)

# High Protein badge
hp_levels <- c(0, 1)   # 0 = none, 1 = badge

# ----------------------------------------------------------
# 2) Design logic
# ----------------------------------------------------------

# WITHIN-DISH
within_compositions <- list(
  C1_111 = c("Meat", "Veg", "Vegan")
)

within_targets <- tribble(
  ~DishFamily, ~CompType, ~n,
  "Gyros",     "C1_111",  4,
  "Pasta",     "C1_111",  4,
  "Curry",     "C1_111",  4
)

# CROSS-DISH
# A = Gyros, B = Pasta, C = Curry
cross_patterns <- list(
  P1 = c("Meat", "Veg", "Vegan"),
  P2 = c("Vegan", "Meat", "Veg"),
  P3 = c("Veg", "Vegan", "Meat")
)

# ----------------------------------------------------------
# 3) Helper functions
# ----------------------------------------------------------
rand_alt_attrs <- function() {
  list(
    Price = sample(price_levels, 1),
    Nutri = sample(nutri_levels, 1, prob = nutri_probs),
    HP    = sample(hp_levels, 1)
  )
}

passes_constraints_wide <- function(row) {
  # No completely identical alternatives
  all_identical <- (
    row$A_Price == row$B_Price & row$B_Price == row$C_Price &
      row$A_Nutri == row$B_Nutri & row$B_Nutri == row$C_Nutri &
      row$A_HP == row$B_HP & row$B_HP == row$C_HP
  )
  if (all_identical) return(FALSE)
  
  # Meat must never be the cheapest option
  min_price <- min(row$A_Price, row$B_Price, row$C_Price)
  
  if ((row$AltA_type == "Meat" & row$A_Price == min_price) |
      (row$AltB_type == "Meat" & row$B_Price == min_price) |
      (row$AltC_type == "Meat" & row$C_Price == min_price)) {
    return(FALSE)
  }
  
  TRUE
}

make_within_task <- function(dish, comp_name, comp_vec) {
  A <- rand_alt_attrs()
  B <- rand_alt_attrs()
  C <- rand_alt_attrs()
  
  tibble(
    TaskType   = "WithinDish",
    DishFamily = dish,
    CompType   = comp_name,
    
    AltA_type  = comp_vec[1],
    AltB_type  = comp_vec[2],
    AltC_type  = comp_vec[3],
    
    A_Price    = A$Price,
    A_Nutri    = A$Nutri,
    A_HP       = A$HP,
    
    B_Price    = B$Price,
    B_Nutri    = B$Nutri,
    B_HP       = B$HP,
    
    C_Price    = C$Price,
    C_Nutri    = C$Nutri,
    C_HP       = C$HP
  )
}

make_cross_task <- function(pattern_name, pattern_vec) {
  A <- rand_alt_attrs()
  B <- rand_alt_attrs()
  C <- rand_alt_attrs()
  
  tibble(
    TaskType   = "CrossDish",
    DishFamily = NA_character_,
    CompType   = pattern_name,
    
    AltA_type  = pattern_vec[1],   # Gyros
    AltB_type  = pattern_vec[2],   # Pasta
    AltC_type  = pattern_vec[3],   # Curry
    
    A_Price    = A$Price,
    A_Nutri    = A$Nutri,
    A_HP       = A$HP,
    
    B_Price    = B$Price,
    B_Nutri    = B$Nutri,
    B_HP       = B$HP,
    
    C_Price    = C$Price,
    C_Nutri    = C$Nutri,
    C_HP       = C$HP
  )
}

# ----------------------------------------------------------
# 4) Candidate pools
# ----------------------------------------------------------
within_pool <- bind_rows(lapply(1:nrow(within_targets), function(i) {
  dish <- within_targets$DishFamily[i]
  ct   <- within_targets$CompType[i]
  comp_vec <- within_compositions[[ct]]
  
  bind_rows(replicate(
    POOL_PER_CELL,
    make_within_task(dish, ct, comp_vec),
    simplify = FALSE
  ))
}))

within_pool <- within_pool %>%
  rowwise() %>%
  mutate(ok = passes_constraints_wide(cur_data())) %>%
  ungroup() %>%
  filter(ok) %>%
  select(-ok)

cross_pool <- bind_rows(lapply(names(cross_patterns), function(pn) {
  pv <- cross_patterns[[pn]]
  
  bind_rows(replicate(
    POOL_PER_CELL,
    make_cross_task(pn, pv),
    simplify = FALSE
  ))
}))

cross_pool <- cross_pool %>%
  rowwise() %>%
  mutate(ok = passes_constraints_wide(cur_data())) %>%
  ungroup() %>%
  filter(ok) %>%
  select(-ok)

# ----------------------------------------------------------
# 4b) Too-easy-task filter
# ----------------------------------------------------------
nutri_rank <- function(x) {
  case_when(
    x == "A" ~ 3,
    x == "B" ~ 2,
    x == "C" ~ 1,
    TRUE ~ NA_real_
  )
}

too_easy_pair <- function(price1, nutri1, hp1, price2, nutri2, hp2) {
  no_worse_price <- price1 <= price2
  
  n1 <- nutri_rank(nutri1)
  n2 <- nutri_rank(nutri2)
  no_worse_nutri <- n1 >= n2
  
  no_worse_hp <- hp1 >= hp2
  
  better_price <- price1 < price2
  better_nutri <- n1 > n2
  better_hp    <- hp1 > hp2
  
  n_strict_better <- sum(c(better_price, better_nutri, better_hp))
  
  no_worse_price & no_worse_nutri & no_worse_hp & n_strict_better >= 2
}

too_easy_task <- function(row) {
  A_price <- row$A_Price
  A_nutri <- row$A_Nutri
  A_hp    <- row$A_HP
  
  B_price <- row$B_Price
  B_nutri <- row$B_Nutri
  B_hp    <- row$B_HP
  
  C_price <- row$C_Price
  C_nutri <- row$C_Nutri
  C_hp    <- row$C_HP
  
  ab <- too_easy_pair(A_price, A_nutri, A_hp, B_price, B_nutri, B_hp)
  ac <- too_easy_pair(A_price, A_nutri, A_hp, C_price, C_nutri, C_hp)
  
  ba <- too_easy_pair(B_price, B_nutri, B_hp, A_price, A_nutri, A_hp)
  bc <- too_easy_pair(B_price, B_nutri, B_hp, C_price, C_nutri, C_hp)
  
  ca <- too_easy_pair(C_price, C_nutri, C_hp, A_price, A_nutri, A_hp)
  cb <- too_easy_pair(C_price, C_nutri, C_hp, B_price, B_nutri, B_hp)
  
  ab | ac | ba | bc | ca | cb
}

within_pool <- within_pool %>%
  rowwise() %>%
  mutate(too_easy = too_easy_task(cur_data())) %>%
  ungroup() %>%
  filter(!too_easy) %>%
  select(-too_easy)

cross_pool <- cross_pool %>%
  rowwise() %>%
  mutate(too_easy = too_easy_task(cur_data())) %>%
  ungroup() %>%
  filter(!too_easy) %>%
  select(-too_easy)

cat("Within-pool after too-easy filter:", nrow(within_pool), "\n")
cat("Cross-pool after too-easy filter:", nrow(cross_pool), "\n")

# ----------------------------------------------------------
# 5) Draw final tasks
# ----------------------------------------------------------
within_selected <- within_targets %>%
  rowwise() %>%
  do({
    dish <- .$DishFamily
    comp <- .$CompType
    need <- .$n
    
    cand <- within_pool %>%
      filter(DishFamily == dish, CompType == comp)
    
    if (nrow(cand) < need) {
      stop("Not enough WithinDish candidates. Increase POOL_PER_CELL.")
    }
    
    cand %>% slice_sample(n = need)
  }) %>%
  ungroup()

cross_selected <- bind_rows(lapply(names(cross_patterns), function(pn) {
  cand <- cross_pool %>% filter(CompType == pn)
  
  if (nrow(cand) < 4) {
    stop("Not enough CrossDish candidates. Increase POOL_PER_CELL.")
  }
  
  cand %>% slice_sample(n = 4)
}))

all_tasks <- bind_rows(within_selected, cross_selected) %>%
  slice_sample(prop = 1) %>%
  mutate(TaskID = 1:n())

# ----------------------------------------------------------
# 6) Assign blocks
# ----------------------------------------------------------
within_ids <- all_tasks %>%
  filter(TaskType == "WithinDish") %>%
  pull(TaskID) %>%
  sample()

cross_ids <- all_tasks %>%
  filter(TaskType == "CrossDish") %>%
  pull(TaskID) %>%
  sample()

block_map <- tibble(
  TaskID = c(within_ids, cross_ids),
  Block  = c(rep(1:N_BLOCKS, each = 3),
             rep(1:N_BLOCKS, each = 3))
)

all_tasks <- all_tasks %>%
  left_join(block_map, by = "TaskID") %>%
  arrange(Block, TaskID)

# ----------------------------------------------------------
# 7) Long format
# ----------------------------------------------------------
design_long <- bind_rows(
  all_tasks %>%
    transmute(
      Block, TaskID, TaskType,
      DishFamily = ifelse(TaskType == "CrossDish", "Gyros", DishFamily),
      Alt = "A",
      ProteinType = AltA_type,
      Price = A_Price,
      NutriScore = A_Nutri,
      HighProtein = A_HP,
      CompType
    ),
  
  all_tasks %>%
    transmute(
      Block, TaskID, TaskType,
      DishFamily = ifelse(TaskType == "CrossDish", "Pasta", DishFamily),
      Alt = "B",
      ProteinType = AltB_type,
      Price = B_Price,
      NutriScore = B_Nutri,
      HighProtein = B_HP,
      CompType
    ),
  
  all_tasks %>%
    transmute(
      Block, TaskID, TaskType,
      DishFamily = ifelse(TaskType == "CrossDish", "Curry", DishFamily),
      Alt = "C",
      ProteinType = AltC_type,
      Price = C_Price,
      NutriScore = C_Nutri,
      HighProtein = C_HP,
      CompType
    )
) %>%
  arrange(Block, TaskID, Alt)

# ----------------------------------------------------------
# 8) Assign image variants
# ----------------------------------------------------------
design_long <- design_long %>%
  group_by(TaskID, ProteinType) %>%
  mutate(Variant = row_number()) %>%
  ungroup() %>%
  mutate(Variant = ifelse(Variant > 2, 2, Variant))

# ----------------------------------------------------------
# 9) Map food images
# ----------------------------------------------------------
pic_map <- expand.grid(
  DishFamily = c("Gyros", "Pasta", "Curry"),
  ProteinType = c("Meat", "Veg", "Vegan"),
  Variant = c(1, 2),
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(
    ProteinFile = case_when(
      ProteinType == "Meat"  ~ "meat",
      ProteinType == "Veg"   ~ "veggi",
      ProteinType == "Vegan" ~ "vegan"
    ),
    ImageFile = paste0(
      tolower(DishFamily), "_",
      ProteinFile, "_",
      Variant, ".png"
    ),
    ImageURL = paste0(food_base, ImageFile)
  )

design_long <- design_long %>%
  left_join(
    pic_map %>% select(DishFamily, ProteinType, Variant, ImageFile, ImageURL),
    by = c("DishFamily", "ProteinType", "Variant")
  )

# ----------------------------------------------------------
# 10) Map label images
# ----------------------------------------------------------
design_long <- design_long %>%
  mutate(
    NutriImageFile = paste0(NutriScore, "_Score.png"),
    NutriImageURL  = paste0(label_base, NutriImageFile),
    HPImageFile    = ifelse(HighProtein == 1, "HP_clean.png", "blank.png"),
    HPImageURL     = paste0(label_base, HPImageFile)
  )

# ----------------------------------------------------------
# 11) Wide format for Qualtrics
# ----------------------------------------------------------
design_wide <- design_long %>%
  select(
    Block, TaskID, TaskType, CompType, Alt,
    DishFamily, ProteinType, Price, NutriScore, HighProtein,
    ImageURL, NutriImageURL, HPImageURL
  ) %>%
  pivot_wider(
    id_cols = c(Block, TaskID, TaskType, CompType),
    names_from = Alt,
    values_from = c(
      DishFamily, ProteinType, Price, NutriScore,
      HighProtein, ImageURL, NutriImageURL, HPImageURL
    ),
    names_glue = "{Alt}_{.value}"
  ) %>%
  arrange(Block, TaskID)

# ----------------------------------------------------------
# 12) Export main design files
# ----------------------------------------------------------
write.csv(design_long, "design_long_for_estimation.csv", row.names = FALSE)
write.csv(design_wide, "qualtrics_design_wide.csv", row.names = FALSE)

# ----------------------------------------------------------
# 13) Minimal Loop & Merge CSV
# ----------------------------------------------------------
qualtrics_loopmerge <- design_wide %>%
  select(
    Block,
    TaskID,
    A_ImageURL, B_ImageURL, C_ImageURL,
    A_Price, B_Price, C_Price,
    A_NutriImageURL, B_NutriImageURL, C_NutriImageURL,
    A_HPImageURL, B_HPImageURL, C_HPImageURL
  ) %>%
  arrange(Block, TaskID)

write.csv(qualtrics_loopmerge, "qualtrics_loopmerge_minimal.csv", row.names = FALSE)

# ----------------------------------------------------------
# 14) Clean Qualtrics task CSV with short file names
# ----------------------------------------------------------
get_filename <- function(x) {
  sub(".*/", "", x)
}

qualtrics_tasks <- design_wide %>%
  mutate(
    A_ImageFile = get_filename(A_ImageURL),
    B_ImageFile = get_filename(B_ImageURL),
    C_ImageFile = get_filename(C_ImageURL),
    A_NutriFile = get_filename(A_NutriImageURL),
    B_NutriFile = get_filename(B_NutriImageURL),
    C_NutriFile = get_filename(C_NutriImageURL),
    A_HPFile = get_filename(A_HPImageURL),
    B_HPFile = get_filename(B_HPImageURL),
    C_HPFile = get_filename(C_HPImageURL)
  ) %>%
  select(
    Block, TaskID,
    A_ImageFile, B_ImageFile, C_ImageFile,
    A_Price, B_Price, C_Price,
    A_NutriFile, B_NutriFile, C_NutriFile,
    A_HPFile, B_HPFile, C_HPFile
  ) %>%
  arrange(Block, TaskID)

write.csv(qualtrics_tasks, "qualtrics_tasks_clean.csv", row.names = FALSE)

cat("Qualtrics CSV created: qualtrics_tasks_clean.csv\n")