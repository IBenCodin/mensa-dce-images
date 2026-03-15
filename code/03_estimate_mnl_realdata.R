############################################################
# 03_estimate_mnl_realdata.R
#
########### UNUSED in the final Report Version #############
#
#
# Input:
#   qualtrics_estimation_long.csv
#
# Output:
#   - pooled / within-dish MNL models
#   - coefficient tables
#   - model fit table
############################################################

library(dplyr)
library(mlogit)
library(tidyr)

# ----------------------------------------------------------
# 1) Read estimation-ready data
# ----------------------------------------------------------
data_long <- read.csv(
  "qualtrics_estimation_long.csv",
  stringsAsFactors = FALSE
)

# ----------------------------------------------------------
# 2) Basic checks
# ----------------------------------------------------------
choice_check <- data_long %>%
  group_by(resp_id, TaskID) %>%
  summarise(sum_chosen = sum(chosen), .groups = "drop")

valid_chids <- choice_check %>%
  filter(sum_chosen == 1) %>%
  mutate(chid = paste(resp_id, TaskID, sep = "_")) %>%
  pull(chid)

data_long <- data_long %>%
  filter(chid %in% valid_chids) %>%
  mutate(
    Price = as.numeric(Price),
    Nutri_A = as.numeric(Nutri_A),
    Nutri_B = as.numeric(Nutri_B),
    HighProtein = as.numeric(HighProtein),
    Veg = as.numeric(Veg),
    Vegan = as.numeric(Vegan),
    chosen = as.numeric(chosen)
  )

# ----------------------------------------------------------
# 3) Pooled model
# ----------------------------------------------------------
mlogit_df <- mlogit.data(
  data_long,
  choice   = "chosen",
  shape    = "long",
  chid.var = "chid",
  alt.var  = "Alt",
  id.var   = "resp_id"
)

mnl_pooled <- mlogit(
  chosen ~ Price + Nutri_A + Nutri_B + HighProtein + Veg + Vegan | 0,
  data = mlogit_df,
  method = "nr"
)

print(summary(mnl_pooled))

# ----------------------------------------------------------
# 4) Within-dish model
# ----------------------------------------------------------
data_within <- data_long %>%
  filter(TaskType == "WithinDish")

mnl_within <- NULL

if (length(unique(data_within$chid)) > 1) {
  mlogit_within <- mlogit.data(
    data_within,
    choice   = "chosen",
    shape    = "long",
    chid.var = "chid",
    alt.var  = "Alt",
    id.var   = "resp_id"
  )
  
  mnl_within <- tryCatch(
    mlogit(
      chosen ~ Price + Nutri_A + Nutri_B + HighProtein + Veg + Vegan | 0,
      data = mlogit_within,
      method = "nr"
    ),
    error = function(e) NULL
  )
  
  if (!is.null(mnl_within)) print(summary(mnl_within))
}

# ----------------------------------------------------------
# 5) Cross-dish model
# ----------------------------------------------------------
data_cross <- data_long %>%
  filter(TaskType == "CrossDish")

mnl_cross <- NULL

if (length(unique(data_cross$chid)) > 1) {
  mlogit_cross <- mlogit.data(
    data_cross,
    choice   = "chosen",
    shape    = "long",
    chid.var = "chid",
    alt.var  = "Alt",
    id.var   = "resp_id"
  )
  
  mnl_cross <- tryCatch(
    mlogit(
      chosen ~ Price + Nutri_A + Nutri_B + HighProtein + Veg + Vegan | 0,
      data = mlogit_cross,
      method = "nr"
    ),
    error = function(e) NULL
  )
  
  if (!is.null(mnl_cross)) print(summary(mnl_cross))
}

# ----------------------------------------------------------
# 6) Coefficient tables
# ----------------------------------------------------------
get_coef_df <- function(model, model_name) {
  if (is.null(model)) return(NULL)
  s <- summary(model)
  out <- data.frame(
    Variable = rownames(s$CoefTable),
    Estimate = s$CoefTable[, "Estimate"],
    StdError = s$CoefTable[, "Std. Error"],
    zvalue   = s$CoefTable[, "z-value"],
    pvalue   = s$CoefTable[, "Pr(>|z|)"],
    Model    = model_name,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

coef_all <- bind_rows(
  get_coef_df(mnl_pooled, "Pooled"),
  get_coef_df(mnl_within, "WithinDish"),
  get_coef_df(mnl_cross, "CrossDish")
)

write.csv(coef_all, "realdata_mnl_coefficients_detailed.csv", row.names = FALSE)

coef_table <- coef_all %>%
  select(Model, Variable, Estimate) %>%
  pivot_wider(names_from = Model, values_from = Estimate)

write.csv(coef_table, "realdata_mnl_coefficients.csv", row.names = FALSE)

# ----------------------------------------------------------
# 7) Fit table
# ----------------------------------------------------------
fit_table <- bind_rows(
  if (!is.null(mnl_pooled)) {
    data.frame(
      Model = "Pooled",
      LogLik = as.numeric(logLik(mnl_pooled)),
      AIC = AIC(mnl_pooled),
      BIC = BIC(mnl_pooled)
    )
  },
  if (!is.null(mnl_within)) {
    data.frame(
      Model = "WithinDish",
      LogLik = as.numeric(logLik(mnl_within)),
      AIC = AIC(mnl_within),
      BIC = BIC(mnl_within)
    )
  },
  if (!is.null(mnl_cross)) {
    data.frame(
      Model = "CrossDish",
      LogLik = as.numeric(logLik(mnl_cross)),
      AIC = AIC(mnl_cross),
      BIC = BIC(mnl_cross)
    )
  }
)

write.csv(fit_table, "realdata_mnl_fit.csv", row.names = FALSE)

cat("Saved files:\n")
cat("- realdata_mnl_coefficients_detailed.csv\n")
cat("- realdata_mnl_coefficients.csv\n")
cat("- realdata_mnl_fit.csv\n")