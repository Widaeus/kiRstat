library(tidyverse)
library(survival)
library(fs)
library(broom)
library(survminer)
library(knitr)
library(kableExtra)

initiate <- function(output_path) {
# Ensure the directory exists
dir_path <- dirname(output_path)
if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
# Get the current date
today_date <- Sys.Date()
# Extract file name and extension
file_name <- basename(output_path)
file_extension <- tools::file_ext(file_name)
file_base <- tools::file_path_sans_ext(file_name)
# Remove any existing date from the file_base
file_base <- sub("_\\d{4}-\\d{2}-\\d{2}$", "", file_base)
# Construct the new file name with the current date
new_file_name <- paste0(file_base, "_", today_date, ".", file_extension)
new_output_path <- file.path(dir_path, new_file_name)
# Assuming 'jasa' is a data frame that you want to write to CSV
if (exists("jasa") && is.data.frame(jasa)) {
write.csv(jasa, new_output_path)
} else {
stop("Data frame 'jasa' does not exist or is not a data frame.")
}
}
# Define base directory
base_dir <- file.path(getwd(), "block5")
# Define output path
output_path <- file.path(base_dir, "data", paste0("jasa_", Sys.Date(), ".csv"))
initiate(output_path)
data <- read.csv(output_path)
save(data, file = file.path(base_dir, "data", paste0("jasa_", Sys.Date(), ".rda")))

glimpse(data)

# Convert character strings that are dates to Date type
data_cleaned <- data %>%
mutate(across(where(~ is.character(.) && any(!is.na(as.Date(.)))), as.Date))
# Convert all 0 and 1 integer columns (including those with NAs) to booleans
data_cleaned <- data_cleaned %>%
mutate(across(where(~ all(. %in% c(0, 1, NA)) && is.numeric(.)), ~ as.logical(.)))


data_cleaned <- data_cleaned %>%
mutate(across(where(~ n_distinct(.) < 10 && !is.logical(.)), as.factor))

data_cleaned %>%
select_if(is.numeric) %>%
select(-X) %>% # Remove the variable 'X'
gather(key = "variables", value = "values") %>%
ggplot(aes(x = values)) +
facet_wrap(~variables, scales = "free") +
geom_histogram(bins = 30)

data_cleaned %>%
select(where(is.logical)) %>%
pivot_longer(cols = everything(), names_to = "variables", values_to = "values") %>% #nolint
ggplot(aes(x = values)) +
facet_wrap(~variables, scales = "free_x") +
geom_bar() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

count_na_hla_a2 <- data_cleaned %>%
filter(!is.na(reject)) %>%
summarize(count = sum(is.na(hla.a2)))
print(count_na_hla_a2)

transplanted <- data_cleaned %>%
filter(transplant == 1)
surv_object <- Surv(time = transplanted$futime, event = transplanted$fustat)
# Fit Kaplan-Meier curve stratified by surgery
km_fit_surgery <- survfit(surv_object ~ surgery, data = transplanted)
# Plot survival curves
ggsurvplot(km_fit_surgery,
data = transplanted,
pval = TRUE,
conf.int = TRUE,
legend.labs = c("No Surgery", "Surgery"),
legend.title = "Surgery Status",
xlab = "Time",
ylab = "Survival Probability",
title = "Survival Curves Stratified by Surgery")

# Plot survival curves
ggsurvplot(km_fit_reject,
data = transplanted,
pval = TRUE,
conf.int = TRUE,
legend.labs = c("No Rejection", "Rejection"),
legend.title = "Rejection Status",
xlab = "Time",
ylab = "Survival Probability",
title = "Survival Curves Stratified by Rejection")

transplanted <- data_cleaned %>%
filter(transplant == 1)
# Create binary variables for age and time based on specified cutoffs
transplanted$age_bin <- ifelse(transplanted$age > 45, ">45", "<=45")
transplanted$wait_bin <- ifelse(transplanted$wait.time > 30, ">30", "<=30")
# Convert them to factors for the Cox model
transplanted$age_bin <- factor(transplanted$age_bin, levels = c("<=45", ">45"))
transplanted$wait_bin <- factor(transplanted$wait_bin, levels = c("<=30", ">30"))
# Univariable models
cox_age <- coxph(Surv(futime, fustat) ~ age_bin, data = transplanted)
cox_reject <- coxph(Surv(futime, fustat) ~ reject, data = transplanted)
cox_surgery <- coxph(Surv(futime, fustat) ~ surgery, data = transplanted)
cox_wait <- coxph(Surv(futime, fustat) ~ wait_bin, data = transplanted)
# Multivariable model
cox_multivariable <- coxph(Surv(futime, fustat) ~ age_bin + reject + surgery + wait_bin, data = transplanted)

ph_test <- cox.zph(cox_multivariable)
print(ph_test)

cox_adjusted_td <- coxph(Surv(futime, fustat) ~ age_bin + surgery + wait_bin + reject + tt(reject),
data = transplanted,
tt = function(x, time, ...) x * time)

transplanted$age_bin <- ifelse(transplanted$age > 45, ">45", "<=45")
transplanted$wait_bin <- ifelse(transplanted$wait.time > 30, ">30", "<=30")
transplanted$age_bin <- factor(transplanted$age_bin)
transplanted$surgery <- factor(transplanted$surgery)
transplanted$wait_bin <- factor(transplanted$wait_bin)
transplanted$reject <- factor(transplanted$reject, levels = c("FALSE", "TRUE"))

# Models
cox_age <- coxph(Surv(futime, fustat) ~ age_bin, data = transplanted)
cox_reject <- coxph(Surv(futime, fustat) ~ reject, data = transplanted)
cox_surgery <- coxph(Surv(futime, fustat) ~ surgery, data = transplanted)
cox_wait <- coxph(Surv(futime, fustat) ~ wait_bin, data = transplanted)
variables <- c("age_bin", "surgery", "wait_bin")
# Function to extract HR, CI, and p-value from Cox models
extract_unadjusted <- function(var) {
formula <- as.formula(paste("Surv(futime, fustat) ~", var))
model <- coxph(formula, data = transplanted)
summary_model <- summary(model)
coef <- summary_model$coefficients[1, ]
conf_int <- summary_model$conf.int[1, c("lower .95", "upper .95")]
hr <- coef["exp(coef)"]
lower95 <- conf_int["lower .95"]
upper95 <- conf_int["upper .95"]
pvalue <- coef["Pr(>|z|)"]
hr_ci <- paste0(round(hr, 2), " (", round(lower95, 2), ", ", round(upper95, 2), ")")
data.frame(
Variable = var,
`Unadjusted HR (95% CI)` = hr_ci,
`Unadjusted P-value` = round(pvalue, 2),
stringsAsFactors = FALSE
)
}
# Generate unadjusted results
unadjusted_results <- do.call(rbind, lapply(variables, extract_unadjusted))
adjusted_vars <- c("age_bin", "surgery", "wait_bin")
adjusted_formula <- as.formula(paste("Surv(futime, fustat) ~", paste(adjusted_vars, collapse = " + ")))
adjusted_model <- coxph(adjusted_formula, data = transplanted)
# Function to extract adjusted HRs
extract_adjusted <- function(var) {
formula <- as.formula(paste("Surv(futime, fustat) ~", paste(adjusted_vars, collapse = " + ")))
adjusted_model <- coxph(formula, data = transplanted)
summary_model <- summary(adjusted_model)
coef_names <- rownames(summary_model$coefficients)
var_coefs <- coef_names[startsWith(coef_names, var)]

result <- do.call(rbind, lapply(var_coefs, function(coef_name){
coef_info <- summary_model$coefficients[coef_name, ]
conf_int <- summary_model$conf.int[coef_name, c("lower .95", "upper .95")]
hr <- coef_info["exp(coef)"]
lower95 <- conf_int["lower .95"]
upper95 <- conf_int["upper .95"]
pvalue <- coef_info["Pr(>|z|)"]
hr_ci <- paste0(round(hr, 2), " (", round(lower95, 2), ", ", round(upper95, 2), ")")
var_level <- sub(paste0("^", var), "", coef_name)
var_display <- ifelse(var_level != "", paste0(var, var_level), var)
data.frame(
Variable = var_display,
`Adjusted HR (95% CI)` = hr_ci,
`Adjusted P-value` = round(pvalue, 2),
stringsAsFactors = FALSE
)
}))
return(result)
}
# Generate adjusted results
adjusted_results <- do.call(rbind, lapply(adjusted_vars, extract_adjusted))
transplanted$reject_num <- ifelse(transplanted$reject == "TRUE", 1, 0)
adjusted_td_formula <- as.formula("Surv(futime, fustat) ~ age_bin + surgery + wait_bin + reject_num + tt(reject_num)")
# Fit the Cox model
adjusted_td_model <- coxph(
adjusted_td_formula,
data = transplanted,
tt = function(x, time, ...) x * time
)
coef_td <- coef(adjusted_td_model)
cov_td <- vcov(adjusted_td_model)
# Extract coefficients
coef_reject <- coef_td["reject_num"]
coef_tt_reject <- coef_td["tt(reject_num)"]
# Extract variances and covariance
var_reject <- cov_td["reject_num", "reject_num"]
var_tt_reject <- cov_td["tt(reject_num)", "tt(reject_num)"]

cov_reject_tt <- cov_td["reject_num", "tt(reject_num)"]
# Time points of interest
time_points <- c(180, 365, 730)
# Compute HRs at each time point
td_results <- do.call(rbind, lapply(time_points, function(t) {
lp <- coef_reject + coef_tt_reject * t
var_lp <- var_reject + (t^2) * var_tt_reject + 2 * t * cov_reject_tt
se_lp <- sqrt(var_lp)
hr <- exp(lp)
lower95 <- exp(lp - 1.96 * se_lp)
upper95 <- exp(lp + 1.96 * se_lp)
# z-score and p-value
z <- lp / se_lp
pvalue <- 2 * (1 - pnorm(abs(z)))
# Format HR and CI
hr_ci <- paste0(round(hr, 2), " (", round(lower95, 2), ", ", round(upper95, 2), ")")
data.frame(
Variable = paste0("Rejection (", t, " days)"),
`Adjusted HR (95% CI)` = hr_ci,
`Adjusted P-value` = round(pvalue, 2),
`Unadjusted HR (95% CI)` = NA,
`Unadjusted P-value` = NA,
stringsAsFactors = FALSE
)
}))
# Step 4: Combine All Results
variable_names_unadjusted <- c(
"age_bin" = "Age",
"surgery" = "Surgery",
"wait_bin" = "Wait Time"
)
variable_names_adjusted <- c(
"age_bin>45" = "Age",
"surgeryTRUE" = "Surgery",
"wait_bin>30" = "Wait Time"
)
unadjusted_results$Variable <- variable_names_unadjusted[unadjusted_results$Variable]
adjusted_results$Variable <- variable_names_adjusted[adjusted_results$Variable]

combined_results <- merge(
unadjusted_results,
adjusted_results,
by = "Variable",
all = TRUE
)
td_results <- td_results[, c(
"Variable",
"Unadjusted.HR..95..CI.",
"Unadjusted.P.value",
"Adjusted.HR..95..CI.",
"Adjusted.P.value"
)]
final_results <- rbind(combined_results, td_results)
final_results$Variable <- as.character(final_results$Variable)
desired_order <- c(
"Age",
"Surgery",
"Wait Time",
"Rejection",
"Rejection (180 days)",
"Rejection (365 days)",
"Rejection (730 days)"
)
final_results$Variable <- factor(final_results$Variable, levels = desired_order)
final_results <- final_results[order(final_results$Variable), ]
rownames(final_results) <- NULL
colnames(final_results) <- c("Variable", "Unadjusted HR (95% CI)", "Unadjusted P-value",
"Adjusted HR (95% CI)", "Adjusted P-value")
kable(final_results,
align = "c",
row.names = FALSE,
booktabs = TRUE) %>%
kable_styling(
font_size = 8,
full_width = FALSE,
position = "center",
latex_options = c("scale_down")
) %>%
row_spec(0, bold = TRUE) %>%
column_spec(1, bold = TRUE) %>%
row_spec(
1:nrow(final_results),
extra_css = "padding-top: 4px; padding-bottom: 4px;"
)