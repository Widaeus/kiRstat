# Loading functions.
r_files <- list.files("block5/utils", pattern = "\\.r$", full.names = TRUE)
for (file in r_files) {
  source(file)
}
# Creating csv file and data.
output_path <- "block5/data/jasa.csv"
initiate(output_path)
# Read in data in R environment.
data <- read.csv(paste0("block5/data/jasa_", Sys.Date(), ".csv"))
# Saving data as .rda file.
save(data, file = paste0("block5/data/jasa_", Sys.Date(), ".rda"))

### Attention: If open data "jasa" not available, load it from the .rda file ###

# Starting EDA
glimpse(data)


# Convert character strings that are dates to Date type
data_cleaned <- data %>%
  mutate(across(where(~ is.character(.) && any(!is.na(as.Date(.)))), as.Date))

# Convert all 0 and 1 integer columns (including those with NAs) to booleans
data_cleaned <- data_cleaned %>%
  mutate(across(where(~ all(. %in% c(0, 1, NA)) && is.numeric(.)), ~ as.logical(.)))

# Faktorisera alla kolumner som har färre än 10 diskreta variabler, men inte är logical.

data_cleaned <- data_cleaned %>%
  mutate(across(where(~ n_distinct(.) < 10 && !is.logical(.)), as.factor))

####### Initial cleaning done #######

# Check distributiuons of numeric values
data_cleaned %>%
  select_if(is.numeric) %>%
  select(-X) %>%  # Remove the variable 'X'
  gather(key = "variables", value = "values") %>%
  ggplot(aes(x = values)) +
  facet_wrap(~variables, scales = "free") +
  geom_histogram(bins = 30)

# Boxplots for numeric variables to spot outliers
data_cleaned %>%
  select_if(is.numeric) %>%
  gather(key = "variables", value = "values") %>%
  ggplot(aes(x = variables, y = values)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Correlation matrix for numeric variables
data_cleaned %>%
  select_if(is.numeric) %>%
  cor() %>%
  as_tibble(rownames = "Variable") %>%
  pivot_longer(-Variable, names_to = "Variable2", values_to = "Correlation") %>%
  ggplot(aes(x = Variable, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_viridis_c()

# Scatter plots for pairs of variables
pairs(data_cleaned %>% select_if(is.numeric))

# Categorical data analysis
data_cleaned %>%
  select(where(is.logical)) %>%
  pivot_longer(cols = everything(), names_to = "variables", values_to = "values") %>% #nolint
  ggplot(aes(x = values)) +
  facet_wrap(~variables, scales = "free_x") +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

count_na_hla_a2 <- data_cleaned %>%
filter(is.na(reject)) %>%
summarize(count = sum(is.na(hla.a2)))
print(count_na_hla_a2)

count_na_hla_a2_not_reject_na <- data_cleaned %>%
  filter(!is.na(reject)) %>%
  summarize(count = sum(is.na(hla.a2)))

print(count_na_hla_a2_not_reject_na)

sum(data$transplant)
sum(data$surgery)

# Load necessary libraries
library(survminer)
library(broom)
library(survival)

transplanted <- data_cleaned %>%
  filter(transplant == 1)

# Fit the Cox proportional hazards model
cox_model <- coxph(Surv(futime, fustat) ~ surgery + age + reject, data = transplanted)

cox_zph <- cox.zph(cox_model)
print(cox_zph)
ggcoxdiagnostics(cox_model, type = "martingale", linear.predictions = FALSE)

## time varying
# Adding a time-varying effect for 'reject'
# Assume 'time' is the follow-up time variable; replace 'futime' with your actual time variable if different
cox_model_time_varying <- coxph(Surv(futime, fustat) ~ surgery + age + reject + tt(reject), data = transplanted,
                                tt = function(x, t, ...) x * log(t + 1))
