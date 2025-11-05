# ======================================================
# Load Packages & Prepare Environment

# Automatically install missing packages
install_if_missing <- function(pkgs, repo = "https://cloud.r-project.org") {
  to_install <- pkgs[!(pkgs %in% rownames(installed.packages()))]
  if (length(to_install)) install.packages(to_install, repos = repo)
}

# Package list from Heming's Code
packages <- c(
  "tidyverse", "haven", "furniture", "sjlabelled", "skimr", "naniar",
  "nephro", "foreign", "patchwork", "corrplot", "plotmo", "arsenal", 
  "lme4", "ModelMetrics", "MuMIn", "optimx", "minqa", "survival", 
  "survminer", "lubridate", "ggfortify", "splines", "viridis", 
  "psych", "stargazer", "broom.mixed", "GGally", "DescTools", "gt", 
  "xtable", "gridExtra", "extrafont", "ggsci", "epiR", "cowplot", "float",
  "tidyr", "tidyselect", "gtsummary", "flextable", "gtExtras", "ggExtra"
)

# Install missing packages
install_if_missing(packages)

# Install furniture from GitHub if not available on CRAN
if (!requireNamespace("furniture", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("tysonbarrett/furniture")
}
# Load all packages
for (pkg in packages) {
  suppressPackageStartupMessages({
    library(pkg, 
            character.only = TRUE, 
            quietly = TRUE, 
            warn.conflicts = FALSE)
  })
}

# Load furniture separately
suppressPackageStartupMessages(library(furniture))

# Display successfully loaded packages
loaded_pkgs <- packages[packages %in% loadedNamespaces()]
cat("\n Successfully loaded packages:\n", 
    paste(loaded_pkgs, 
          collapse = ", "), 
    "\n")

# ======================================================
# Load RAND Data and Explore Variables

rand <- read_dta("~/Library/CloudStorage/OneDrive-共享的库-ColumbiaUniversityIrvingMedicalCenter/Belsky, Daniel - Zhengfei_Bian/Rand/randhrs1992_2022v1.dta")
# tracker <- read_dta("~/Library/CloudStorage/OneDrive-共享的库-ColumbiaUniversityIrvingMedicalCenter/Belsky, Daniel - Zhengfei_Bian/Rand/trk2022tr_r.dta")
# Check key ID variables
names(rand)[grepl("hhidpn", names(rand), ignore.case = TRUE)]
# names(tracker)[grepl("hhidpn", names(tracker), ignore.case = TRUE)]

# Search by topic
names(rand)[grepl("hhidpn", names(rand), ignore.case = TRUE)]
grep("educ|age|gender|smoke|race", names(rand), value = TRUE)
names(rand) %>% grep("age", ., value = TRUE)
grep("gender", names(rand), value = TRUE)
grep("race|hispan", names(rand), value = TRUE)
grep("educ", names(rand), value = TRUE)
grep("urban|rural|metro|region|city", names(rand), value = TRUE)
grep("smoke", names(rand), value = TRUE)
grep("bmi|weight|height", names(rand), value = TRUE)
grep("house|home|hous|housing|quality", names(rand), value = TRUE)

# View value labels
sjlabelled::val_labels(rand$rahispan)
sjlabelled::val_labels(rand$raracem)
sjlabelled::val_labels(rand$raeduc)
sjlabelled::val_labels(rand$r16lbsatcity)
lapply(rand[c("r16smokev", "r16smoken")], sjlabelled::val_labels)
sjlabelled::val_labels(rand$r16lbhouseprb)

# ======================================================
# Derive Variables
rand <- rand %>%
  mutate(
    # Age
    age = as.numeric(r16agey_e),
    
    # Sex (women is 0/1, used only for percentage calculations)
    sex   = factor(if_else(ragender == 2, "Women", "Men"), levels = c("Men","Women")),
    women = if_else(ragender == 2, 1, 0, missing = NA_real_),
    
    # Race/Ethnicity
    race = case_when(
      rahispan == 0 & raracem == 1 ~ "White",
      rahispan == 0 & raracem == 2 ~ "Black",
      rahispan == 1                ~ "Hispanic",
      rahispan == 0 & raracem == 3 ~ "Other",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("White","Black","Hispanic","Other")),
    
    # Education
    edu = case_when(
      raeduc == 5          ~ "College and above",
      raeduc %in% c(2,3,4) ~ "High-school graduate",
      raeduc == 1          ~ "Left high school",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("College and above","High-school graduate","Left high school")),
    
    # Age group
    age_group = factor(if_else(r16agey_e < 65, "Less than 65", "65 and over"),
                       levels = c("Less than 65","65 and over")),
    
    # Age - 65
    age_minus_65 = if_else(!is.na(r16agey_e), r16agey_e - 65, NA_real_),
    
    # Smoking status
    smoke = case_when(
      r16smokev == 0                  ~ "Never",
      r16smokev == 1 & r16smoken == 0 ~ "Former",
      r16smoken == 1                  ~ "Current",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Never","Former","Current")),
    
    # BMI category
    bmi_cat = case_when(
      r16bmi >= 18.5 & r16bmi < 25 ~ "Normal",
      r16bmi >= 25   & r16bmi < 30 ~ "Overweight",
      r16bmi >= 30                 ~ "Obese",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Normal","Overweight","Obese")),
    
    # Housing quality
    housing_problem = factor(
      case_when(
        r16lbhouseprb %in% c(1, 2) ~ "No problem",
        r16lbhouseprb %in% c(3, 4) ~ "Some problem",
        TRUE ~ NA_character_
      ),
      levels = c("No problem", "Some problem")
    )
  )

# Summary
# ======================================================
# Descriptive Summary 
age_mean <- round(mean(rand$age, na.rm = TRUE), 4)

age_sd   <- round(sd(rand$age, na.rm = TRUE), 4)

women_pct <- round(mean(rand$women, na.rm = TRUE) * 100, 4)

# to compute % tables
factor_pct_table <- function(fct_var, var_name) {
  tb <- prop.table(table(fct_var, useNA = "no")) * 100
  tibble::tibble(
    Variable = var_name,
    Level    = names(tb),
    Percent  = round(as.numeric(tb), 4)
  )
}

# Percentage tables
race_tab     <- factor_pct_table(rand$race, "Race")
edu_tab      <- factor_pct_table(rand$edu, "Education")
smoke_tab    <- factor_pct_table(rand$smoke, "Smoking")
bmi_tab      <- factor_pct_table(rand$bmi_cat, "BMI")
housing_tab  <- if ("housing_problem" %in% names(rand)) {
  factor_pct_table(rand$housing_problem, "Housing problems")
} else NULL

# Age row (Mean/SD)
age_row <- tibble::tibble(
  Variable = "Age",
  Level    = "Mean (SD)",
  Percent  = NA_real_,
  Mean     = age_mean,
  SD       = age_sd
)

# Women/Men % table
women_tab <- factor_pct_table(
  factor(ifelse(rand$women == 1, "Women", "Men"), levels = c("Women","Men")),
  "Women"
)

# Bind all into a descriptive table
desc_expanded <- dplyr::bind_rows(
  age_row,
  women_tab  %>% dplyr::mutate(Mean = NA_real_, SD = NA_real_),
  race_tab   %>% dplyr::mutate(Mean = NA_real_, SD = NA_real_),
  edu_tab    %>% dplyr::mutate(Mean = NA_real_, SD = NA_real_),
  smoke_tab  %>% dplyr::mutate(Mean = NA_real_, SD = NA_real_),
  bmi_tab    %>% dplyr::mutate(Mean = NA_real_, SD = NA_real_),
  if (!is.null(housing_tab)) housing_tab %>% dplyr::mutate(Mean = NA_real_, SD = NA_real_) else NULL
) %>%
  dplyr::relocate(Variable, Level, Percent, Mean, SD) %>%
  # Final guard: ensure all numeric columns are rounded to 4 decimals
  dplyr::mutate(
    Percent = ifelse(is.na(Percent), NA_real_, round(Percent, 4)),
    Mean    = ifelse(is.na(Mean),    NA_real_, round(Mean,    4)),
    SD      = ifelse(is.na(SD),      NA_real_, round(SD,      4))
  )

desc_expanded

# ======================================================
# Continuous BMI 
# r16pmbmi preferred, fallback to r16bmi
if ("r16pmbmi" %in% names(rand)) {
  message("Using r16pmbmi as continuous BMI")
  rand <- rand %>% dplyr::mutate(bmi_cont = as.numeric(r16pmbmi))
} else if ("r16bmi" %in% names(rand)) {
  message("Using r16bmi as continuous BMI")
  rand <- rand %>% dplyr::mutate(bmi_cont = as.numeric(r16bmi))
} else {
  stop("No BMI variable found (r16pmbmi or r16bmi).")
}

# Exclude implausible values; compute mean/SD 
rand <- rand %>%
  dplyr::mutate(bmi_cont = dplyr::if_else(bmi_cont < 10 | bmi_cont > 80, NA_real_, bmi_cont))

bmi_mean <- round(mean(rand$bmi_cont, na.rm = TRUE), 4)
bmi_sd   <- round(sd(rand$bmi_cont,   na.rm = TRUE), 4)

bmi_cont_row <- tibble::tibble(
  Variable = "BMI (continuous)",
  Level    = "Mean (SD)",
  Percent  = NA_real_,
  Mean     = bmi_mean,
  SD       = bmi_sd
)

# Append to descriptive summary 
desc_expanded <- dplyr::bind_rows(desc_expanded, bmi_cont_row) %>%
  dplyr::mutate(
    Percent = ifelse(is.na(Percent), NA_real_, round(Percent, 4)),
    Mean    = ifelse(is.na(Mean),    NA_real_, round(Mean,    4)),
    SD      = ifelse(is.na(SD),      NA_real_, round(SD,      4))
  )

desc_expanded


