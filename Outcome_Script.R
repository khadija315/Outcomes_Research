# Load packages ----
library(labelled)
library(dplyr) 
library(VIM)  # For KNN imputation
library(tableone)



# Import data and define variables  ----
data<-read.table("clipboard", header=TRUE, sep="\t",dec=",", na.string="")
str(data)
str(data$PREOPVURGRADE) # To check the nature of a variable
 data$AGS <-as.numeric (data$AGS)
 data$BMI <-as.numeric (data$BMI)
 data$BLDLOSS <-as.numeric (data$BLDLOSS)
 data$CATHPERIOD <-as.numeric (data$CATHPERIOD)
 data$FUPm <-as.numeric (data$FUPm)
 data$HOSPTIME <-as.numeric (data$HOSPTIME)
 data$OPRTIME <-as.numeric (data$OPRTIME)
 data$UTIpostopFup <-as.numeric (data$UTIpostopFup)
 data$UTIpostopT <-as.numeric (data$UTIpostopT)
 data$PREOPVURGRADE <-as.factor (data$PREOPVURGRADE)
 data$HISTOINJ <-as.factor (data$HISTOINJ)
 data$PREOPRNPLN <-as.factor (data$PREOPRNPLN)
 data$PREOPRNSCR <-as.factor (data$PREOPRNSCR)
 data$VURSIDE <-as.factor (data$VURSIDE)
 data$VURPHASE <-as.factor (data$VURPHASE)
 data$PREOPEPISODEn <-as.factor (data$PREOPEPISODEn)
 data$PREOPTYPEUn <-as.factor (data$PREOPTYPEUn)
 data$SURGTECHn2 <-as.factor (data$SURGTECHn2)
 data$TRTn2 <-as.factor (data$TRTn2)
 data$ANASTOMO <-as.factor (data$ANASTOMO)
 data$BLDCOMPLEAK <-as.factor (data$BLDCOMPLEAK)
 data$BLDCOMPRETN <-as.factor (data$BLDCOMPRETN)
 data$BLDCOMPVOID <-as.factor (data$BLDCOMPVOID)
 data$BLDSPASM <-as.factor (data$BLDSPASM)
 data$CATH <-as.factor (data$CATH)
 data$Center <-as.factor (data$Center)
 data$CLAVIENCLASS <-as.factor (data$CLAVIENCLASS)
 data$EPIDURAL <-as.factor (data$EPIDURAL)
 data$GENDER <-as.factor (data$GENDER)
 data$GU30 <-as.factor (data$GU30)
 data$GU90 <-as.factor (data$GU90)
 data$GUER30 <-as.factor (data$GUER30)
 data$GUER90 <-as.factor (data$GUER90)
 data$HEMAT <-as.factor (data$HEMAT)
 data$ILEUS <-as.factor (data$ILEUS)
 data$INCCOMPBLEED <-as.factor (data$INCCOMPBLEED)
 data$INCCOMPINF <-as.factor (data$INCCOMPINF)
 data$INCCOMPSCAR <-as.factor (data$INCCOMPSCAR)
 data$NARC <-as.factor (data$NARC)
 data$NONARC <-as.factor (data$NONARC)
 data$PAIN <-as.factor (data$PAIN)
 data$PROC365 <-as.factor (data$PROC365)
 data$STENT <-as.factor (data$STENT)
 data$UDR7M <-as.factor (data$UDR7M)
 data$URCOMPCVUR <-as.factor (data$URCOMPCVUR)
 data$URCOMPINJ <-as.factor (data$URCOMPINJ)
 data$URCOMPOBST <-as.factor (data$URCOMPOBST)
 data$URCOMPSTEN <-as.factor (data$URCOMPSTEN)
 data$UTIpostop <-as.factor (data$UTIpostop)
 data$SURGIND <-as.factor (data$SURGIND)
 data$RECGRADE_MAX <-as.factor (data$RECGRADE_MAX)
 data$REC011 <-as.factor (data$REC011)

 
# Label variables ----
var_label(data$Case) <- "Case"
var_label(data$AGS) <- "Age at surgery, years"
var_label(data$ANASTOMO) <- "Anastomotic stricture"
var_label(data$BLDCOMPLEAK) <- "Urinary leakage"
var_label(data$BLDCOMPRETN) <- "Acute urinary retention"
var_label(data$BLDCOMPVOID) <- "Voiding dysfunction"
var_label(data$BLDLOSS) <- "blood loss, mL"
var_label(data$BLDSPASM) <- "1-week bladder spasms"
var_label(data$BMI) <- "BMI, kg/m2"
var_label(data$CATH) <- "Catheter placement"
var_label(data$CATHPERIOD) <- "Urethral catheterization duration, days"
var_label(data$Center) <- "Hospital center"
var_label(data$CLAVIENCLASS) <- "Severity of complications"
var_label(data$EPIDURAL) <- "Use of epidural analgesia"
var_label(data$FUPm) <- "Follow up, months"
var_label(data$GENDER) <- "Gender"
var_label(data$GU30) <- "30-day GU readmissions"
var_label(data$GU90) <- "90-day GU readmissions"
var_label(data$GUER30) <- "30-day GU ER visits"
var_label(data$GUER90) <- "90-day GU ER visits"
var_label(data$HEMAT) <- "Gross hematuria"
var_label(data$HISTOINJ) <- "Prior injection"
var_label(data$HOSPTIME) <- "Hospitalization time, days"
var_label(data$ILEUS) <- "Short-term restricted bowel activity"
var_label(data$INCCOMPBLEED) <- "Incision bleeding/bruising"
var_label(data$INCCOMPINF) <- "Infection of the incision"
var_label(data$INCCOMPSCAR) <- "Incisional hernia"
var_label(data$NARC) <- "Narcotics"
var_label(data$NONARC) <- "Non-narcotics"
var_label(data$OPRTIME) <- "Operative time, minutes"
var_label(data$PAIN) <- "1-day postoperative pain score"
var_label(data$PREOPEPISODEn) <- "Nb of preoperative UTIs"
var_label(data$PREOPRNPLN) <- "Preoperative APN"
var_label(data$PREOPRNSCR) <- "Preoperative renal scarring"
var_label(data$PREOPTYPEUn) <- "Type of preoperative UTIs"
var_label(data$PREOPVURGRADE) <- "VUR Grade"
var_label(data$PROC365) <- "365-day reoperations"
var_label(data$STENT) <- "Ureteral stent placement"
var_label(data$SURGTECHn2) <- "Surgeon experience, nb of prior similar operations"
var_label(data$TRTn2) <- "Treatment group"
var_label(data$UDR7M) <- "Ureteral dilation"
var_label(data$URCOMPCVUR) <- "Contralateral reflux"
var_label(data$URCOMPINJ) <- "Ureteral injury"
var_label(data$URCOMPOBST) <- "Acute ureteral obstruction"
var_label(data$URCOMPSTEN) <- "Ureteral stenosis"
var_label(data$UTIpostop) <- "Postoperative UTIs"
var_label(data$UTIpostopFup) <- "Follow up time to postoperative UTIs"
var_label(data$UTIpostopT) <- "Time to postoperative UTIs"
var_label(data$VURPHASE) <- "VUR timing"
var_label(data$VURSIDE) <- "VUR side"
var_label(data$SURGIND) <- "Indication for VUR Surgical Management"
var_label(data$RECGRADE_MAX) <- "VUR recurrence defined as Grades 0–1 (no recurrence) at VCUG performed at 12 months or after"
var_label(data$REC011) <- "VUR recurrence Grade>1 at VCUG performed at 12 months or after"

var_label(data$VURSIDE)  # Check the label
str(data$TRTn2)

# Code variables ----
data$Center <- recode(data$Center, `1` = "Al-Zahraa hospital", `2` = "Dresden Hospital",`3` = "Hotel Dieu Hospital",`4` = "AUBMC" ,`5` = "India" )
data$GENDER <- recode(data$GENDER, `0` = "Male", `1` = "Female")
data$UDR7M <- recode(data$UDR7M, `0` = "< 7 mm", `1` = "≥ 7mm")
data$STENT <- recode(data$STENT, `0` = "No", `1` = "Yes")
data$CATH  <- recode(data$CATH, `0` = "No", `1` = "Yes")
data$NARC <- recode(data$NARC, `0` = "No", `1` = "Yes")
data$NONARC <- recode(data$NONARC, `0` = "No", `1` = "Yes")
data$PAIN <- recode(data$PAIN , `0` = "No pain", `1` = "Mild pain", `2` = "Moderate pain",`3` = "Severe pain" )
data$BLDSPASM <- recode(data$BLDSPASM, `0` = "No", `1` = "Yes")
data$HEMAT <- recode(data$HEMAT, `0` = "No", `1` = "Yes")
data$BLDCOMPRETN <- recode(data$BLDCOMPRETN, `0` = "No", `1` = "Yes")
data$BLDCOMPVOID <- recode(data$BLDCOMPVOID, `0` = "No", `1` = "Yes")
data$BLDCOMPLEAK <- recode(data$BLDCOMPLEAK, `0` = "No", `1` = "Yes")
data$URCOMPOBST <- recode(data$URCOMPOBST, `0` = "No", `1` = "Yes")
data$URCOMPINJ <- recode(data$URCOMPINJ, `0` = "No", `1` = "Yes")
data$URCOMPSTEN <- recode(data$URCOMPSTEN, `0` = "No", `1` = "Yes")
data$INCCOMPBLEED <- recode(data$INCCOMPBLEED, `0` = "No", `1` = "Yes")
data$INCCOMPINF <- recode(data$INCCOMPINF, `0` = "No", `1` = "Yes")
data$INCCOMPSCAR <- recode(data$INCCOMPSCAR, `0` = "No", `1` = "Yes")
data$ILEUS <- recode(data$ILEUS, `0` = "No", `1` = "Yes")
data$CLAVIENCLASS  <- recode(data$CLAVIENCLASS , `1` = "Clavien grade 1", `2` = "Clavien grade 2",`3` = "Clavien grade 3",`4` = "Clavien grade 4" ,`5` = "Clavien grade 5" )
data$GU30 <- recode(data$GU30, `0` = "No", `1` = "Yes")
data$GU90 <- recode(data$GU90, `0` = "No", `1` = "Yes")
data$GUER30 <- recode(data$GUER30, `0` = "No", `1` = "Yes")
data$GUER90 <- recode(data$GUER90, `0` = "No", `1` = "Yes")
data$PROC365 <- recode(data$PROC365, `0` = "No", `1` = "Yes")
data$TRTn2 <- recode(data$TRTn2, `0` = "Cohen group", `1` = "Lich-Gregoir group")
data$EPIDURAL<- recode(data$EPIDURAL, `0` = "No", `1` = "Yes")
data$HISTOINJ <- recode(data$HISTOINJ, `0` = "No", `1` = "Yes")
data$PREOPVURGRADE  <- recode(data$PREOPVURGRADE , `1` = "III", `2` = "IV",`3` = "V" )
data$VURSIDE <- recode(data$VURSIDE, `0` = "Left", `1` = "Right")
data$VURPHASE <- recode(data$VURPHASE, `1` = "At voiding", `2` = "At filling",`3` = "At filling and voiding" )
data$PREOPTYPEUn <- recode(data$PREOPTYPEUn, `0` = "Afebrile", `1` = "Febrile")
data$PREOPEPISODEn <- recode(data$PREOPEPISODEn, `1` = "1", `2` = "2",`3` = "≥ 3" )
data$PREOPRNPLN <- recode(data$PREOPRNPLN, `0` = "No", `1` = "Yes")
data$PREOPRNSCR <- recode(data$PREOPRNSCR, `0` = "No scarring", `1` = "Any scarring")	
data$ANASTOMO <- recode(data$ANASTOMO, `0` = "No", `1` = "Yes")
data$SURGTECHn2  <- recode(data$SURGTECHn2 , `1` = "≥ 100", `2` = "50 - 100",`3` = "<50" )	
data$URCOMPCVUR <- recode(data$URCOMPCVUR, `0` = "No", `1` = "Yes")
data$UTIpostop <- recode(data$UTIpostop, `0` = "No UTI", `1` = "UTI")
data$SURGIND <- recode(data$SURGIND,
  `1` = "Poor compliance to medical treatment",
  `2` = "Children with ineffective or poorly tolerated CAP",
  `3` = "Symptomatic high-grade reflux (III-V)",
  `4` = "Repeated breakthrough UTI",
  `5` = "Girls with persistent VUR after puberty",
  `6` = "VUR recurrence after endoscopic injection",
  `7` = "Reflux nephropathy",
  `8` = "Presence of new renal scars on DMSA",
  `9` = "Worsening in renal function",
  `10` = "Parental preference",
  `11` = "Persistent reflux >2 years"
)
data$RECGRADE_MAX <- recode(data$RECGRADE_MAX,
  `0` = "Grade 0",
  `1` = "Grade1", 
  `2` = "Grade2",
  `3` = "Grade3",
  `4` = "Grade 4",
  `5` = "Grade 5",
  `NA` = "VCUG not performed"
)
data$REC011 <- recode(data$REC011, `0` = "No recurrence", `1` = "Recurrence", `N/A` = "missing")


# Data Imputation ----

# Missingness in the dataset
m_outcome <- colSums(is.na(Outcome)) / nrow(Outcome) * 100
print(m_outcome)

# KNN imputation for missing values using KNN (k-nearest neighbors)
Outcome_knn <- Outcome # create a temporary dataset copy for KNN using scaled variables

# Convert categorical variables
cat_vars <- c(
  "GENDER","PREOPVURGRADE", "VURPHASE","VURSIDE",
  "UDR7M","PREOPRNSCR","HISTOINJ","SURGIND",
  "CENTER","TRT2", "SURGTECH2",
  "PREOPEPISODE1", "PREOPRNPLN","PREOPTYPEU1"
)
Outcome_knn[cat_vars] <- lapply(Outcome_knn[cat_vars], as.factor)

# Scale continuous variables
continuous_vars <- c("AGS", "BMI", "YEARS")
Outcome_knn[continuous_vars] <- scale(Outcome_knn[continuous_vars])


# KNN imputation without subsetting the dataset
Outcome_knn <- kNN(
  Outcome_knn,
  variable = c("PREOPEPISODE1", "PREOPRNPLN", "PREOPTYPEU1"),  # Variables to impute
  dist_var = c("AGS","GENDER","BMI", # Variables used to compute patient similarity
               "PREOPVURGRADE","VURPHASE","VURSIDE","UDR7M",
               "PREOPRNSCR","HISTOINJ","SURGIND","YEARS",
               "TRT2"
  ),
  k = 5,
  imp_var = FALSE 
# FALSE generates a clean dataset
# TRUE adds missingness indicators 
# (a new column as *_imp with *=original variable name, with two levels: 
# 0 indicates original observed value, and 1 indicates value was imputed by KNN) 
# to track what was imputed. 
# This helps in comparing imputed vs non-imputed 
# to see if missingness is systematic.
)


# CENTER + TRT2 + SURGTECH2 together may overweight institutional structure, 
# which means clustering patients by institution rather than biology
# → keep only one of them, I will keep "TRT2".

Out <- Outcome   # New dataset for analysis including imputed variables 
Out$PREOPEPISODE1 <- Outcome_knn$PREOPEPISODE1
Out$PREOPRNPLN   <- Outcome_knn$PREOPRNPLN
Out$PREOPTYPEU1  <- Outcome_knn$PREOPTYPEU1


## Imputation Rationale ----
# Ensuring that relationships between variables were preserved:
# - Continuous variables were normalized prior to imputation.
# - Categorical variables were treated according to their levels.
# - KNN is a reasonable imputation method, particularly when
#   simplicity and preservation of variable relationships are priorities.
# - KNN imputation is a good choice when:
#     * relationships among variables are important,
#     * multiple imputation is unavailable or inappropriate,
#     * missingness is moderate (e.g., <20%),
#     * and missing data are distributed across several variables.


## Note 1 ----
# Imputation can be done without scaling using VIM package. 
# With scaling, consider that scaling should affect ONLY distance calculation, 
# not the final analytic dataset.

## Note 2 ----
# `dist_var` should be carefully chosen. 
# The variables that should not be included are:
#     * Patient identifiers
#     * Postoperative variables (data leakage)
#     * Treatment effect variables (post-decision)
# The variables that should be included:
#     * Demographics
#     * Disease severity / anatomy
#     * Preoperative renal / infection status
#     * Clinical context (SURGIND, YEARS)
# Variables to use cautiously:
#     * CENTER → may introduce institutional clustering (can be good OR harmful)
#     * SURGTECH2 → surgeon experience (ok if relevant, but can bias similarity)
#     * TRT2 → treatment modality (depends on whether imputation should reflect treatment groups)

## Note 3 ----
# Variables to impute can be included in `dist_var` 
# but this makes imputation partly self-informative.


# Recode and create (composite) variables ----

# Recode SURGIND into a new variable SURGIND2 with grouped categories
# This is a dplyr::case_when() problem

Out <- Out %>%
  mutate(
    SURGIND2 = case_when(
      SURGIND == 4 ~ 1,
      SURGIND == 3 ~ 2,
      SURGIND == 1 ~ 3,
      SURGIND == 2 ~ 4,
      SURGIND %in% c(5, 6, 7, 8, 9, 10, 11) ~ 5,
      TRUE ~ NA_real_
    )
  )

# adds labels for clarity
Out <- Out %>%
  mutate(
    SURGIND2 = case_when(
      SURGIND == 4 ~ 1,
      SURGIND == 3 ~ 2,
      SURGIND == 1 ~ 3,
      SURGIND == 2 ~ 4,
      SURGIND %in% c(5, 6, 7, 8, 9, 10, 11) ~ 5,
      TRUE ~ NA_real_
    ),
    SURGIND2 = factor(
      SURGIND2,
      levels = 1:5,
      labels = c(
        "Breakthrough UTI",
        "Symptomatic high-grade VUR",
        "Poor compliance",
        "Ineffective/poor CAP tolerance",
        "Other indications"
      )
    )
  )



# Create a new variable LUTD by combining BLDCOMPRETN, BLDCOMPVOID, BLDCOMPLEAK
Out <- Out %>%
  mutate(
    LUTD = case_when(
      BLDCOMPRETN == 1 |
        BLDCOMPVOID == 1 |
        BLDCOMPLEAK == 1 ~ 1,
      
      BLDCOMPRETN == 0 &
        BLDCOMPVOID == 0 &
        BLDCOMPLEAK == 0 ~ 0,
      
      TRUE ~ NA_real_
    ),
    LUTD = factor(
      LUTD,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# Before matching ----

## Descriptive Statistics ----

# Code for all variables while excluding some
table1 <- Out %>%
  select(-PID, -QCODE) %>%   # exclude both identifiers
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})", #   all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing_text = "Missing"
  ) %>%
  bold_labels()


# Code for selected variables
table1<- Out %>%
  select(AGS, GENDER, BMI, HISTOINJ, SURGIND, PREOPVURGRADE, VURSIDE, UDR7M, 
         VURPHASE, PREOPTYPEU1, PREOPEPISODE1, PREOPRNPLN, PREOPRNSCR,
         SURGTECH2, CENTER
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{median} ({p25}-{p75})", #   all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)" 
    ),
    digits = all_categorical() ~ c(0, 1),  # n = 0 decimals, p = 1 decimal
    missing = "ifany",  # shows missing only when present, and keep clean publication table
    missing_text = "Missing"
  ) %>%
  bold_labels()

table1

# Code for selected variables + stratification

table1<- Out %>%
  select(AGS, GENDER, BMI, HISTOINJ, SURGIND, PREOPVURGRADE, VURSIDE, UDR7M, 
         VURPHASE, PREOPTYPEU1, PREOPEPISODE1, PREOPRNPLN, PREOPRNSCR,
         SURGTECH2, CENTER, TRT2
  ) %>%
  tbl_summary(
    by = TRT2,  # for stratification
    statistic = list(
      all_continuous() ~ "{median} ({p25}-{p75})", #   all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)" 
    ),
    digits = all_categorical() ~ c(0, 1),  # n = 0 decimals, p = 1 decimal
    missing = "ifany",  # shows missing only when present, and keep clean publication table
    missing_text = "Missing"
  ) %>%
  bold_labels()

table1

### Code for selected variables + ALL + stratification

# Baseline characteristics
table1<- Out %>%
  select(AGS, GENDER, BMI, HISTOINJ, SURGIND2, PREOPVURGRADE, VURSIDE, UDR7M, 
         VURPHASE, PREOPTYPEU1, PREOPEPISODE1, PREOPRNPLN, PREOPRNSCR,
         SURGTECH2, CENTER, TRT2
  ) %>%
  tbl_summary(
    by = TRT2,  # for stratification
    statistic = list(
      all_continuous() ~ "{median} ({p25}-{p75})", # all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)" 
    ),
    digits = all_categorical() ~ c(0, 1),  # n = 0 decimals, p = 1 decimal
    missing = "ifany",  # shows missing only when present, and keep clean publication table
    missing_text = "Missing"
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%  #  3 decimals 
  bold_labels()

table1

table1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "output/baseline.docx")
# To export it as csv:
table1_df <- as_tibble(table1)  # A tibble is a “modern, safer, prettier data frame used in tidyverse workflows
# table1 from gtsummary → is NOT a tibble or data.frame
write.csv(table1_df, "output/baseline.csv", row.names = FALSE)

table1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "output/baseline.docx")


# Procedural factors
table2<- Out %>%
  select(TRT2, OPRTIME, STENT, CATH, CATHPERIOD, HOSPTIME) %>%
  tbl_summary(
    by = TRT2,  # for stratification
    statistic = list(
      all_continuous() ~ "{median} ({p25}-{p75})", # or ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)" 
    ),
    digits = all_categorical() ~ c(0, 1),  # n = 0 decimals, p = 1 decimal
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%  #  3 decimals 
  bold_labels()

table2

table2 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "output/Procedural_factors.docx")

# Note for catheterization duration
## Any point beyond 1.5 × IQR from Q1 or Q3 is classified as an outlier
## The distribution of catheterization duration in Cohen group 
## is a highly dispersed / skewed group.
## The main finding is strong distributional difference between treatments.
## This likely reflects true postoperative management differences, not noise.

# Plot catheterization distribution
ggplot(Out, aes(x = TRT2, y = CATHPERIOD)) +
  geom_boxplot()

OR

p <- ggplot(Out, aes(x = factor(TRT2), y = CATHPERIOD)) +
  geom_boxplot() +
  labs(
    x = "Treatment Group",
    y = "Catheterization Duration (days)"
  )

OR


p <- ggplot(
  Out,
  aes(
    x = factor(
      TRT2,
      levels = c(0, 1),
      labels = c("Cohen Group", "Lich-Gregoir Group")
    ),
    y = CATHPERIOD
  )
) +
  geom_boxplot() +
  labs(
    x = "Treatment Group",
    y = "Catheterization Period (days)",
    title = "Distribution of Catheterization Period by Treatment Group"
  ) +
  theme_bw()   #optional

p 

# Smoothed trend (clean publication figure)

Out %>%
  count(YEARS, TRT2) %>%
  ggplot(aes(x = YEARS, y = n, color = factor(TRT2))) +
  geom_smooth(se = FALSE, method = "loess") +
  scale_color_manual(values = c(
    "0" = "#ADD8E6",
    "1" = "#00008B"
  )) +
  labs(x = "Year", y = "Trend of surgeries", color = "Group") +
  theme_bw()


# Line plot of counts over time (trend-focused)

library(dplyr)

Out %>%
  count(YEARS, TRT2) %>%
  ggplot(aes(x = YEARS, y = n, color = factor(TRT2))) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c(
    "0" = "#ADD8E6",
    "1" = "#00008B"
  )) +
  labs(x = "Year", y = "Number of surgeries", color = "Group") +
  theme_bw()


# Side-by-side bar
ggplot(
  Out,
  aes(
    x = factor(YEARS),
    fill = factor(TRT2,
                  levels = c(0,1),
                  labels = c("Cohen Group", "Lich-Gregoir Group"))
  )
) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c(
    "Cohen Group" = "#ADD8E6",
    "Lich-Gregoir Group" = "#00008B"
  )) +
  labs(x = "Year of Surgery", y = "Number of Surgeries", fill = "Group") +
  theme_bw()


# a simple stacked bar plot
p_y <-ggplot(
  Out, 
  aes(x = factor(YEARS), fill = factor(TRT2,
                                       levels = c(0,1),
                                       labels = c("Cohen Group", "Lich-Gregoir Group")))) +
  geom_bar(position = "stack") +
  scale_fill_manual(
    values = c(
      "Cohen Group" = "#ADD8E6",
      "Lich-Gregoir Group" = "#00008B"
    )
  ) +
  labs(x = "Year of Surgery", y = "Number of Surgeries", fill = "Treatment Group") +
  theme_bw() 

p_y

ggsave(filename = "figures/treatment_across_years.png", 
       plot = p_y, 
       width = 8,   # width in inches
       height = 6,  # height in inches
       dpi = 300)   # resolution

# Note: Treatment distribution varied over calendar years, suggesting potential temporal confounding -> years will be included as covariate in PS.


######## Postoperative UTI outcome as time-to-event outcome

# Create labeled TRT2 variable only for survival analysis
Out$Treatment <- factor(
  Out$TRT2,
  levels = c(0,1),
  labels = c("Cohen", "Lich–Gregoir")
)

surv_object <- Surv(time = Out$POSTUTIFUP, event = Out$POSTUTI== 1)
fit <- survfit(surv_object ~ Treatment , data = Out)

p_u <- ggsurvplot(
  fit,
  data = Out,
  fun="event",  # Show cumulative incidence (1 - survival)
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  
  xlim = c(0, 150),
  break.time.by = 12,
  
  xlab = "Follow-up time",
  ylab = "UTI-free survival probability",
  
  risk.table.y.text = FALSE,   # important for clean alignment
  risk.table.height = 0.2,     # optional but recommended
  
  palette = c("#ADD8E6", "#00008B"),
  ylim = c(0, 1),  # Force y-axis from 0 to 1
  risk.table.align = TRUE   # KEY FIX
)


p_u$plot <- p_u$plot + scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 12))
p_u$plot <- p_u$plot +
  scale_y_continuous(
    labels = function(x) paste0(round(x * 100, 1), "%"),
    limits = c(0, 1)
  )

p_u$table <- p_u$table + scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 12))

# To control height of risk table in the saved plot, REMOVE risk.table from ggsurvplot
ggsave(
  filename = "figures/UTI_outcome.png",
  gridExtra::grid.arrange(
    p_u$plot,
    p_u$table,
    heights = c(4, 1.2), # FULL CONTROL HERE (plot height, risk table height)
    ncol = 1
  ),
  width = 10,
  height = 6,
  dpi = 300
)




# To modify the risk table theme
p_u$table <- p_u$table +                 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )



final_plot <- p_u$plot / p_u$table + plot_layout(heights = c(4, 1.2))
ggsave("figures/UTI_outcome.png", final_plot, width = 10, height = 6, dpi = 300)



## Comparative descriptive statistics for the whole cohort (stratified by treatment group)
----------------------------------------------------------------------------------------
#Note:
#For VUR recurrence : REC011
matrix <- matrix(c(13, 91, 11, 158), nrow = 2, byrow = TRUE)
 
# Chi-square test
chisq.test(matrix)
-------------------------------------------------------------------------------------------

## Descriptive statistics for the whole cohort (not stratified by treatment group)

# Load the required package
if (!requireNamespace("tableone", quietly = TRUE)) {
  install.packages("tableone")
}
library(tableone)

# Specify variables
numeric_vars <- c("AGS", "BMI", "BLDLOSS", "CATHPERIOD", "FUPm", 
                  "HOSPTIME", "OPRTIME", "UTIpostopFup", "UTIpostopT")

factor_vars <- c("PREOPVURGRADE", "HISTOINJ", "PREOPRNPLN", "PREOPRNSCR", "VURSIDE", 
                 "VURPHASE", "PREOPEPISODEn", "PREOPTYPEUn", "SURGTECHn2", "TRTn2", 
                 "ANASTOMO", "BLDCOMPLEAK", "BLDCOMPRETN", "BLDCOMPVOID", "BLDSPASM", 
                 "CATH", "Center", "CLAVIENCLASS", "EPIDURAL", "GENDER", "GU30", 
                 "GU90", "GUER30", "GUER90", "HEMAT", "ILEUS", "INCCOMPBLEED", 
                 "INCCOMPINF", "INCCOMPSCAR", "NARC", "NONARC", "PAIN", "PROC365", 
                 "STENT", "UDR7M", "URCOMPCVUR", "URCOMPINJ", "URCOMPOBST", 
                 "URCOMPSTEN", "UTIpostop", "SURGIND", "RECGRADE_MAX", "REC011")

all_vars <- c(numeric_vars, factor_vars)

# Create Table 1 for the whole cohort
table1 <- CreateTableOne(vars = all_vars, data = data, test = FALSE)

# Print Table 1 without p-values (since there are no groups)
print(table1, showAllLevels = TRUE)

# Convert TableOne to a matrix
table1_matrix <- print(table1, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE)

# Save the matrix as a CSV file
write.csv(as.data.frame(table1_matrix), file = "Table1_WholeCohort.csv", row.names = TRUE)



# Calculate median and IQR for numeric variables
library(dplyr)
results_numeric <- lapply(numeric_vars, function(var) {
  data %>%
    summarise(
      Median = median(.data[[var]], na.rm = TRUE),
      Q1 = quantile(.data[[var]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[var]], 0.75, na.rm = TRUE)
    ) %>%
    mutate(Statistic = paste0(Median, " (", Q1, "-", Q3, ")")) %>%
    select(Statistic)
})

# Combine numeric results into a data frame
results_numeric_combined <- do.call(rbind, results_numeric)
rownames(results_numeric_combined) <- numeric_vars

# Save numeric results to CSV
write.csv(results_numeric_combined, "Numeric_Median_IQR_WholeCohort.csv", row.names = TRUE)

---------------------------------------------------------------------------------------------
  
# Do the matching ----


## Variables to use for the matching ----
BMI 
AGS 
HISTOINJ 
PREOPVURGRADE 
VURPHASE 
VURSIDE 
PREOPTYPEUn
PREOPEPISODEn
PREOPRNPLN
PREOPRNSCR 
SURGTECHn2
GENDER
UDR7M
SURGIND
#Missing data: PREOPEPISODEn 12; PREOPRNPLN 85; PREOPTYPEUn 1



# Calculate the propensity score using logistic regression
propensity_model <- glm(TRTn2 ~ PREOPVURGRADE + HISTOINJ + BMI + AGS + PREOPRNPLN + PREOPRNSCR + 
                         VURSIDE + VURPHASE + PREOPEPISODEn + PREOPTYPEUn + SURGTECHn2 + GENDER + UDR7M + SURGIND, 
                         data = data, family = binomial())
# Add the propensity scores to the dataset
 data$PS <- predict(propensity_model, type = "response")
 logit_ps <- log(data$PS / (1 - data$PS))
 sd_logit_ps <- sd(logit_ps)
 caliper_width <- 0.11 * sd_logit_ps  # Slightly smaller than 0.2


install.packages("MatchIt")
library(MatchIt)
set.seed(123) # Fixes the sequence of "random" operations = the same individuals are matched every time

m.out <- matchit(TRTn2 ~ PREOPVURGRADE + HISTOINJ + BMI  + AGS + PREOPRNPLN + PREOPRNSCR + VURSIDE + VURPHASE 
+ PREOPEPISODEn + PREOPTYPEUn + SURGTECHn2 + GENDER + UDR7M + SURGIND, data = data, method = "nearest", caliper = caliper_width)
summary(m.out, standardize=TRUE)

# Extract the matched dataset from the matchit object
mdata <- match.data(m.out)
#Save the matched dataset to reuse
saveRDS(mdata, "matched_sample.rds")
# Load the matched dataset to use it later in any R session
mdata <- readRDS("matched_sample.rds")


install.packages("tableone")
library(tableone)
# Create the table for the matched data with your specified variables
 tableb <- CreateTableOne(
   vars = c('PREOPVURGRADE', 'HISTOINJ', 'BMI', 'AGS', 'PREOPRNPLN', 
            'PREOPRNSCR', 'VURSIDE', 'VURPHASE', 'PREOPEPISODEn', 
            'PREOPTYPEUn', 'SURGTECHn2', 'GENDER', 'UDR7M', 'SURGIND'),
   data = mdata,
   factorVars = c('PREOPVURGRADE', 'HISTOINJ', 'PREOPRNPLN', 'PREOPRNSCR','VURSIDE', 'VURPHASE','SURGTECHn2','PREOPTYPEUn', 'PREOPEPISODEn', 'GENDER', 'UDR7M', 'SURGIND'),  # Categorical variables
   strata = 'TRTn2',  # Adjust if needed based on your dataset
   smd = TRUE          # Include standardized mean differences
 )
# Print the table with SMDs and other options
 tableb <- print(
   tableb,
   smd = TRUE,               # Include standardized mean differences
   showAllLevels = TRUE,     # Show all levels for factor variables
   noSpaces = TRUE,          # Remove spaces in output
   printToggle = FALSE       # Don't print a toggle for summary
 )
 tableb
write.csv(tableb, file = "Tableb_after_matching.csv")
getwd()


# Load necessary libraries
if (!requireNamespace("tableone", quietly = TRUE)) install.packages("tableone")
library(tableone)

# Define variables
vars <- c('AGS','GENDER','BMI','HISTOINJ', 'SURGIND', 'PREOPVURGRADE','UDR7M','VURSIDE', 
           'VURPHASE','PREOPTYPEUn', 'PREOPEPISODEn', 'PREOPRNPLN', 'PREOPRNSCR', 
           'SURGTECHn2' )

factor_vars <- c('GENDER','HISTOINJ', 'SURGIND', 'PREOPVURGRADE','UDR7M','VURSIDE',
  		'VURPHASE','PREOPTYPEUn', 'PREOPEPISODEn', 'PREOPRNPLN', 'PREOPRNSCR', 
           	'SURGTECHn2' )

# Table for unmatched data (before matching)
table_unmatched <- CreateTableOne(
  vars = vars,
  data = data,    # Replace with the name of your unmatched dataset
  factorVars = factor_vars,
  strata = 'TRTn2',        # Stratify by treatment group
  smd = TRUE               # Include standardized mean differences
)

# Table for matched data (after matching)
table_matched <- CreateTableOne(
  vars = vars,
  data = mdata,            # Replace with the name of your matched dataset
  factorVars = factor_vars,
  strata = 'TRTn2',        # Stratify by treatment group
  smd = TRUE               # Include standardized mean differences
)

# Print and save both tables
table_unmatched <- print(
  table_unmatched,
  smd = TRUE, showAllLevels = TRUE, noSpaces = TRUE, printToggle = FALSE
)

table_matched <- print(
  table_matched,
  smd = TRUE, showAllLevels = TRUE, noSpaces = TRUE, printToggle = FALSE
)

# Save as CSV files
write.csv(as.data.frame(table_unmatched), "Tableb_Before_Matching.csv", row.names = TRUE)
write.csv(as.data.frame(table_matched), "Tableb_After_Matching.csv", row.names = TRUE)

# View the tables
table_unmatched
table_matched



## Mean SMDs ----

# To calculate the standardized mean differences of the whole model before and after propensity score
# matching, we can calculate the average of the individual SMD values across all covariates,as follows:

# Mean SMD for the whole unmatched cohort:
# ............................................

# Load the required library for reading CSV files
library(readr)

# Read the CSV files
table_unmatched <- read.csv("Tableb_Before_Matching.csv", row.names = 1)

# View the structure of the table to locate SMD columns
str(table_unmatched)

# Check the first few rows of the data to identify the relevant columns for SMDs
head(table_unmatched)

# If the SMD column has a specific name, such as "SMD" or contains it as part of the column name, you can extract it as follows
# Example: extracting SMD for all variables
smd_values <- table_unmatched$SMD  # Replace 'SMD' with the correct column name for SMDs

# If the SMDs are spread across multiple columns, you can extract them like this:
smd_values <- table_unmatched[, grep("SMD", colnames(table_unmatched))]  # Adjust based on actual column names

# Calculate the mean SMD, or analyze it as needed
mean_smd <- mean(as.numeric(smd_values), na.rm = TRUE)

# Print the result
print(mean_smd)

# Mean SMD for the whole matched cohort:
# ........................................

# Load the required library for reading CSV files
library(readr)

# Read the CSV files
table_matched <- read.csv("Tableb_After_Matching.csv", row.names = 1)

# View the structure of the table to locate SMD columns
str(table_matched)

# Check the first few rows of the data to identify the relevant columns for SMDs
head(table_matched)

# If the SMD column has a specific name, such as "SMD" or contains it as part of the column name, you can extract it as follows
# Example: extracting SMD for all variables
smd_values <- table_matched$SMD  # Replace 'SMD' with the correct column name for SMDs

# If the SMDs are spread across multiple columns, you can extract them like this:
smd_values <- table_matched[, grep("SMD", colnames(table_matched))]  # Adjust based on actual column names

# Calculate the mean SMD, or analyze it as needed
mean_smd <- mean(as.numeric(smd_values), na.rm = TRUE)

# Print the result
print(mean_smd)


----------
# Load necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

library(tidyr)

# Define numeric variables
numeric_vars <- c('AGS', 'BMI', 'BLDLOSS', 'FUPm', 'CATHPERIOD', 'OPRTIME', 'HOSPTIME')  # Replace with the numeric variables in your dataset

# Calculate median and IQR for numeric variables stratified by treatment group
numeric_summary_stratified <- mdata %>%
  select(all_of(numeric_vars), TRTn2) %>%  # Include the treatment group variable
  group_by(TRTn2) %>%  # Stratify by treatment group
  summarise(across(everything(), list(
    Median = ~median(.x, na.rm = TRUE),
    Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
    Q3 = ~quantile(.x, 0.75, na.rm = TRUE)
  ), .names = "{col}_{fn}"))  # Naming convention for the summary columns

# Reshape the data into a readable format
numeric_summary_stratified <- numeric_summary_stratified %>%
  pivot_longer(cols = -TRTn2,
               names_to = c("Variable", ".value"),
               names_sep = "_") %>%
  mutate(IQR = paste0(Q1, " - ", Q3))  # Combine Q1 and Q3 into IQR column

# Save the stratified summary as CSV
write.csv(numeric_summary_stratified, "Numeric_Median_IQR_Matched_Stratifed_by_Treatment.csv", row.names = FALSE)

# View the summary
numeric_summary_stratified

-----------------------------------------------------------------------------------------
# After matching ----

## Descriptive analysis ----

# Comparative descriptive statistics for the matched cohort (stratified by treatment group):

# Descriptive analysis generating mean (SD) for numerical variables

# Load the required package
if (!requireNamespace("tableone", quietly = TRUE)) {
  install.packages("tableone")
}
library(tableone)

# Specify variables
numeric_vars <- c("AGS", "BMI", "BLDLOSS", "CATHPERIOD", "FUPm", 
                  "HOSPTIME", "OPRTIME", "UTIpostopFup", "UTIpostopT")

factor_vars <- c("PREOPVURGRADE", "HISTOINJ", "PREOPRNPLN", "PREOPRNSCR", "VURSIDE", 
                 "VURPHASE", "PREOPEPISODEn", "PREOPTYPEUn", "SURGTECHn2", "TRTn2", 
                 "ANASTOMO", "BLDCOMPLEAK", "BLDCOMPRETN", "BLDCOMPVOID", "BLDSPASM", 
                 "CATH", "Center", "CLAVIENCLASS", "EPIDURAL", "GENDER", "GU30", 
                 "GU90", "GUER30","GUER90", "HEMAT", "ILEUS", "INCCOMPBLEED", "INCCOMPINF", 
                 "INCCOMPSCAR", "NARC", "NONARC", "PAIN", "PROC365", "STENT", 
                 "UDR7M", "URCOMPCVUR", "URCOMPINJ", "URCOMPOBST", "URCOMPSTEN", 
                 "UTIpostop", "SURGIND", "VURRECG01" , "REC011")

all_vars <- c(numeric_vars, factor_vars)

# Create Table m
tablem <- CreateTableOne(vars = all_vars, strata = "TRTn2", data = mdata, test = TRUE)

# Print Table 1 with p-values
print(tablem, showAllLevels = TRUE, pDigits = 3)

# Convert TableOne to a matrix
tablem_matrix <- print(tablem, showAllLevels = TRUE, pDigits = 3, quote = FALSE, noSpaces = TRUE)

# Save the matrix as a CSV file
write.csv(as.data.frame(tablem_matrix), file = "Tablem_descriptive.csv", row.names = TRUE)


# Calculate median and IQR manually for numeric variables
calc_median_iqr <- function(var, group) {
  mdata %>%
    group_by(.data[[group]]) %>%
    summarise(
      Median = median(.data[[var]], na.rm = TRUE),
      Q1 = quantile(.data[[var]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[var]], 0.75, na.rm = TRUE)
    )
}

# Loop through numeric variables and calculate median/IQR for treatment groups
library(dplyr)
results_numeric <- lapply(numeric_vars, calc_median_iqr, group = "TRTn2")
names(results_numeric) <- numeric_vars

# Combine numeric and categorical results
results_numeric_combined <- do.call(rbind, lapply(results_numeric, function(x) {
  x %>% mutate(Statistic = paste0(Median, " (", Q1, "-", Q3, ")")) %>%
    select(Statistic)
}))

# Export the results
write.csv(results_numeric_combined, "Numeric_mMedian_IQR.csv")

----------------------------------------------------------------------------------------
#Note:
#For VUR recurrence : REC011
matrix <- matrix(c(10, 72, 9, 80), nrow = 2, byrow = TRUE)
 
# Chi-square test
chisq.test(matrix)
-------------------------------------------------------------------------------------------

# Descriptive statistics for the matched cohort (not stratified by treatment group):

# Load the required package
if (!requireNamespace("tableone", quietly = TRUE)) {
  install.packages("tableone")
}
library(tableone)

# Specify variables
numeric_vars <- c("AGS", "BMI", "BLDLOSS", "CATHPERIOD", "FUPm", 
                  "HOSPTIME", "OPRTIME", "UTIpostopFup", "UTIpostopT")

factor_vars <- c("PREOPVURGRADE", "HISTOINJ", "PREOPRNPLN", "PREOPRNSCR", "VURSIDE", 
                 "VURPHASE", "PREOPEPISODEn", "PREOPTYPEUn", "SURGTECHn2", "TRTn2", 
                 "ANASTOMO", "BLDCOMPLEAK", "BLDCOMPRETN", "BLDCOMPVOID", "BLDSPASM", 
                 "CATH", "Center", "CLAVIENCLASS", "EPIDURAL", "GENDER", "GU30", 
                 "GU90", "GUER30", "GUER90", "HEMAT", "ILEUS", "INCCOMPBLEED", 
                 "INCCOMPINF", "INCCOMPSCAR", "NARC", "NONARC", "PAIN", "PROC365", 
                 "STENT", "UDR7M", "URCOMPCVUR", "URCOMPINJ", "URCOMPOBST", 
                 "URCOMPSTEN", "UTIpostop", "SURGIND", "VURRECG01", "REC011")

all_vars <- c(numeric_vars, factor_vars)

# Create Table mn for the whole cohort
tablemn <- CreateTableOne(vars = all_vars, data = mdata, test = FALSE)

# Print Table mn without p-values (since there are no groups)
print(tablemn, showAllLevels = TRUE)

# Convert TableOne to a matrix
tablemn_matrix <- print(tablemn, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE)

# Save the matrix as a CSV file
write.csv(as.data.frame(tablemn_matrix), file = "Tablemn_MatchedCohort.csv", row.names = TRUE)



# Calculate median and IQR for numeric variables
library(dplyr)
results_numeric <- lapply(numeric_vars, function(var) {
  mdata %>%
    summarise(
      Median = median(.data[[var]], na.rm = TRUE),
      Q1 = quantile(.data[[var]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[var]], 0.75, na.rm = TRUE)
    ) %>%
    mutate(Statistic = paste0(Median, " (", Q1, "-", Q3, ")")) %>%
    select(Statistic)
})

# Combine numeric results into a data frame
results_numeric_combined <- do.call(rbind, results_numeric)
rownames(results_numeric_combined) <- numeric_vars

# Save numeric results to CSV
write.csv(results_numeric_combined, "Numeric_mnMedian_IQR_MatchedCohort.csv", row.names = TRUE)


## Love plot - Study the matching balance ----

install.packages("cobalt")
library(cobalt)

# Generate the balance table with unadjusted values
bal <- bal.tab(m.out, m.threshold = 0.25, un = TRUE)

# Exclude the "distance" covariate from the balance table
bal$Balance <- bal$Balance[!rownames(bal$Balance) %in% "distance", ]

# Create the Love plot without the "distance" covariate
love_plot <- love.plot(bal, stat = "mean.diffs", grid = TRUE, stars = "raw", 
          abs = FALSE, sample.names = c("Before Matching", "After Matching"))

## NOTE: I changed the name of the variables in the dataset "data" to have them in the plot correctly, 
# and then I run matchIt again

# Load necessary library
library(labelled)
 
# Create a function to rename variables based on their labels
rename_with_labels <- function(data) {
# Extract the labels for all variables in the dataset
   labels <- sapply(data, function(x) var_label(x))  # Extract labels for each variable
   
# Remove any variables with no label
labels[is.na(labels)] <- names(labels)[is.na(labels)]  # Retain the original name if no label is present
   
# Rename variables in the dataset to match their labels
   names(data) <- labels
   
   return(data)
 }
 
# Apply the function to rename variables in your dataset
data <- rename_with_labels(data)
 
# View the dataset with updated variable names
head(data)

library(MatchIt)

m.out <- matchit(`Treatment group` ~ `VUR Grade` + `Prior injection` + `BMI, kg/m2` + 
                  `Age at surgery, years` + `Preoperative APN` + `Preoperative renal scarring` + 
                  `VUR side` + `VUR timing` + `Nb of preoperative UTIs` + 
                  `Type of preoperative UTIs` + `Surgeon experience, nb of prior similar operations` + 
                  `Gender` + `Ureteral dilation` + `Indication for VUR Surgical Management`, 
                  data = data, 
                  method = "nearest", caliper = caliper_width)


install.packages("MatchIt")
library(MatchIt)

m.out <- matchit(TRTn2 ~ PREOPVURGRADE + HISTOINJ + BMI  + AGS + PREOPRNPLN + PREOPRNSCR + VURSIDE + VURPHASE 
+ PREOPEPISODEn + PREOPTYPEUn + SURGTECHn2 + GENDER + UDR7M + SURGIND, data = data, method = "nearest", caliper = caliper_width)
summary(m.out, standardize=TRUE)

# Extract the matched dataset from the matchit object
mdata <- match.data(m.out)



install.packages("cobalt")
library(cobalt)

# Generate the balance table with unadjusted values
bal <- bal.tab(m.out, m.threshold = 0.25, un = TRUE)

# Exclude the "distance" covariate from the balance table
bal$Balance <- bal$Balance[!rownames(bal$Balance) %in% "distance", ]

# Create the Love plot without the "distance" covariate
love_plot <- love.plot(bal, stat = "mean.diffs", grid = TRUE, stars = "raw", 
          abs = FALSE, sample.names = c("Before Matching", "After Matching"))


## Customized plot:(It works properly) ----

#Use adjusted SMD to order the covariates in the Love plot to visualize how well the matching process has improved the balance between the groups.

#In a Love plot, the most informative approach is to order the covariates based on adjusted SMD rather than unadjusted SMD. Here's why:
# - Adjusted SMD reflects the balance after matching, which is the main focus of propensity score matching or other balancing techniques. It shows the improvement in balance achieved by the matching process, indicating whether the covariates are now more similar across the treatment and control groups after adjustment. This is the key information for assessing the success of the matching process.
# - Unadjusted SMD shows the imbalance before matching and doesn't provide insight into the effect of the matching procedure. Ordering by unadjusted SMD would highlight covariates that were highly imbalanced before matching, but it wouldn't help in showing how well the matching method has addressed these imbalances.

# Assign the row names (covariates) to the smd_values
smd_values <- bal$Balance$Diff.Adj  # Use "Diff.Adj" for adjusted differences

# Assign the names of the covariates (row names of the balance table)
names(smd_values) <- rownames(bal$Balance)

# Now, order the covariates by the SMD in ascending order
ordered_covariates <- names(sort(smd_values, decreasing = TRUE))  # Sort in ascending order

# Check the ordered covariates
ordered_covariates

# Reorder the Balance table based on ordered covariates
bal$Balance <- bal$Balance[ordered_covariates, , drop = FALSE]

# Now generate the Love plot with the ordered covariates
love_plot <- love.plot(bal, 
                       stat = "mean.diffs", 
                       grid = TRUE, 
                       stars = "raw", 
                       abs = FALSE, 
                       sample.names = c("Before Matching", "After Matching"))

# Load the required package
library(ggplot2)


##Option 1 to draw and save love plot with the customization: ----

# Generate the Love plot with all the customizations
ggsave("love_plot.png", plot = love_plot +  
     theme( 

     # Minimize plot width and reduce plot margin
     plot.margin = margin(10, 10, 10, 10),  # Adjust margins to reduce width
     # Customize all text to the same size
     axis.text = element_text(size = 12),  # Set size for axis labels
     axis.title = element_text(size = 12),  # Set size for axis titles
     plot.title = element_text(size = 12),  # Set size for plot title without bold
     legend.text = element_text(size = 12),  # Set size for legend text
     legend.title = element_text(size = 12),  # Set size for legend title
     # Customize axis ticks
     axis.ticks = element_line(size = 0.5, color = "black"),  # Make ticks smaller and black
     axis.line = element_line(size = 0.5, color = "black"),  # Add lines to axis
     # Set plot background to white
     panel.background = element_blank(),
     # Remove gridlines by setting major and minor gridlines to blank
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     # Remove the top and right borders (frame)
     panel.border = element_blank(),
     axis.ticks.length = unit(0.3, "cm"),  # Adjust tick length
     # Increase the space between covariates (adjusting panel spacing)
     panel.spacing = unit(2, "lines")  # Increase space between rows 
     ), width = 12, height = 10, dpi = 300)  # Adjust width, height, and resolution for larger size


##Option 2 to draw and save love plot with the customization: ----

love_plot_customized <- love.plot(bal, 
                                   stat = "mean.diffs", 
                                   grid = TRUE, 
                                   stars = "raw", 
                                   abs = FALSE, 
                                   sample.names = c("Before Matching", "After Matching")) +  
   theme(
     # Minimize plot width and reduce plot margin
     plot.margin = margin(10, 10, 10, 10),  # Adjust margins to reduce width
     # Customize all text to the same size
     axis.text = element_text(size = 12),  # Set size for axis labels
     axis.title = element_text(size = 12),  # Set size for axis titles
     plot.title = element_text(size = 12),  # Set size for plot title without bold
     legend.text = element_text(size = 12),  # Set size for legend text
     legend.title = element_text(size = 12),  # Set size for legend title
     # Customize axis ticks
     axis.ticks = element_line(size = 0.5, color = "black"),  # Make ticks smaller and black
     axis.line = element_line(size = 0.5, color = "black"),  # Add lines to axis
     # Set plot background to white
     panel.background = element_blank(),
     # Remove gridlines by setting major and minor gridlines to blank
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     # Remove the top and right borders (frame)
     panel.border = element_blank(),
     axis.ticks.length = unit(0.3, "cm"),  # Adjust tick length
     # Increase the space between covariates (adjusting panel spacing)
     panel.spacing = unit(2, "lines")  # Increase space between rows
   )
 
# Step 2: Save the customized plot
ggsave("love_plot_customized.png", plot = love_plot_customized, width = 12, height = 10, dpi = 300)

-----------------------------------------------------------------------------------------------------
# Propensity score distribution histogram (before vs after matching), Used to assess overlap/positivity assumption, not covariate balance
  
p <- bal.plot(
  m.out,
  var.name = "distance",
  which = "both",
  type = "histogram",
  mirror = TRUE,
  sample.names = c("Before Matching", "After Matching")
)

# Update treatment labels in the legend
p + scale_fill_discrete(labels = c("Cohen group", "Lich-Gregoir group")) +
  scale_color_discrete(labels = c("Cohen group", "Lich-Gregoir group"))
-----------------------------------------------------------------------------------------------------
  
  # BEFORE MATCHING
  tab_before <- Out %>%
  select(
    AGS, GENDER, BMI, HISTOINJ, SURGIND2, PREOPVURGRADE,
    VURSIDE, UDR7M, VURPHASE, PREOPTYPEU1,
    PREOPEPISODE1, PREOPRNPLN, PREOPRNSCR,
    SURGTECH2, CENTER, TRT2
  ) %>%
  tbl_summary(
    by = TRT2,
    statistic = list(
      all_continuous() ~ "{median} ({p25}-{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "ifany"
  ) %>%
  add_p()

# AFTER MATCHING
tab_after <- mdata %>%
  select(
    AGS, GENDER, BMI, HISTOINJ, SURGIND2, PREOPVURGRADE,
    VURSIDE, UDR7M, VURPHASE, PREOPTYPEU1,
    PREOPEPISODE1, PREOPRNPLN, PREOPRNSCR,
    SURGTECH2, CENTER, TRT2
  ) %>%
  tbl_summary(
    by = TRT2,
    statistic = list(
      all_continuous() ~ "{median} ({p25}-{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "ifany"
  ) %>%
  add_p()

# MERGE TABLES
table_combined <- tbl_merge(
  tbls = list(tab_before, tab_after),
  tab_spanner = c("**Before Matching**", "**After Matching**")
)

table_combined

table_combined %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "output/baseline_before_after.docx")

-----------------------------------------------------------------------------------------------
  
# Univariable and multivariable analysis for the outcomes  ----
# using the multivariable GEE model with a logit link function and an exchangeable correlation


# %BLDCOMPRETN

# Convert BLDCOMPRETN to numeric binary (0/1) in the matched dataset
mdata$BLDCOMPRETN <- ifelse(mdata$BLDCOMPRETN == "Yes", 1, 0)
table(mdata$BLDCOMPRETN)  # Check distribution of "Yes" and "No"
# Check structure of BLDCOMPRETN
str(mdata$BLDCOMPRETN)
# Check frequency of values
table(mdata$BLDCOMPRETN)  # Should show counts for 0 and 1

#Ensure that all the other variable are in the correct format
mdata$TRTn2 <-as.factor (mdata$TRTn2)
mdata$CATHPERIOD <-as.numeric (mdata$CATHPERIOD)
mdata$OPRTIME <-as.numeric (mdata$OPRTIME)
mdata$HOSPTIME <-as.numeric (mdata$HOSPTIME)
mdata$STENT <-as.factor (mdata$STENT)
mdata$Center <-as.factor (mdata$Center)
str (mdata)

> install.packages("geepack")
# Load the geepack package
library(geepack)


#Fit the GEE model for univariate analysis - TRTn2
gee_model <- geeglm(
BLDCOMPRETN  ~ TRTn2 , 
  data = mdata, 
  id = Center, 
  family = binomial(link = "logit"), 
  corstr = "exchangeable"
)
# Display the summary of the model
summary(gee_model)

gee_model <- geeglm(
BLDCOMPRETN  ~ STENT , 
  data = mdata, 
  id = Center, 
  family = binomial(link = "logit"), 
  corstr = "exchangeable"
)
# Display the summary of the model
summary(gee_model)

gee_model <- geeglm(
BLDCOMPRETN  ~ CATHPERIOD , 
  data = mdata, 
  id = Center, 
  family = binomial(link = "logit"), 
  corstr = "exchangeable"
)
# Display the summary of the model
summary(gee_model)


gee_model <- geeglm(
BLDCOMPRETN  ~ OPRTIME , 
  data = mdata, 
  id = Center, 
  family = binomial(link = "logit"), 
  corstr = "exchangeable"
)
# Display the summary of the model
summary(gee_model)

gee_model <- geeglm(
BLDCOMPRETN  ~ HOSPTIME , 
  data = mdata, 
  id = Center, 
  family = binomial(link = "logit"), 
  corstr = "exchangeable"
)
# Display the summary of the model
summary(gee_model)

# Fit the GEE model for multivariable analysis
gee_model <- geeglm(
  BLDCOMPRETN ~ TRTn2 + PS + STENT + CATHPERIOD + OPRTIME + HOSPTIME, 
  data = mdata, 
  id = Center, 
  family = binomial(link = "logit"), 
  corstr = "exchangeable"
)
# Display the summary of the model
summary(gee_model)

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#Additional commands to add in order to generate directly OR and its confidence interval - Applicable for univariate and multivariate analysis

# Fit the GEE model for multivariate analysis
gee_model <- geeglm(
BLDCOMPRETN ~ TRTn2 + PS + STENT + CATHPERIOD + OPRTIME + HOSPTIME,  
  data = mdata, 
  id = Center, 
  family = binomial(link = "logit"), 
  corstr = "exchangeable"
)
# Display the summary of the model
summary(gee_model)


# Extract the coefficients, standard errors, and p-values
summary_output <- summary(gee_model)$coefficients
coefficients <- summary_output[, "Estimate"]
std_errors <- summary_output[, "Std.err"]
p_values <- summary_output[, "Pr(>|W|)"]

# Extract the names of the covariates
covariate_names <- rownames(summary_output)[-1]  # Removing the intercept

# Check the names of the coefficients
print(covariate_names)

# Remove intercept if it is included
coefficients <- coefficients[-1]
std_errors <- std_errors[-1]
p_values <- p_values[-1]

# Calculate the odds ratios (OR) and 95% confidence intervals (CI)
OR <- exp(coefficients)
CI_lower <- exp(coefficients - 1.96 * std_errors)
CI_upper <- exp(coefficients + 1.96 * std_errors)

# Check if lengths match
print(length(coefficients))
print(length(std_errors))
print(length(p_values))

# Create a data frame to display the results
results <- data.frame(
  Covariate = covariate_names,
  OR = round(OR, 2),
  CI = paste0("(", round(CI_lower, 2), " - ", round(CI_upper, 2), ")"),
  P_value = round(p_values, 3)
)

# View the results
print(results)

#Explore data if issue encountered in analysis
> table(mdata$STENT, mdata$INCCOMPSCAR)
> table(mdata$STENT, mdata$ANASTOMO)


^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# Repeat all the above univariable and multivariable analyses for all outcomes

# Convert covariates to numeric binary (0/1) in the matched dataset
mdata$BLDCOMPRETN <- ifelse(mdata$BLDCOMPRETN == "Yes", 1, 0)
mdata$BLDSPASM <- ifelse(mdata$BLDSPASM == "Yes", 1, 0)
mdata$HEMAT <- ifelse(mdata$HEMAT == "Yes", 1, 0)
mdata$UTIpostop <- ifelse(mdata$UTIpostop == "UTI", 1, 0)
mdata$BLDCOMPVOID <- ifelse(mdata$BLDCOMPVOID == "Yes", 1, 0)
mdata$BLDCOMPLEAK <- ifelse(mdata$BLDCOMPLEAK == "Yes", 1, 0)
mdata$URCOMPOBST <- ifelse(mdata$URCOMPOBST == "Yes", 1, 0)
mdata$URCOMPINJ <- ifelse(mdata$URCOMPINJ == "Yes", 1, 0)
mdata$URCOMPSTEN <- ifelse(mdata$URCOMPSTEN == "Yes", 1, 0)
mdata$INCCOMPBLEED <- ifelse(mdata$INCCOMPBLEED == "Yes", 1, 0)
mdata$INCCOMPINF <- ifelse(mdata$INCCOMPINF == "Yes", 1, 0)
mdata$INCCOMPSCAR <- ifelse(mdata$INCCOMPSCAR == "Yes", 1, 0)
mdata$ILEUS <- ifelse(mdata$ILEUS == "Yes", 1, 0)
mdata$ANASTOMO <- ifelse(mdata$ANASTOMO == "Yes", 1, 0)
mdata$URCOMPCVUR <- ifelse(mdata$URCOMPCVUR == "Yes", 1, 0)
mdata$GU30 <- ifelse(mdata$GU30 == "Yes", 1, 0)
mdata$GU90 <- ifelse(mdata$GU90 == "Yes", 1, 0)
mdata$GUER30 <- ifelse(mdata$GUER30 == "Yes", 1, 0)
mdata$GUER90 <- ifelse(mdata$GUER90 == "Yes", 1, 0)
mdata$PROC365 <- ifelse(mdata$PROC365 == "Yes", 1, 0)

# Check structure of each covariate
str(mdata$BLDCOMPRETN)
str(mdata$BLDSPASM)
str(mdata$HEMAT)
str(mdata$UTIpostop)
str(mdata$BLDCOMPVOID)
str(mdata$BLDCOMPLEAK)
str(mdata$URCOMPOBST)
str(mdata$URCOMPINJ)
str(mdata$URCOMPSTEN)
str(mdata$INCCOMPBLEED)
str(mdata$INCCOMPINF)
str(mdata$INCCOMPSCAR)
str(mdata$ILEUS)
str(mdata$ANASTOMO)
str(mdata$URCOMPCVUR)
str(mdata$GU30)
str(mdata$GU90)
str(mdata$GUER30)
str(mdata$GUER90)
str(mdata$PROC365)

# Check frequency of values for each covariate
table(mdata$BLDCOMPRETN)
table(mdata$BLDSPASM)
table(mdata$HEMAT)
table(mdata$UTIpostop)
table(mdata$BLDCOMPVOID)
table(mdata$BLDCOMPLEAK)
table(mdata$URCOMPOBST)
table(mdata$URCOMPINJ)
table(mdata$URCOMPSTEN)
table(mdata$INCCOMPBLEED)
table(mdata$INCCOMPINF)
table(mdata$INCCOMPSCAR)
table(mdata$ILEUS)
table(mdata$ANASTOMO)
table(mdata$URCOMPCVUR)
table(mdata$GU30)
table(mdata$GU90)
table(mdata$GUER30)
table(mdata$GUER90)
table(mdata$PROC365)

--------------------------------------------------------------------------------------
# Check the performance of the multivariate model using AUC, 95% CI of AUC and p-value

install.packages("pROC")
library(pROC)

# Ensure predicted probabilities are numeric
predicted_probs <- as.numeric(predict(gee_model, type = "response"))

# Extract observed outcomes
observed_outcomes <- mdata$BLDCOMPRETN

# Calculate ROC and AUC
roc_curve <- roc(observed_outcomes, predicted_probs)

# Extract AUC
auc_value <- auc(roc_curve)

# Calculate 95% CI for AUC using bootstrapping
ci_auc <- ci.auc(roc_curve, method = "bootstrap", boot.n = 2000)

# Perform p-value test manually (AUC = 0.5 as the null hypothesis)
boot_results <- replicate(2000, {
  shuffled_outcomes <- sample(observed_outcomes)  # Shuffle outcomes
  roc_shuffled <- roc(shuffled_outcomes, predicted_probs)
  auc(roc_shuffled)
})

# Calculate p-value as the proportion of bootstrap AUCs >= observed AUC
p_value <- mean(boot_results >= auc_value)

# Print the results
print(paste("AUC:", round(auc_value, 3)))
print(paste("95% CI for AUC:", paste(round(ci_auc[1:3], 3), collapse = " - ")))
print(paste("P-value:", round(p_value, 3)))

---------------------------------------------------------------------------------------
# save workspace

save.image("OUT.RData")
load("OUT.RData")
---------------------------------------------------------------------------------------
####################################################################################################
####################################################################################################

## Repetitive Script for each outcome uivariable and multivariable analyses - To change it for each outcome
# Note 1: Stenting was excluded from the multivariable analysis for incisional hernia due to zero occurrence of the variable among cases.
# Note 2: Stenting was excluded from the multivariable analysis for anastomosis due to zero occurrence of the variable among cases.
# Note 3: For UTIpostop outcome, ensure changing code 1 for UTI and 0 for No UTI


# %BLDCOMPRETN
# =================

# Univariable and multivariable analysis for the outcomes using the multivariable GEE model with a logit link function and an exchangeable correlation


# Convert BLDCOMPRETN to numeric binary (0/1) in the matched dataset
mdata$BLDCOMPRETN <- ifelse(mdata$BLDCOMPRETN == "Yes", 1, 0)
table(mdata$BLDCOMPRETN)  # Check distribution of "Yes" and "No"
# Check structure of BLDCOMPRETN
str(mdata$BLDCOMPRETN)
# Check frequency of values
table(mdata$BLDCOMPRETN)  # Should show counts for 0 and 1

#Ensure that all the other variable are in the correct format
mdata$TRTn2 <-as.factor (mdata$TRTn2)
mdata$CATHPERIOD <-as.numeric (mdata$CATHPERIOD)
mdata$OPRTIME <-as.numeric (mdata$OPRTIME)
mdata$HOSPTIME <-as.numeric (mdata$HOSPTIME)
mdata$STENT <-as.factor (mdata$STENT)
mdata$Center <-as.factor (mdata$Center)
str (mdata)

#---------

#Fit the GEE model for univariate analysis - TRTn2
gee_model <- geeglm(
BLDCOMPRETN  ~ TRTn2 , 
  data = mdata, 
  id = Center, 
  family = binomial(link = "logit"), 
  corstr = "exchangeable"
)
# Display the summary of the model
summary(gee_model)

# Extract the coefficients, standard errors, and p-values
summary_output <- summary(gee_model)$coefficients
coefficients <- summary_output[, "Estimate"]
std_errors <- summary_output[, "Std.err"]
p_values <- summary_output[, "Pr(>|W|)"]

# Extract the names of the covariates
covariate_names <- rownames(summary_output)[-1]  # Removing the intercept

# Check the names of the coefficients
print(covariate_names)

# Remove intercept if it is included
coefficients <- coefficients[-1]
std_errors <- std_errors[-1]
p_values <- p_values[-1]

# Calculate the odds ratios (OR) and 95% confidence intervals (CI)
OR <- exp(coefficients)
CI_lower <- exp(coefficients - 1.96 * std_errors)
CI_upper <- exp(coefficients + 1.96 * std_errors)

# Check if lengths match
print(length(coefficients))
print(length(std_errors))
print(length(p_values))

# Create a data frame to display the results
results <- data.frame(
  Covariate = covariate_names,
  OR = round(OR, 2),
  CI = paste0("(", round(CI_lower, 2), " - ", round(CI_upper, 2), ")"),
  P_value = round(p_values, 3)
)

# View the results
print(results)

#---------

gee_model <- geeglm(
BLDCOMPRETN  ~ STENT , 
  data = mdata, 
  id = Center, 
  family = binomial(link = "logit"), 
  corstr = "exchangeable"
)
# Display the summary of the model
summary(gee_model)


# Extract the coefficients, standard errors, and p-values
summary_output <- summary(gee_model)$coefficients
coefficients <- summary_output[, "Estimate"]
std_errors <- summary_output[, "Std.err"]
p_values <- summary_output[, "Pr(>|W|)"]

# Extract the names of the covariates
covariate_names <- rownames(summary_output)[-1]  # Removing the intercept

# Check the names of the coefficients
print(covariate_names)

# Remove intercept if it is included
coefficients <- coefficients[-1]
std_errors <- std_errors[-1]
p_values <- p_values[-1]

# Calculate the odds ratios (OR) and 95% confidence intervals (CI)
OR <- exp(coefficients)
CI_lower <- exp(coefficients - 1.96 * std_errors)
CI_upper <- exp(coefficients + 1.96 * std_errors)

# Check if lengths match
print(length(coefficients))
print(length(std_errors))
print(length(p_values))

# Create a data frame to display the results
results <- data.frame(
  Covariate = covariate_names,
  OR = round(OR, 2),
  CI = paste0("(", round(CI_lower, 2), " - ", round(CI_upper, 2), ")"),
  P_value = round(p_values, 3)
)

# View the results
print(results)

#---------

gee_model <- geeglm(
BLDCOMPRETN  ~ CATHPERIOD , 
  data = mdata, 
  id = Center, 
  family = binomial(link = "logit"), 
  corstr = "exchangeable"
)
# Display the summary of the model
summary(gee_model)


# Extract the coefficients, standard errors, and p-values
summary_output <- summary(gee_model)$coefficients
coefficients <- summary_output[, "Estimate"]
std_errors <- summary_output[, "Std.err"]
p_values <- summary_output[, "Pr(>|W|)"]

# Extract the names of the covariates
covariate_names <- rownames(summary_output)[-1]  # Removing the intercept

# Check the names of the coefficients
print(covariate_names)

# Remove intercept if it is included
coefficients <- coefficients[-1]
std_errors <- std_errors[-1]
p_values <- p_values[-1]

# Calculate the odds ratios (OR) and 95% confidence intervals (CI)
OR <- exp(coefficients)
CI_lower <- exp(coefficients - 1.96 * std_errors)
CI_upper <- exp(coefficients + 1.96 * std_errors)

# Check if lengths match
print(length(coefficients))
print(length(std_errors))
print(length(p_values))

# Create a data frame to display the results
results <- data.frame(
  Covariate = covariate_names,
  OR = round(OR, 2),
  CI = paste0("(", round(CI_lower, 2), " - ", round(CI_upper, 2), ")"),
  P_value = round(p_values, 3)
)

# View the results
print(results)

#---------


gee_model <- geeglm(
BLDCOMPRETN  ~ OPRTIME , 
  data = mdata, 
  id = Center, 
  family = binomial(link = "logit"), 
  corstr = "exchangeable"
)
# Display the summary of the model
summary(gee_model)


# Extract the coefficients, standard errors, and p-values
summary_output <- summary(gee_model)$coefficients
coefficients <- summary_output[, "Estimate"]
std_errors <- summary_output[, "Std.err"]
p_values <- summary_output[, "Pr(>|W|)"]

# Extract the names of the covariates
covariate_names <- rownames(summary_output)[-1]  # Removing the intercept

# Check the names of the coefficients
print(covariate_names)

# Remove intercept if it is included
coefficients <- coefficients[-1]
std_errors <- std_errors[-1]
p_values <- p_values[-1]

# Calculate the odds ratios (OR) and 95% confidence intervals (CI)
OR <- exp(coefficients)
CI_lower <- exp(coefficients - 1.96 * std_errors)
CI_upper <- exp(coefficients + 1.96 * std_errors)

# Check if lengths match
print(length(coefficients))
print(length(std_errors))
print(length(p_values))

# Create a data frame to display the results
results <- data.frame(
  Covariate = covariate_names,
  OR = round(OR, 2),
  CI = paste0("(", round(CI_lower, 2), " - ", round(CI_upper, 2), ")"),
  P_value = round(p_values, 3)
)

# View the results
print(results)

#---------

gee_model <- geeglm(
BLDCOMPRETN  ~ HOSPTIME , 
  data = mdata, 
  id = Center, 
  family = binomial(link = "logit"), 
  corstr = "exchangeable"
)
# Display the summary of the model
summary(gee_model)


# Extract the coefficients, standard errors, and p-values
summary_output <- summary(gee_model)$coefficients
coefficients <- summary_output[, "Estimate"]
std_errors <- summary_output[, "Std.err"]
p_values <- summary_output[, "Pr(>|W|)"]

# Extract the names of the covariates
covariate_names <- rownames(summary_output)[-1]  # Removing the intercept

# Check the names of the coefficients
print(covariate_names)

# Remove intercept if it is included
coefficients <- coefficients[-1]
std_errors <- std_errors[-1]
p_values <- p_values[-1]

# Calculate the odds ratios (OR) and 95% confidence intervals (CI)
OR <- exp(coefficients)
CI_lower <- exp(coefficients - 1.96 * std_errors)
CI_upper <- exp(coefficients + 1.96 * std_errors)

# Check if lengths match
print(length(coefficients))
print(length(std_errors))
print(length(p_values))

# Create a data frame to display the results
results <- data.frame(
  Covariate = covariate_names,
  OR = round(OR, 2),
  CI = paste0("(", round(CI_lower, 2), " - ", round(CI_upper, 2), ")"),
  P_value = round(p_values, 3)
)

# View the results
print(results)

#---------

# Fit the GEE model for multivariable analysis
gee_model <- geeglm(
  BLDCOMPRETN ~ TRTn2 + PS + STENT + CATHPERIOD + OPRTIME + HOSPTIME, 
  data = mdata, 
  id = Center, 
  family = binomial(link = "logit"), 
  corstr = "exchangeable"
)
# Display the summary of the model
summary(gee_model)


# Extract the coefficients, standard errors, and p-values
summary_output <- summary(gee_model)$coefficients
coefficients <- summary_output[, "Estimate"]
std_errors <- summary_output[, "Std.err"]
p_values <- summary_output[, "Pr(>|W|)"]

# Extract the names of the covariates
covariate_names <- rownames(summary_output)[-1]  # Removing the intercept

# Check the names of the coefficients
print(covariate_names)

# Remove intercept if it is included
coefficients <- coefficients[-1]
std_errors <- std_errors[-1]
p_values <- p_values[-1]

# Calculate the odds ratios (OR) and 95% confidence intervals (CI)
OR <- exp(coefficients)
CI_lower <- exp(coefficients - 1.96 * std_errors)
CI_upper <- exp(coefficients + 1.96 * std_errors)

# Check if lengths match
print(length(coefficients))
print(length(std_errors))
print(length(p_values))

# Create a data frame to display the results
results <- data.frame(
  Covariate = covariate_names,
  OR = round(OR, 2),
  CI = paste0("(", round(CI_lower, 2), " - ", round(CI_upper, 2), ")"),
  P_value = round(p_values, 3)
)

# View the results
print(results)

#---------

# Check the performance of the multivariate model using AUC, 95% CI of AUC and p-value

# Ensure predicted probabilities are numeric
predicted_probs <- as.numeric(predict(gee_model, type = "response"))

# Extract observed outcomes
observed_outcomes <- mdata$BLDCOMPRETN

# Calculate ROC and AUC
roc_curve <- roc(observed_outcomes, predicted_probs)

# Extract AUC
auc_value <- auc(roc_curve)

# Calculate 95% CI for AUC using bootstrapping
ci_auc <- ci.auc(roc_curve, method = "bootstrap", boot.n = 2000)

# Perform p-value test manually (AUC = 0.5 as the null hypothesis)
boot_results <- replicate(2000, {
  shuffled_outcomes <- sample(observed_outcomes)  # Shuffle outcomes
  roc_shuffled <- roc(shuffled_outcomes, predicted_probs)
  auc(roc_shuffled)
})

# Calculate p-value as the proportion of bootstrap AUCs >= observed AUC
p_value <- mean(boot_results >= auc_value)

# Print the results
print(paste("AUC:", round(auc_value, 3)))
print(paste("95% CI for AUC:", paste(round(ci_auc[1:3], 3), collapse = " - ")))
print(paste("P-value:", round(p_value, 3)))

####################################################################################################
####################################################################################################

# Analysis for postoperative pain and analgesia

# Exclude cases where EPIDURAL is "Yes"
dataEx <- data[data$EPIDURAL != "Yes", ]
# Frequency table for EPIDURAL in dataEx
table(dataEx$EPIDURAL)

# Exclude cases where EPIDURAL is "Yes"
mdataEx <- mdata[mdata$EPIDURAL != "Yes", ]
# Frequency table for EPIDURAL in dataEx
table(mdataEx$EPIDURAL)


#Prepare Data to draw a histogram of the pain management scores, stratified by treatment group,
and show the percentage of patients in each score for both the data before matching (dataEx) 
and after matching
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Exclude cases where EPIDURAL is "Yes" in dataEx and mdataEx
dataEx <- data[data$EPIDURAL != "Yes", ]
mdataEx <- mdata[mdata$EPIDURAL != "Yes", ]

# Ensure PAIN and TRTn2 are factors
dataEx$PAIN <- as.factor(dataEx$PAIN)
mdataEx$PAIN <- as.factor(mdataEx$PAIN)


# check difference in pain score between treatment group and generate P-value for each plot

# Perform Fisher's Exact Test for dataEx dataset
fisher_dataEx <- fisher.test(table(dataEx$PAIN, dataEx$TRTn2))
cat("Fisher's Exact Test p-value for dataEx: ", fisher_dataEx$p.value, "\n")

# Perform Fisher's Exact Test for mdataEx dataset
fisher_mdataEx <- fisher.test(table(mdataEx$PAIN, mdataEx$TRTn2))
cat("Fisher's Exact Test p-value for mdataEx: ", fisher_mdataEx$p.value, "\n")


#Draw the plot and add P-value to the plot
# Step 1: Extract only the 'PAIN' and 'TRTn2' columns
mdataEx_selected <- mdataEx[, c("PAIN", "TRTn2")]
dataEx_selected <- dataEx[, c("PAIN", "TRTn2")]

# Step 2: Add the 'MatchingStatus' column to both datasets
mdataEx_selected$MatchingStatus <- "After matching (N=235), p<0.001"
dataEx_selected$MatchingStatus <- " Before matching (N=384), p<0.001"

# Step 3: Combine the datasets
combined_data <- rbind(mdataEx_selected, dataEx_selected)

# Step 4: Calculate percentages for stacked bars based on 'PAIN' and 'TRTn2'
combined_data_summary <- combined_data %>%
  group_by(MatchingStatus, TRTn2, PAIN) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(MatchingStatus, TRTn2) %>%
  mutate(Percent = (Count / sum(Count)) * 100)

# Step 5: Plot the stacked histogram
library(ggplot2)
p <- ggplot(combined_data_summary, aes(x = PAIN, y = Percent, fill = TRTn2)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Pain Management Score Stratified by Treatment Group and Matching Status",
       x = "1-Day Postoperative Pain Score", y = "Percentage of Patients (%)") +
  scale_fill_manual(values = c("Cohen group" = "#ADD8E6", "Lich-Gregoir group" = "#00008B")) +  # Custom colors
  facet_wrap(~ MatchingStatus, scales = "free_y") +  # Split by Matching Status (before vs after matching)
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove gridlines
    axis.title = element_text(size = 14),  # Set axis labels size
    axis.text = element_text(size = 12),  # Set axis text size
    strip.text = element_text(size = 12, face = "bold"),  # Bold facet labels
    panel.border = element_rect(color = "black", fill = NA),  # Add frame to the plot
    plot.title = element_text(hjust = 0.5, size = 16),  # Center the title
    axis.ticks = element_line(size = 0.5, color = "black"),  # Set tick line properties
    axis.ticks.length = unit(-0.2, "cm")  # Move ticks inside the plot
  )

# Step 6: Save the plot as a PNG file with larger dimensions
ggsave("Pain_Management_Score_Plot.png", plot = p, width = 12, height = 8, dpi = 300)



-------------------------------------------------------------------------------------
#Option 2 
# Draw the plot and add P-value to the plot
# Step 1: Extract only the 'PAIN' and 'TRTn2' columns
mdataEx_selected <- mdataEx[, c("PAIN", "TRTn2")]
dataEx_selected <- dataEx[, c("PAIN", "TRTn2")]

# Step 2: Add the 'MatchingStatus' column to both datasets
mdataEx_selected$MatchingStatus <- "After matching (N=235), p<0.001"
dataEx_selected$MatchingStatus <- " Before matching (N=384), p<0.001"

# Step 3: Combine the datasets
combined_data <- rbind(mdataEx_selected, dataEx_selected)

# Step 4: Calculate percentages for stacked bars based on 'PAIN' and 'TRTn2'
combined_data_summary <- combined_data %>%
  group_by(MatchingStatus, TRTn2, PAIN) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(MatchingStatus, TRTn2) %>%
  mutate(Percent = (Count / sum(Count)) * 100)

# Step 5: Plot the stacked histogram
library(ggplot2)
p <- ggplot(combined_data_summary, aes(x = PAIN, y = Percent, fill = TRTn2)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Pain Management Score Stratified by Treatment Group and Matching Status",
       x = "1-Day Postoperative Pain Score", y = "Percentage of Patients (%)") +
  scale_fill_manual(values = c("Cohen group" = "#ADD8E6", "Lich-Gregoir group" = "#00008B")) +  # Custom colors
  facet_wrap(~ MatchingStatus, scales = "fixed") +  # Set consistent y-axis for both facets
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove gridlines
    axis.title = element_text(size = 14),  # Set axis labels size
    axis.text = element_text(size = 12),  # Set axis text size
    strip.text = element_text(size = 12, face = "bold"),  # Bold facet labels
    panel.border = element_rect(color = "black", fill = NA),  # Add frame to the plot
    plot.title = element_text(hjust = 0.5, size = 16),  # Center the title
    axis.ticks = element_line(size = 0.5, color = "black"),  # Set tick line properties
    axis.ticks.length = unit(-0.2, "cm"),  # Move ticks inside the plot
    axis.text.y = element_text(size = 12),  # Adjust the y-axis text size
    strip.background = element_blank(),  # Remove background of facet labels
    panel.spacing = unit(0, "lines")  # Reduce space between panels
  )

# Step 6: Save the plot as a PNG file with larger dimensions
ggsave("Pain_Management_Score_Plot.png", plot = p, width = 12, height = 8, dpi = 300)


^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#Additional customization on option 2 - without the title, more space between the x title and the plot, 
without label name: - reported 

p <- ggplot(combined_data_summary, aes(x = PAIN, y = Percent, fill = TRTn2)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "
            1-Day Postoperative Pain Score", y = "Percentage of Patients (%)") +
  scale_fill_manual(values = c("Cohen group" = "#ADD8E6", "Lich-Gregoir group" = "#00008B"), name = NULL) +  # Remove legend title
  facet_wrap(~ MatchingStatus, scales = "fixed") +  # Set consistent y-axis for both facets
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove gridlines
    axis.title = element_text(size = 14),  # Set axis labels size
    axis.text = element_text(size = 12),  # Set axis text size
    strip.text = element_text(size = 12, face = "bold"),  # Bold facet labels
    panel.border = element_rect(color = "black", fill = NA),  # Add frame to the plot
    plot.title = element_text(hjust = 0.5, size = 16),  # Center the title
    axis.ticks = element_line(size = 0.5, color = "black"),  # Set tick line properties
    axis.ticks.length = unit(-0.2, "cm"),  # Move ticks inside the plot
    axis.text.y = element_text(size = 12),  # Adjust the y-axis text size
    strip.background = element_blank(),  # Remove background of facet labels
    panel.spacing = unit(0, "lines")  # Reduce space between panels
  )

# Save the plot
ggsave("Pain_Management_Score_Plot.png", plot = p, width = 12, height = 8, dpi = 300)

-----------------------------------------------------------------------------------------
#Option 3
# Draw the plot and add P-value to the plot
# Step 1: Extract only the 'PAIN' and 'TRTn2' columns
mdataEx_selected <- mdataEx[, c("PAIN", "TRTn2")]
dataEx_selected <- dataEx[, c("PAIN", "TRTn2")]

# Step 2: Add the 'MatchingStatus' column to both datasets
mdataEx_selected$MatchingStatus <- "After matching (N=235), p<0.001"
dataEx_selected$MatchingStatus <- " Before matching (N=384), p<0.001"

# Step 3: Combine the datasets
combined_data <- rbind(mdataEx_selected, dataEx_selected)

# Step 4: Calculate percentages for stacked bars based on 'PAIN' and 'TRTn2'
combined_data_summary <- combined_data %>%
  group_by(MatchingStatus, TRTn2, PAIN) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(MatchingStatus, TRTn2) %>%
  mutate(Percent = (Count / sum(Count)) * 100)

# Step 5: Plot the stacked histogram
library(ggplot2)
p <- ggplot(combined_data_summary, aes(x = PAIN, y = Percent, fill = TRTn2)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Pain Score Stratified by Treatment Group and Matching Status",
       x = "1-Day Postoperative Pain Score", y = "Percentage of Patients (%)") +
  scale_fill_manual(values = c("Cohen group" = "#ADD8E6", "Lich-Gregoir group" = "#00008B")) +  # Custom colors
  facet_wrap(~ MatchingStatus, scales = "fixed") +  # Set fixed y-axis scale across plots
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove gridlines
    axis.title = element_text(size = 14),  # Set axis labels size
    axis.text = element_text(size = 12),  # Set axis text size
    strip.text = element_text(size = 12, face = "bold"),  # Bold facet labels
    panel.border = element_rect(color = "black", fill = NA),  # Add frame to the plot
    plot.title = element_text(hjust = 0.5, size = 16),  # Center the title
    axis.ticks = element_line(size = 0.5, color = "black"),  # Set tick line properties
    axis.ticks.length = unit(-0.2, "cm")  # Move ticks inside the plot
  )

# Step 6: Save the plot as a PNG file with larger dimensions
ggsave("Pain_Score_Plot.png", plot = p, width = 12, height = 8, dpi = 300)


^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#Color

Option 1: Soft Pastel Colors
Cohen group: #FFB6C1 (Light Pink)
Lich-Gregoir group: #B0E0E6 (Powder Blue)
Option 2: Vibrant Colors
Cohen group: #FF6347 (Tomato Red)
Lich-Gregoir group: #32CD32 (Lime Green)
Option 3: Elegant Muted Tones
Cohen group: #8A2BE2 (Blue Violet)
Lich-Gregoir group: #D2691E (Chocolate)
Option 4: Bold Contrasting Colors
Cohen group: #FF4500 (Orange Red)
Lich-Gregoir group: #1E90FF (Dodger Blue)
Option 5: Earthy Tones
Cohen group: #C2B280 (Khaki)
Lich-Gregoir group: #6B8E23 (Olive Drab)
Option 6: Light and Dark Contrast
Cohen group: #ADD8E6 (Light Blue)
Lich-Gregoir group: #00008B (Dark Blue)


2 other colors:
"#FFD700",
"#90EE90"


-----------------------------------------------------------------------------------------
# Rosenbaum’s sensitivity analysis
-----------------------------------------
#Identify the Outcome Variables
BLDSPASM
HEMAT
VURRECG01
UTIpostop
BLDCOMPRETN 
BLDCOMPVOID 
BLDCOMPLEAK 
URCOMPOBST
URCOMPINJ
URCOMPSTEN
INCCOMPBLEED 
INCCOMPINF
INCCOMPSCAR 
ILEUS
ANASTOMO
URCOMPCVUR
GU30 
GU90
GUER30 
GUER90 
PROC365 


# Recall the Matchit object (Perform it if it was not done before)
> library(MatchIt)
m.out <- matchit(TRTn2 ~ PREOPVURGRADE + HISTOINJ + BMI  + AGS + PREOPRNPLN + PREOPRNSCR + VURSIDE + VURPHASE 
+ PREOPEPISODEn + PREOPTYPEUn + SURGTECHn2 + GENDER + UDR7M + SURGIND, data = data, method = "nearest", caliper = caliper_width)
summary(m.out, standardize=TRUE)

# Extract the matched dataset from the matchit object
mdata <- match.data(m.out)

#Outcomes should be numeric
mdata$BLDCOMPRETN <- ifelse(mdata$BLDCOMPRETN == "Yes", 1, 0)
mdata$BLDSPASM <- ifelse(mdata$BLDSPASM == "Yes", 1, 0)
mdata$HEMAT <- ifelse(mdata$HEMAT == "Yes", 1, 0)
mdata$UTIpostop <- ifelse(mdata$UTIpostop == "UTI", 1, 0)
mdata$BLDCOMPVOID <- ifelse(mdata$BLDCOMPVOID == "Yes", 1, 0)
mdata$BLDCOMPLEAK <- ifelse(mdata$BLDCOMPLEAK == "Yes", 1, 0)
mdata$URCOMPOBST <- ifelse(mdata$URCOMPOBST == "Yes", 1, 0)
mdata$URCOMPINJ <- ifelse(mdata$URCOMPINJ == "Yes", 1, 0)
mdata$URCOMPSTEN <- ifelse(mdata$URCOMPSTEN == "Yes", 1, 0)
mdata$INCCOMPBLEED <- ifelse(mdata$INCCOMPBLEED == "Yes", 1, 0)
mdata$INCCOMPINF <- ifelse(mdata$INCCOMPINF == "Yes", 1, 0)
mdata$INCCOMPSCAR <- ifelse(mdata$INCCOMPSCAR == "Yes", 1, 0)
mdata$ILEUS <- ifelse(mdata$ILEUS == "Yes", 1, 0)
mdata$ANASTOMO <- ifelse(mdata$ANASTOMO == "Yes", 1, 0)
mdata$URCOMPCVUR <- ifelse(mdata$URCOMPCVUR == "Yes", 1, 0)
mdata$GU30 <- ifelse(mdata$GU30 == "Yes", 1, 0)
mdata$GU90 <- ifelse(mdata$GU90 == "Yes", 1, 0)
mdata$GUER30 <- ifelse(mdata$GUER30 == "Yes", 1, 0)
mdata$GUER90 <- ifelse(mdata$GUER90 == "Yes", 1, 0)
mdata$PROC365 <- ifelse(mdata$PROC365 == "Yes", 1, 0)

# Primal analysis
------------------
# Assuming m.out is the MatchIt object and 'BLDCOMPVOID' is your binary outcome variable
# Extract matched pairs for outcomes and filter those who did not get matched (NA values)
> m.pairs <- cbind(
    mdata[row.names(m.out$match.matrix), 'BLDCOMPVOID'],  # Replace 'OUTCOME_VAR' with your binary outcome
    mdata[m.out$match.matrix, 'BLDCOMPVOID']
)

print(m.pairs)
m.pairs <- na.omit(m.pairs)
print(m.pairs)

# Optional: Check the structure of matched pairs
head(m.pairs)


# Why is Outcome Matching Necessary?
# The matched pairs control for observed confounders, isolating the treatment effect. 
# By focusing on discordant outcomes, Rosenbaum's method assesses the sensitivity of your results 
to unmeasured confounders.
# This approach ensures that the analysis is conducted within the framework of the matched design, 
maintaining the validity of causal inferences.


# Count discordant pairs
# Discordant pairs are pairs where one unit (treated or control) has the outcome, but the other does not.
# Let x represent the number of pairs where the control has the outcome, but the treated does not.
# Let y represent the number of pairs where the treated has the outcome, but the control does not.
> x <- sum((m.pairs[, 1] == 0) & (m.pairs[, 2] == 1))
> y <- sum((m.pairs[, 1] == 1) & (m.pairs[, 2] == 0))
print(x)
print(y) 

# Perform Rosenbaum primal sensitivity analysis
install.packages("rbounds")
> library(rbounds) 
> primal <- binarysens(x=x, y=y, Gamma = 15, GammaInc = 2)
> print(primal)

##Primal analysis asks:
it asks: "How strong would this unknown confounder need to be for it to change the conclusions 
we draw about the effect of the treatment?" This is typically assessed in sensitivity analysis 
to understand the robustness of the results against unmeasured factors."

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^6
How to extract the matched pairs in the matched data (mdata):

matched_pairs <- mdata %>% group_by(subclass) %>% filter(n() == 2)
matched_pairs %>%
  arrange(subclass) %>%
  select(subclass, Case)
pairing_table <- matched_pairs %>%
  group_by(subclass) %>%
  summarise(Case1 = Case[1], Case2 = Case[2], .groups = "drop")
print(pairing_table)
write.csv(pairing_table, "matched_pairs.csv", row.names = FALSE)



# First, make sure Case1 and Case2 are of type character to match Case in mdata
pairing_table$Case1 <- as.character(pairing_table$Case1)
pairing_table$Case2 <- as.character(pairing_table$Case2)

# Merge pairing_table with mdata for Case1
merged_Case1 <- merge(pairing_table, mdata, by.x = "Case1", by.y = "Case", all.x = TRUE)

# Merge the result with mdata for Case2
merged_Case2 <- merge(merged_Case1, mdata, by.x = "Case2", by.y = "Case", all.x = TRUE, suffixes = c("_Case1", "_Case2"))

# Now you can access the BLDCOMPVOID values for both Case1 and Case2
# Example:
head(merged_Case2)

----------------------------------------------------------------------------------------

Another model for sensitivity analysis:
----------------------------------------

caliper_width <- 0.1 * sd_logit_ps  # A more conservative caliper width

 m.out2 <- matchit(TRTn2 ~ PREOPVURGRADE + HISTOINJ + BMI  + AGS + PREOPRNPLN_impv + PREOPRNSCR + VURSIDE + VURPHASE + PREOPEPISODEn_impv + PREOPTYPEUn_impv + SURGTECHn2, data = data2, method = "nearest", caliper = caliper_width)
 summary(m.out2, standardize=TRUE)

 # Extract the matched dataset from the matchit object
mdata <- match.data(m.out2)
# Create the table for the matched data with your specified variables
 table6 <- CreateTableOne(
   vars = c('PREOPVURGRADE', 'HISTOINJ', 'BMI', 'AGS', 'PREOPRNPLN_impv', 
            'PREOPRNSCR', 'VURSIDE', 'VURPHASE', 'PREOPEPISODEn_impv', 
            'PREOPTYPEUn_impv', 'SURGTECHn2'),
   data = mdata,
   factorVars = c('PREOPVURGRADE', 'HISTOINJ', 'PREOPRNPLN_impv', 'PREOPRNSCR','VURSIDE', 'VURPHASE','SURGTECHn2','PREOPTYPEUn_impv', 'PREOPEPISODEn_impv'),  # Categorical variables
   strata = 'TRTn2',  # Adjust if needed based on your dataset
   smd = TRUE          # Include standardized mean differences
 )
# Print the table with SMDs and other options
 table6 <- print(
   table6,
   smd = TRUE,               # Include standardized mean differences
   showAllLevels = TRUE,     # Show all levels for factor variables
   noSpaces = TRUE,          # Remove spaces in output
   printToggle = FALSE       # Don't print a toggle for summary
 )
 table6

write.csv(table6, file = "Table6_after_matching.csv")
> love.plot(bal.tab(m.out2, m.threshold=0.1), stat = "mean.diffs", grid=TRUE, stars="raw", abs = FALSE, sample.names = c("Before Matching", "After Matching"))

> bal.plot(
  m.out2,
  var.name = "distance",
  which = "both",
  type = "histogram",
  mirror = TRUE,
  sample.names = c("Before Matching", "After Matching")
)

# To adjust the treatment label:

library(ggplot2)

# Generate the plot
p <- bal.plot(
  m.out2,
  var.name = "distance",
  which = "both",
  type = "histogram",
  mirror = TRUE,
  sample.names = c("Before Matching", "After Matching")
)

# Update treatment labels in the legend
p + scale_fill_discrete(labels = c("Cohen group", "Lich-Gregoir group")) +
  scale_color_discrete(labels = c("Cohen group", "Lich-Gregoir group"))
----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------
#Secondary objective: Association between Complications, Readmissions, ER visits and Reoperations
-----------------------------------------------------------------------------------------------

#KNN imputation or removing missing values from CLAVIENCLASS,CLAVIENCLASS2, URCOMPCVUR:

missing in outcomes %:
BLDLOSS 1.9%
CLAVIENCLASS 22%
PAIN 0.9%
URCOMPCVUR 8.7%

# Imputation - I need to select the set of variables appropriate for the imputation of these outcomes

#Check Impact: After imputing, you may want to check if the imputation has changed the distribution of 
your variables (e.g., by plotting histograms before and after imputation or running descriptive 
statistics).
#You can impute some variables with one set of covariates and others with another set, as long as 
the covariates you choose are relevant to the variable being imputed and do not introduce bias.


# If the proportion of missing outcome data is small, excluding them might not drastically 
affect the statistical power. 

NOTE: Baseline characteristics are predictive but outcomes are critical, so I will choose to remove the 
missing values from outcomes instead of imputing them:
complications and readmissions are your outcomes of interest, and imputing them could undermine 
the accuracy of your conclusions. Additionally, outcome data might be more likely to be missing 
due to unobserved factors that could affect the results (e.g., patients who were readmitted may 
not have complete records), which complicates imputation.

-------------

#Recode variable

# Load the dplyr package
library(dplyr)

To recode the variable in the original dataset "data" or matched dataset "mdata"

# Recode the CLAVIENCLASS to create a new variable CLAVIENCLASS2
data <- data %>%
  mutate(CLAVIENCLASS2 = recode(CLAVIENCLASS, 
                               `1` = "Clavien grade 1-2", 
                               `2` = "Clavien grade 1-2", 
                               `3` = "Clavien grade 3-5", 
                               `4` = "Clavien grade 3-5", 
                               `5` = "Clavien grade 3-5"))

# Check the structure of the new variable
table(data$CLAVIENCLASS2)

OR

# Recode the CLAVIENCLASS to create a new variable CLAVIENCLASS2
mdata <- mdata %>%
  mutate(CLAVIENCLASS2 = recode(CLAVIENCLASS, 
                               `1` = "Clavien grade 1-2", 
                               `2` = "Clavien grade 1-2", 
                               `3` = "Clavien grade 3-5", 
                               `4` = "Clavien grade 3-5", 
                               `5` = "Clavien grade 3-5"))

# Check the structure of the new variable
table(mdata$CLAVIENCLASS2)

OR (according to the levels)
mdata <- mdata %>%
  mutate(CLAVIENCLASS2 = recode(CLAVIENCLASS, 
                               `Clavien grade 1` = "Clavien grade 1-2", 
                               `Clavien grade 2` = "Clavien grade 1-2", 
                               `Clavien grade 3` = "Clavien grade 3-5", 
                               `Clavien grade 4` = "Clavien grade 3-5", 
                               `Clavien grade 5` = "Clavien grade 3-5"))

# Check the structure of the new variable
table(mdata$CLAVIENCLASS2)

-------------

#Remove missing values

# Load the tidyr package
library(tidyr)

# Check the result before removal
sum(is.na(mdata$CLAVIENCLASS))
sum(is.na(mdata$CLAVIENCLASS2))
sum(is.na(mdata$URCOMPCVUR))

# Drop rows with missing values in the CLAVIENCLASS column
mdata_clean <- mdata %>% drop_na(CLAVIENCLASS,CLAVIENCLASS2,URCOMPCVUR)

# Check the result after removal
sum(is.na(mdata_clean$CLAVIENCLASS))  # Should be 0 if no missing values in CLAVIENCLASS
sum(is.na(mdata_clean$CLAVIENCLASS2))
sum(is.na(mdata_clean$URCOMPCVUR))

-----------------
# Ensure the type of the dependent variable is numeric

mdata$GU30 <- ifelse(mdata$GU30 == "Yes", 1, 0)
mdata$GU90 <- ifelse(mdata$GU90 == "Yes", 1, 0)
mdata$GUER30 <- ifelse(mdata$GUER30 == "Yes", 1, 0)
mdata$GUER90 <- ifelse(mdata$GUER90 == "Yes", 1, 0)
mdata$PROC365 <- ifelse(mdata$PROC365 == "Yes", 1, 0)


mdata_clean$GU30 <- ifelse(mdata_clean$GU30 == "Yes", 1, 0)
mdata_clean$GU90 <- ifelse(mdata_clean$GU90 == "Yes", 1, 0)
mdata_clean$GUER30 <- ifelse(mdata_clean$GUER30 == "Yes", 1, 0)
mdata_clean$GUER90 <- ifelse(mdata_clean$GUER90 == "Yes", 1, 0)
mdata_clean$PROC365 <- ifelse(mdata_clean$PROC365 == "Yes", 1, 0)

-----------------

install.packages("geepack")
library(geepack)

#NOTE : Use  mdata_clean for the variables: CLAVIENCLASS2, URCOMPCVUR

# Fit the GEE model in univariate analysis
gee_model <- geeglm(
  GU30 ~ URCOMPCVUR,
  data = mdata_clean,
  id = Center,           # Account for clustering by Center
  family = binomial(link = "logit") # Binary outcome with logit link
)


# Fit the GEE model in multivariate analysis
gee_model <- geeglm(
  GU30 ~ URCOMPCVUR + PS + TRTn2 + CATHPERIOD + STENT + OPRTIME + HOSPTIME,
  data = mdata_clean,
  id = Center,           # Account for clustering by Center
  family = binomial(link = "logit") # Binary outcome with logit link
)

---------------

#NOTE : Use  mdata for the other variables
#NOTE : Remove STENT from the multivariable analysis for the variables: INCCOMPSCAR, ANASTOMO

# Fit the GEE model in univariate analysis
gee_model <- geeglm(
  GU30 ~ BLDCOMPRETN,
  data = mdata,
  id = Center,           # Account for clustering by Center
  family = binomial(link = "logit") # Binary outcome with logit link
)


# Fit the GEE model in multivariate analysis
gee_model <- geeglm(
  GU30 ~ BLDCOMPRETN + PS + TRTn2 + CATHPERIOD + STENT + OPRTIME + HOSPTIME,
  data = mdata,
  id = Center,           # Account for clustering by Center
  family = binomial(link = "logit") # Binary outcome with logit link
)

---------------------

# Extract the coefficients, standard errors, and p-values
summary_output <- summary(gee_model)$coefficients
coefficients <- summary_output[, "Estimate"]
std_errors <- summary_output[, "Std.err"]
p_values <- summary_output[, "Pr(>|W|)"]

# Extract the names of the covariates
covariate_names <- rownames(summary_output)[-1]  # Removing the intercept

# Check the names of the coefficients
print(covariate_names)

# Remove intercept if it is included
coefficients <- coefficients[-1]
std_errors <- std_errors[-1]
p_values <- p_values[-1]

# Calculate the odds ratios (OR) and 95% confidence intervals (CI)
OR <- exp(coefficients)
CI_lower <- exp(coefficients - 1.96 * std_errors)
CI_upper <- exp(coefficients + 1.96 * std_errors)

# Check if lengths match
print(length(coefficients))
print(length(std_errors))
print(length(p_values))

# Create a data frame to display the results
results <- data.frame(
  Covariate = covariate_names,
  OR = round(OR, 2),
  CI = paste0("(", round(CI_lower, 2), " - ", round(CI_upper, 2), ")"),
  P_value = round(p_values, 3)
)

# View the results
print(results)

# Display the p-value with more precision
print(p_values)
---------------------------
Complications:

CLAVIENCLASS2 (we will not use CLAVIENCLASS, just to simplify the interpretation)
BLDCOMPRETN 
BLDCOMPVOID 
BLDCOMPLEAK 
URCOMPOBST
URCOMPINJ
URCOMPSTEN
INCCOMPBLEED 
INCCOMPINF
INCCOMPSCAR 
ILEUS
ANASTOMO
URCOMPCVUR

Test them but not for the report:

BLDSPASM
HEMAT
UTIpostop

Associations with:

GU30 
GU90
GUER30 
GUER90 
PROC365 

-----------
# Check the performance of the multivariate model using AUC, 95% CI of AUC and p-value

install.packages("pROC")
library(pROC)

# Ensure predicted probabilities are numeric
predicted_probs <- as.numeric(predict(gee_model, type = "response"))

# Extract observed outcomes
observed_outcomes <- mdata$BLDCOMPRETN

# Calculate ROC and AUC
roc_curve <- roc(observed_outcomes, predicted_probs)

# Extract AUC
auc_value <- auc(roc_curve)

# Calculate 95% CI for AUC using bootstrapping
ci_auc <- ci.auc(roc_curve, method = "bootstrap", boot.n = 2000)

# Perform p-value test manually (AUC = 0.5 as the null hypothesis)
boot_results <- replicate(2000, {
  shuffled_outcomes <- sample(observed_outcomes)  # Shuffle outcomes
  roc_shuffled <- roc(shuffled_outcomes, predicted_probs)
  auc(roc_shuffled)
})

# Calculate p-value as the proportion of bootstrap AUCs >= observed AUC
p_value <- mean(boot_results >= auc_value)

# Print the results
print(paste("AUC:", round(auc_value, 3)))
print(paste("95% CI for AUC:", paste(round(ci_auc[1:3], 3), collapse = " - ")))
print(paste("P-value:", round(p_value, 3)))


//////////////////DRAFT ///////////////////////////////////DRAFT ///////////////////////////////////DRAFT /////////////////

#To save the R commands: - not perfecly work , some mannual change to the document to implement

# Clear the R console history
flush.console()

# Clear the history file (deletes old history)
file.remove(".Rhistory")

# Load necessary libraries
library(officer)
library(flextable)
library(magrittr)

# Create a Word document
doc <- read_docx()

# Save the Word document (empty at this point)
print(doc, target = "Output_VUR_Article_2.docx")

# Save the current session history to a temporary file (after clearing old history)
history_file <- tempfile()  # Create a temporary file to store history
savehistory(history_file)   # Save current session's history to this file

# Add a new command to the history file
writeLines("cat('New Command: Example of adding content to the session history')", history_file, append = TRUE)

# Create a Word document with code content
doc <- doc %>%
  body_add_par("Code used to create this document", style = "heading 1") %>%
  body_add_par("# Clear the R console history", style = "Normal") %>%
  body_add_par("flush.console()", style = "Normal") %>%
  body_add_par("# Clear the history file (deletes old history)", style = "Normal") %>%
  body_add_par("file.remove('.Rhistory')", style = "Normal") %>%
  body_add_par("# Load necessary libraries", style = "Normal") %>%
  body_add_par("library(officer)", style = "Normal") %>%
  body_add_par("library(flextable)", style = "Normal") %>%
  body_add_par("library(magrittr)", style = "Normal") %>%
  body_add_par("# Create a Word document", style = "Normal") %>%
  body_add_par("doc <- read_docx()", style = "Normal") %>%
  body_add_par("# Save the Word document (empty at this point)", style = "Normal") %>%
  body_add_par("print(doc, target = 'Output_VUR_Article_2.docx')", style = "Normal") %>%
  body_add_par("# Save the current session history to a temporary file (after clearing old history)", style = "Normal") %>%
  body_add_par("history_file <- tempfile()", style = "Normal") %>%
  body_add_par("savehistory(history_file)", style = "Normal") %>%
body_add_par("# New Command Example", style = "Normal") %>% 
  body_add_par("cat('New Command: Example of adding content to the session history')", style = "Normal")

# Add the second section about copying content
doc <- doc %>%
  body_add_par("Code used to copy content to this document", style = "heading 1") %>%
  body_add_par("# Read the commands from the history file", style = "Normal") %>%
  body_add_par("commands <- readLines(history_file)", style = "Normal") %>%
  body_add_par("# Add commands from the current session to the Word document", style = "Normal") %>%
  body_add_par("for (cmd in commands) { doc <- doc %>% body_add_par(cmd, style = 'Normal') }", style = "Normal") %>%
  body_add_par("# Save the Word document", style = "Normal") %>%
  body_add_par("print(doc, target = 'Output_VUR_Article_2.docx')", style = "Normal")

# Add the third section about copying content
doc <- doc %>%
  body_add_par("All commands in the current session", style = "heading 1")

# Read the commands from the history file
commands <- readLines(history_file)  # Read all commands

... write the commands
...
...


# Add commands from the current session to the Word document
for (cmd in commands) {
  doc <- doc %>%
    body_add_par(cmd, style = "Normal")
}

# Save the Word document
print(doc, target = "Output_VUR_Article_2.docx")

//////////////////DRAFT ///////////////////////////////////DRAFT ///////////////////////////////////DRAFT /////////////////


