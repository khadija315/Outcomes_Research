# Outcomes_Research
Outcomes of Open Cohen vs. Lich-Gregoir Ureteral Reimplantation for Pediatric VUR

This repository contains the dataset and code for the study, as well as the manuscript figures.

---
##  Purpose

This study compared postoperative outcomes in children for whom either technique was clinically appropriate and assessed whether ureteral stenting modified treatment effects.

---

## Study Population

- **Design**: Multicenter retrospective cohort study 
- **Participants**: 412 children with **primary unilateral dilating VUR (grades III–V)**
- **Inclusion criteria**:
  - Underwent open ureteral reimplantation (Cohen or Lich-Gregoir) (June 2010– September 2022)
  - ≥1-year postoperative follow-up
  - >1 year at surgery
- **Exclusion criteria**:
  - Had secondary VUR
  - Had repeated or tapering reimplantation
  - Had bladder and bowel dysfunction
  - Had posterior urethral valves
  - Had previous bladder surgery
  - Had reflux-related anomalies (ureteroceles, duplex systems, megaureter, and ectopic ureter)
  - Suspected renal failure associated with multicystic dysplasia kidney
---

##  Dataset
The dataset includes anonymized variables.

---
##  Analysis
- Overlap propensity score weighting to improve comparability between treatment groups while retaining all eligible children in the analysis.
- Treatment effects were estimated using center-clustered generalized estimating equations and Cox models
- Bonferroni–Holm adjustment for multiple comparisons correction.
- Sensitivity analyses:
    - 1:1 propensity score matching
    - E-values
    - Rosenbaum bounds

---
##  Results Summary

- **Main results**: 
Treatment effects: In overlap-weighted multivariable analyses, LG was associated with:
  - Higher odds of ureteral obstruction (OR 3.67, 95% CI 2.21–6.10)
  - Lower odds of ureteral injury (OR 0.43, 95% CI 0.27–0.69)
  - Lower odds of surgical-site complications (OR 0.72, 95% CI 0.58–0.90)
  - Lower odds of bowel restriction (OR 0.10, 95% CI 0.04–0.25)
  - Lower odds of 90-day readmission (OR 0.20, 95% CI 0.16–0.26)
Effect modification by stenting:
  - LG was associated with lower odds of surgical-site complications than Cohen, particularly among stented children (OR, 0.05; 95% CI, 0.02–0.14 vs. OR, 0.81; 95% CI, 0.69–0.96 in non-stented children)
  - LG was associated with lower odds of moderate-to-severe pain, particularly among non-stented children (OR, 0.17; 95% CI, 0.14–0.20). 

---
## Repository structure
- `data/` – analysis dataset
- `Figures_manuscript/` – manuscript figures
- `Outcome_Project.Rproj` – RStudio project file
- `Outcome_analysis_03092026_shared_clean_version.qmd` – R code for data preparation and statistical analyses

Analyses were conducted using R version 4.5.2
