# Statistical Analysis Plan (SAP) Template
## Real-World Evidence Study

**Study Title:** [Insert Study Title]

**SAP Version:** [Version Number]
**SAP Date:** [Date]
**Protocol Version:** [Reference Protocol Version]

**Statistician:** [Name, Credentials]
**Co-Statisticians:** [Names if applicable]

---

## Document History

| Version | Date | Changes | Author |
|---------|------|---------|--------|
| 1.0 | [Date] | Initial version | [Name] |

---

## Table of Contents

1. [Study Overview](#1-study-overview)
2. [Study Objectives and Endpoints](#2-study-objectives-and-endpoints)
3. [Sample Size and Power](#3-sample-size-and-power)
4. [Analysis Populations](#4-analysis-populations)
5. [General Statistical Considerations](#5-general-statistical-considerations)
6. [Data Handling and Transformations](#6-data-handling-and-transformations)
7. [Baseline Characteristics](#7-baseline-characteristics)
8. [Propensity Score Analysis](#8-propensity-score-analysis)
9. [Primary Analysis](#9-primary-analysis)
10. [Secondary Analyses](#10-secondary-analyses)
11. [Subgroup Analyses](#11-subgroup-analyses)
12. [Sensitivity Analyses](#12-sensitivity-analyses)
13. [Safety Analyses](#13-safety-analyses)
14. [Interim Analyses](#14-interim-analyses)
15. [Software and Reproducibility](#15-software-and-reproducibility)

---

## 1. Study Overview

### 1.1 Study Design
[Brief description of study design from protocol]

### 1.2 Data Source
[Description of RWD source]

### 1.3 Study Period
- **Index Date Period:** [Dates]
- **Follow-up Period:** [Duration]
- **Data Lock Date:** [Date]

---

## 2. Study Objectives and Endpoints

### 2.1 Primary Objective
[State primary objective]

### 2.2 Primary Endpoint
**Endpoint Definition:** [Detailed definition]
**Type:** [Binary, Time-to-event, Continuous]
**Assessment Window:** [Timing]

### 2.3 Secondary Endpoints
1. **[Endpoint Name]**
   - Type: [...]
   - Definition: [...]

2. **[Endpoint Name]**
   - Type: [...]
   - Definition: [...]

### 2.4 Exploratory Endpoints
[List exploratory endpoints]

---

## 3. Sample Size and Power

### 3.1 Target Sample Size
**Total Sample Size:** [N]
**Per Group:** [n1, n2]

### 3.2 Power Calculation
**Primary Endpoint:**
- Assumed effect size: [HR/OR/RR] = [value]
- Power: [e.g., 80%]
- Significance level: α = 0.05 (two-sided)
- Expected event rate: [%]

**Calculation Details:**
```r
# Power calculation using rwevidence
library(rwevidence)

# [Include actual power calculation code]
```

### 3.3 Sample Size Adjustments
[Account for matching ratio, expected attrition, etc.]

---

## 4. Analysis Populations

### 4.1 Full Analysis Set (FAS)
**Definition:** All patients meeting inclusion/exclusion criteria with at least one post-baseline assessment

**Justification:** Approximates intention-to-treat principle in observational setting

### 4.2 Per-Protocol Set (PPS)
**Definition:** Subset of FAS with:
- Complete follow-up
- No major protocol deviations
- [Additional criteria]

### 4.3 Safety Analysis Set
**Definition:** All patients with at least one safety assessment post-index

### 4.4 Propensity Score Matched Set
**Definition:** Subset of FAS successfully matched on propensity scores

---

## 5. General Statistical Considerations

### 5.1 Statistical Hypothesis Testing
**Null Hypothesis (H₀):** [Statement]
**Alternative Hypothesis (H₁):** [Statement]

**Significance Level:** α = 0.05 (two-sided)
**Confidence Intervals:** 95% (two-sided)

### 5.2 Multiple Comparison Adjustment
**Approach:** [e.g., Hochberg, Bonferroni, or No adjustment with rationale]

**Hierarchy of Endpoints:**
1. Primary endpoint
2. Key secondary endpoint 1
3. Key secondary endpoint 2

### 5.3 Handling of Missing Data
**Assessment:**
- Quantify missingness by variable and treatment group
- Assess patterns (MCAR, MAR, MNAR)
- Compare characteristics of complete vs incomplete cases

**Primary Approach:**
[e.g., Multiple imputation using MICE]
- Number of imputations: m = [20]
- Imputation model variables: [List]
- Pooling method: Rubin's rules

**Sensitivity Analysis:**
- Complete case analysis
- [Alternative imputation method]
- Tipping point analysis for primary endpoint

### 5.4 Outliers
**Definition:** Values > 3 SD from mean or [other definition]
**Handling:** [Describe approach - transform, exclude with sensitivity analysis, etc.]

---

## 6. Data Handling and Transformations

### 6.1 Derived Variables

#### 6.1.1 Treatment Exposure
```r
# Define treatment exposure
treatment_exposed <- [operational definition]
```

#### 6.1.2 Baseline Period
**Definition:** [Time window before index date]
**Lookback:** [Duration, e.g., 180 days]

#### 6.1.3 Follow-up Time
```r
# Calculate follow-up time
followup_time <- min(
  time_to_event,
  time_to_censoring,
  administrative_censoring_date
)
```

### 6.2 Comorbidity Scores

#### Charlson Comorbidity Index
```r
# Calculate using rwevidence
cci <- calculate_charlson_index(
  diagnosis_codes,
  reference_date = index_date
)
```

#### Elixhauser Comorbidity Index
```r
# Calculate using rwevidence
eci <- calculate_elixhauser_index(
  diagnosis_codes,
  reference_date = index_date
)
```

### 6.3 Categorization of Continuous Variables
[Specify cut-points for continuous variables]

---

## 7. Baseline Characteristics

### 7.1 Summary Statistics

**Continuous Variables:**
- Mean, SD
- Median, Q1, Q3, min, max
- Number of missing values

**Categorical Variables:**
- N, percentage
- Number of missing values

### 7.2 Baseline Table (Table 1)
```r
# Generate Table 1 using rwevidence
table1 <- rwe_table_one(
  data = study_data,
  treatment = "treatment_group",
  vars = c(
    "age", "sex", "race",
    "comorbidity_score",
    "baseline_labs",
    # ... additional variables
  ),
  strata = NULL
)
```

**Variables to include:**
- Demographics
- Disease characteristics
- Comorbidities
- Concomitant medications
- Baseline lab values
- Healthcare utilization

### 7.3 Balance Assessment (Pre-matching)
- Standardized mean differences (SMD)
- Visual assessment (Love plot)

---

## 8. Propensity Score Analysis

### 8.1 Propensity Score Model

**Outcome Variable:** Treatment assignment (binary)

**Covariates:**
[List all variables included in PS model, organized by category]

**Model Type:**
- Primary: [Logistic regression / GBM / Random Forest]
- Sensitivity: [Alternative method]

**Model Development:**
```r
# Estimate propensity scores
ps_model <- rwe_propensity_score(
  data = study_data,
  treatment = "treatment_group",
  covariates = c(
    "age", "sex", "race",
    "comorbidity_score",
    # ... additional covariates
  ),
  method = "logistic"  # or "gbm", "random_forest"
)
```

### 8.2 Propensity Score Diagnostics

**Distribution Assessment:**
- Histograms by treatment group
- Overlap assessment
- Common support region

**Trimming:**
- Trim PS at [e.g., 0.05 and 0.95 percentiles]
- Document number of patients excluded

### 8.3 Propensity Score Application

#### 8.3.1 Matching
**Method:** [Nearest neighbor / Optimal / Caliper]
**Ratio:** [1:1 / 1:k]
**Caliper:** [Width, e.g., 0.2 * SD of logit(PS)]
**Replacement:** [Yes / No]

```r
# Perform matching
matched_cohort <- rwe_match(
  ps_model,
  method = "nearest",
  ratio = 1,
  caliper = 0.1
)
```

#### 8.3.2 Weighting (Alternative to Matching)
**Weight Type:** [IPTW / ATT / ATO]

```r
# Calculate IPTW weights
iptw_weights <- rwe_calculate_iptw(
  ps_model,
  weight_type = "ate"
)
```

**Weight Diagnostics:**
- Weight distribution (mean, median, range)
- Effective sample size
- Weight stabilization if needed

### 8.4 Balance Assessment (Post-matching/weighting)

**Target:** SMD < 0.10 for all covariates

**Assessment Methods:**
- Standardized mean differences
- Variance ratios
- Love plots
- Empirical CDF plots

**Handling Residual Imbalance:**
[Describe approach if balance is not achieved]

---

## 9. Primary Analysis

### 9.1 Analysis Method

**Primary Endpoint:** [Endpoint name]

**Statistical Model:**
[e.g., Cox proportional hazards model for time-to-event]

**Model Specification:**
```r
# Primary analysis model
primary_model <- coxph(
  Surv(time, event) ~ treatment_group + [adjusting covariates],
  data = matched_cohort
)
```

**Adjusting Covariates:**
[List any covariates included in outcome model beyond treatment]

**Effect Estimate:** Hazard Ratio (HR)
**Confidence Interval:** 95% CI
**P-value:** Two-sided

### 9.2 Model Assumptions

**For Cox Model:**
- Proportional hazards assumption (Schoenfeld residuals test)
- Linearity of continuous covariates (martingale residuals)
- No influential outliers (dfbeta)

**Handling Violations:**
[Describe plan if assumptions are violated]

### 9.3 Presentation of Results

**Primary Results Table:**
| Endpoint | Treatment | Control | HR (95% CI) | P-value |
|----------|-----------|---------|-------------|---------|
| [Primary] | [n, events, rate] | [n, events, rate] | [HR] | [p] |

**Kaplan-Meier Curves:**
```r
# Generate KM plot
km_plot <- rwe_kaplan_meier(
  data = matched_cohort,
  time = "time",
  event = "event",
  treatment = "treatment_group"
)
```

---

## 10. Secondary Analyses

### 10.1 Secondary Endpoint 1: [Name]

**Analysis Method:** [Describe statistical approach]
**Model Specification:** [Include model formula]

### 10.2 Secondary Endpoint 2: [Name]

**Analysis Method:** [Describe statistical approach]
**Model Specification:** [Include model formula]

### 10.3 Additional Secondary Endpoints
[Repeat for each secondary endpoint]

---

## 11. Subgroup Analyses

### 11.1 Pre-specified Subgroups

1. **Age:** <65 years vs ≥65 years
2. **Sex:** Male vs Female
3. **Disease Stage:** [Categories]
4. **Baseline Severity:** [Categories]
5. **[Additional subgroups]**

### 11.2 Analysis Method

**Approach:** Fit separate models within each subgroup

**Interaction Testing:**
```r
# Test treatment-by-subgroup interaction
interaction_model <- coxph(
  Surv(time, event) ~ treatment * subgroup_variable,
  data = matched_cohort
)
```

**Significance Level for Interactions:** α = 0.10

### 11.3 Multiple Comparison Consideration
[State approach to multiplicity - typically no adjustment for exploratory subgroups]

### 11.4 Forest Plots
```r
# Generate forest plot
forest_plot <- rwe_forest_plot(
  results = subgroup_results,
  outcome = "primary_endpoint"
)
```

---

## 12. Sensitivity Analyses

### 12.1 Sensitivity Analysis 1: Alternative PS Method
**Rationale:** Assess robustness to PS model specification
**Method:** [e.g., GBM instead of logistic regression]

### 12.2 Sensitivity Analysis 2: Complete Case Analysis
**Rationale:** Assess impact of missing data handling
**Method:** Restrict to patients with complete data

### 12.3 Sensitivity Analysis 3: Alternative Matching Ratio
**Rationale:** Maximize sample size
**Method:** 1:2 matching instead of 1:1

### 12.4 Sensitivity Analysis 4: E-value
**Rationale:** Assess potential unmeasured confounding
**Method:** Calculate E-value for primary effect estimate

```r
# Calculate E-value
e_value <- calculate_e_value(
  hr = primary_hr,
  ci_lower = primary_ci_lower
)
```

### 12.5 Additional Sensitivity Analyses
[List any additional sensitivity analyses]

---

## 13. Safety Analyses

### 13.1 Adverse Events

**Analysis Population:** Safety analysis set

**Summary Tables:**
- Frequency and proportion of patients with any AE
- Frequency by severity grade (CTCAE)
- Frequency by relationship to treatment
- Serious adverse events (SAEs)

### 13.2 Statistical Methods

**Incidence Rate:**
```r
# Calculate incidence rates
safety_analysis <- rwe_analyze_safety(
  data = safety_data,
  events = "ae_term",
  person_time = "person_years",
  treatment = "treatment_group"
)
```

**Rate Ratio:** Poisson regression
**Confidence Intervals:** 95% (exact method for rare events)

### 13.3 Safety Signal Detection
[Describe methods for signal detection if applicable]

---

## 14. Interim Analyses

**Planned Interim Analyses:** [Number and timing]

**Stopping Rules:** [If applicable]

**Alpha Spending:** [If applicable]

---

## 15. Software and Reproducibility

### 15.1 Statistical Software

**Primary Software:**
- R version: [4.4.1 or later]
- rwevidence package version: 0.1.0

**Key Packages:**
```r
library(rwevidence)   # v0.1.0
library(survival)     # Cox regression, KM curves
library(dplyr)        # Data manipulation
library(ggplot2)      # Visualization
```

### 15.2 Reproducibility

**Random Seed:** `set.seed(12345)` for all stochastic procedures

**Version Control:**
- Analysis scripts maintained in Git repository
- Repository: [URL]

**Audit Trail:**
- All analyses logged using rwevidence audit features
- Session info captured for reproducibility

### 15.3 Analysis Scripts

**Script Organization:**
```
analysis/
├── 01_data_preparation.R
├── 02_data_quality.R
├── 03_propensity_scores.R
├── 04_matching.R
├── 05_primary_analysis.R
├── 06_secondary_analyses.R
├── 07_subgroup_analyses.R
├── 08_sensitivity_analyses.R
├── 09_safety_analyses.R
└── 10_generate_tables_figures.R
```

---

## Appendices

### Appendix A: Programming Specifications
[Detailed specifications for all tables, listings, and figures]

### Appendix B: Data Derivations
[Detailed derivation logic for all analysis variables]

### Appendix C: Model Diagnostics
[Specifications for model diagnostic checks]

### Appendix D: Handling of Protocol Deviations
[Pre-specified approach to handling deviations]

---

**SAP Approval:**

Lead Statistician: _________________________ Date: _________

Co-Statistician: _________________________ Date: _________

Principal Investigator: _________________________ Date: _________

---

**SAP Amendments:**

| Amendment | Date | Description | Approved By |
|-----------|------|-------------|-------------|
| [No.] | [Date] | [Description] | [Name] |

---

*This SAP template is generated using the rwevidence R package (v0.1.0)*
*For more information: https://github.com/singhg039/rwe_evidence_R_package*
