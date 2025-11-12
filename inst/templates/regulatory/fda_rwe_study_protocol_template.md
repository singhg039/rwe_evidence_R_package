# Real-World Evidence Study Protocol Template
## FDA Submission Format

**Study Title:** [Insert Study Title]

**Protocol Version:** [Version Number]
**Protocol Date:** [Date]

**Sponsor:** [Company/Organization Name]
**Principal Investigator:** [Name, Credentials, Affiliation]

---

## Table of Contents

1. [Study Synopsis](#1-study-synopsis)
2. [Background and Rationale](#2-background-and-rationale)
3. [Study Objectives](#3-study-objectives)
4. [Study Design](#4-study-design)
5. [Data Sources](#5-data-sources)
6. [Study Population](#6-study-population)
7. [Variables and Endpoints](#7-variables-and-endpoints)
8. [Statistical Analysis Plan](#8-statistical-analysis-plan)
9. [Data Quality and Validation](#9-data-quality-and-validation)
10. [Regulatory Considerations](#10-regulatory-considerations)
11. [References](#11-references)

---

## 1. Study Synopsis

### 1.1 Study Design
- **Design Type:** [e.g., Retrospective cohort, Prospective observational, External control arm]
- **Study Period:** [Start Date] to [End Date]
- **Follow-up Duration:** [Duration]

### 1.2 Study Objectives
**Primary Objective:**
[State primary objective clearly]

**Secondary Objectives:**
- [Objective 1]
- [Objective 2]

### 1.3 Study Population
**Inclusion Criteria:**
- [Criterion 1]
- [Criterion 2]

**Exclusion Criteria:**
- [Criterion 1]
- [Criterion 2]

**Target Sample Size:** [N patients]

### 1.4 Primary Endpoint
[Define primary endpoint with measurement details]

### 1.5 Statistical Analysis
**Primary Analysis:** [Brief description]
**Comparator:** [Description of control/comparator group]

---

## 2. Background and Rationale

### 2.1 Disease Background
[Describe the disease, unmet medical need, and current treatment landscape]

### 2.2 Treatment Under Study
[Describe the treatment/intervention being evaluated]

### 2.3 Rationale for Real-World Evidence
[Explain why RWD is appropriate for this study question]

### 2.4 Regulatory Context
[Reference relevant FDA guidance documents, e.g., FDA RWE Framework (December 2018)]

---

## 3. Study Objectives

### 3.1 Primary Objective
[Detailed statement of primary research question]

### 3.2 Secondary Objectives
1. [Secondary objective 1]
2. [Secondary objective 2]

### 3.3 Exploratory Objectives
1. [Exploratory objective 1]
2. [Exploratory objective 2]

---

## 4. Study Design

### 4.1 Overall Study Design
[Comprehensive description of study design including:]
- Study type (cohort, case-control, etc.)
- Temporal framework (prospective vs. retrospective)
- Comparison groups
- Index date definition

### 4.2 Study Schema
[Include flowchart or diagram showing study design]

```
[Patient Identification] → [Eligibility Assessment] → [Cohort Assembly]
                                                     ↓
                              [Baseline Characteristics] ← [Index Date]
                                                     ↓
                              [Treatment Exposure Assessment]
                                                     ↓
                              [Outcome Ascertainment] ← [Follow-up Period]
                                                     ↓
                              [Statistical Analysis]
```

### 4.3 Study Timeline
- **Patient Identification Period:** [Dates]
- **Index Date Period:** [Dates]
- **Follow-up Period:** [Duration]
- **Analysis Completion:** [Target Date]

---

## 5. Data Sources

### 5.1 Primary Data Source
**Name:** [Database/Registry Name]
**Description:** [Brief description of data source]
**Coverage:** [Geographic/population coverage]
**Time Period:** [Available data period]

### 5.2 Data Standards and Format
- **Data Model:** [e.g., OMOP CDM 5.4, FHIR, Custom]
- **Terminology Standards:** [e.g., SNOMED CT, ICD-10-CM, RxNorm, LOINC]
- **Data Format:** [e.g., CSV, Parquet, Database]

### 5.3 Data Linkage (if applicable)
[Describe any data linkage procedures and privacy protections]

### 5.4 Data Access and Governance
[Describe data access procedures, IRB approval, data use agreements]

---

## 6. Study Population

### 6.1 Source Population
[Define the source population from which study subjects will be identified]

### 6.2 Inclusion Criteria
1. [Criterion 1 with operational definition]
2. [Criterion 2 with operational definition]
3. [Additional criteria...]

### 6.3 Exclusion Criteria
1. [Criterion 1 with operational definition]
2. [Criterion 2 with operational definition]
3. [Additional criteria...]

### 6.4 Index Date Definition
[Clear definition of index date and rationale]

### 6.5 Treatment Groups
**Exposed/Treatment Group:**
[Definition and identification method]

**Unexposed/Control Group:**
[Definition and identification method]

### 6.6 Sample Size Justification
[Power calculation or precision estimation]

---

## 7. Variables and Endpoints

### 7.1 Primary Endpoint
**Definition:** [Precise operational definition]
**Measurement:** [How endpoint will be measured]
**Timing:** [When endpoint will be assessed]
**Data Source:** [Source of endpoint data]

### 7.2 Secondary Endpoints
1. **[Endpoint Name]**
   - Definition: [...]
   - Measurement: [...]
   - Timing: [...]

2. **[Endpoint Name]**
   - Definition: [...]
   - Measurement: [...]
   - Timing: [...]

### 7.3 Baseline Covariates
[List all baseline variables to be collected, organized by category:]

**Demographics:**
- Age, sex, race, ethnicity

**Clinical Characteristics:**
- Disease stage/severity
- Comorbidities (Charlson/Elixhauser)
- Laboratory values

**Treatment History:**
- Prior medications
- Prior procedures

**Healthcare Utilization:**
- Hospitalizations
- Emergency department visits

### 7.4 Time-Varying Covariates
[List any time-varying covariates and assessment schedule]

### 7.5 Safety Endpoints
[Define safety endpoints if applicable]

---

## 8. Statistical Analysis Plan

### 8.1 General Principles
- **Analysis Population:** [ITT, Per-protocol, As-treated]
- **Statistical Software:** R (rwevidence package v0.1.0)
- **Significance Level:** α = 0.05 (two-sided)
- **Confidence Intervals:** 95%

### 8.2 Handling of Missing Data
**Assessment of Missingness:**
- Quantify missingness by variable
- Assess missing data patterns (MCAR, MAR, MNAR)

**Imputation Strategy:**
- [Describe imputation method: MICE, Random Forest, etc.]
- Sensitivity analysis for primary endpoint

### 8.3 Propensity Score Analysis
**Purpose:** [Control for confounding]

**Variables in PS Model:**
[List all variables to be included in propensity score model]

**PS Estimation Method:**
- [Logistic regression, GBM, Random Forest]

**PS Application:**
- **Matching:** [1:1, 1:k, caliper width]
- **Weighting:** [IPTW, overlap weights]
- **Stratification:** [Number of strata]

**Balance Assessment:**
- Standardized mean differences (SMD) < 0.10
- Visual assessment (Love plots)

### 8.4 Primary Analysis
**Outcome Model:**
[Describe statistical model for primary endpoint]
- Model type: [Cox proportional hazards, logistic regression, etc.]
- Covariates: [List adjusting variables]
- Assumptions: [List model assumptions and verification plan]

**Effect Measure:**
[Hazard ratio, odds ratio, risk ratio, risk difference]

**Sensitivity Analyses:**
1. [Sensitivity analysis 1]
2. [Sensitivity analysis 2]

### 8.5 Secondary Analyses
[Describe analysis plan for each secondary endpoint]

### 8.6 Subgroup Analyses
**Pre-specified Subgroups:**
1. [Subgroup 1: e.g., Age (<65 vs ≥65)]
2. [Subgroup 2: e.g., Disease stage]

**Statistical Testing:**
- Interaction tests (α = 0.10)
- Multiple comparison adjustment if needed

### 8.7 Interim Analyses
[If applicable, describe timing and stopping rules]

---

## 9. Data Quality and Validation

### 9.1 Data Quality Assessment
**Completeness:**
- Assess missingness by variable
- Document data availability

**Validity:**
- Validate key variables against medical records (if feasible)
- Assess coding accuracy

**Consistency:**
- Check for logical inconsistencies
- Verify temporal relationships

### 9.2 Quality Control Procedures
- Data extraction validation
- Duplicate detection and resolution
- Outlier identification and handling

### 9.3 Audit Trail
All data transformations and analyses will be:
- Version controlled
- Fully documented
- Reproducible

---

## 10. Regulatory Considerations

### 10.1 Ethical Considerations
- IRB approval status
- Informed consent (if applicable)
- Patient privacy protections (HIPAA compliance)

### 10.2 Prespecification and Transparency
- Protocol registration: [ClinicalTrials.gov ID if applicable]
- Protocol deviations: [Documentation plan]

### 10.3 Alignment with FDA Guidance
This study follows principles outlined in:
- FDA Framework for Real-World Evidence (December 2018)
- FDA Guidance for Industry: Real-World Data
- [Other relevant guidance documents]

### 10.4 Study Limitations
[Acknowledge known limitations of RWD approach]

---

## 11. References

1. FDA. Framework for FDA's Real-World Evidence Program. December 2018.
2. FDA. Submitting Documents Using Real-World Data and Real-World Evidence to FDA for Drugs and Biologics. Guidance for Industry. May 2019.
3. [Additional references]

---

## Appendices

### Appendix A: Data Quality Assessment Framework (DQAF)
[Reference to DQAF implementation using rwevidence package]

### Appendix B: Variable Definitions
[Detailed operational definitions for all variables]

### Appendix C: Code Lists
[ICD-10, CPT, HCPCS, NDC codes for key variables]

### Appendix D: Statistical Analysis Code
[Reference to analysis scripts repository]

---

**Protocol Approval:**

Principal Investigator: _________________________ Date: _________

Statistician: _________________________ Date: _________

Sponsor Representative: _________________________ Date: _________

---

*This protocol template is generated using the rwevidence R package (v0.1.0)*
*For more information: https://github.com/singhg039/rwe_evidence_R_package*
