# rwevidence

<!-- badges: start -->
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)](https://github.com/singhg039/rwe_evidence_R_package/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**rwevidence** is an R package that provides end-to-end workflows for generating Real-World Evidence (RWE) from Real-World Data (RWD) sources for pharmaceutical research and regulatory submissions.

## Overview

The package bridges the gap between Real-World Data sources and regulatory-grade Real-World Evidence, providing validated, reproducible workflows following FDA and EMA guidance.

### Key Features

- **Data Ingestion** – Read and validate EHR, claims, registry, and wearables data from multiple sources
- **Data Quality Assessment** – Comprehensive quality evaluation across 5 dimensions (completeness, validity, consistency, timeliness, uniqueness)
- **Harmonization** – Schema mapping and terminology standardization (OMOP CDM, FHIR, SNOMED, RxNorm)
- **AI/ML Features** – Machine learning imputation (Random Forest, XGBoost), propensity scoring, and causal inference
- **Study Design** – External control arms, propensity score matching, IPTW weighting
- **Survival Analysis** – Kaplan-Meier curves, Cox regression, log-rank tests, IPTW-weighted survival
- **Effectiveness Analysis** – Relative risk, odds ratios, NNT/NNH, subgroup analysis with forest plots, sensitivity analysis
- **Safety Surveillance** – Signal detection, disproportionality analysis, adverse event monitoring
- **Regulatory Reporting** – Table 1, CONSORT diagrams, safety reports, automated HTML export

## Installation

You can install the development version of rwevidence from [GitHub](https://github.com/singhg039/rwe_evidence_R_package) with:

``` r
# install.packages("devtools")
devtools::install_github("singhg039/rwe_evidence_R_package")
```

## Quick Start

``` r
library(rwevidence)

# 1. Read EHR data
ehr_data <- rwe_read_ehr(
  source = "data/patient_data.parquet",
  format = "omop_cdm",
  version = "5.4"
)

# 2. Assess data quality
quality <- rwe_assess_quality(ehr_data)
print(quality)

# 3. Harmonize data
harmonized <- rwe_harmonize(
  data = ehr_data,
  target_schema = "omop_cdm_5.4"
)

# 4. Impute missing values
imputed <- rwe_impute(
  data = harmonized,
  method = "random_forest"
)

# 5. Generate external control arm
control <- rwe_external_control(
  target_trial = trial_criteria,
  rwd_source = imputed,
  matching_vars = c("age", "stage", "biomarkers"),
  method = "synthetic"
)

# 6. Analyze effectiveness and safety
rr <- rwe_relative_risk(
  data = control,
  outcome = "progression_free_survival_event",
  treatment = "new_drug"
)

km <- rwe_kaplan_meier(
  data = control,
  time_var = "progression_free_survival",
  event_var = "progression_free_survival_event",
  group_var = "new_drug"
)

# safety_records: adverse event dataset (one row per subject/period)
safety_analysis <- rwe_analyze_safety(
  data = safety_records,
  events = "ae_count",
  person_time = "person_years",
  treatment = "new_drug",
  time_var = "visit_day"
)

subgroup <- rwe_subgroup_analysis(
  data = control,
  outcome = "progression_free_survival_event",
  treatment = "new_drug",
  subgroup_vars = c("sex", "region")
)

consort <- rwe_consort_diagram(
  screened = 1200,
  enrolled = 950,
  excluded = list("Not eligible" = 180, "Declined" = 70),
  allocated = list("Drug" = 475, "Control" = 475),
  analyzed = list("Drug" = 430, "Control" = 445)
)

# 7. Assemble automated regulatory report
reg_report <- rwe_generate_regulatory_report(
  study_title = "Phase III Oncology Study",
  cohort_data = control,
  treatment_var = "new_drug",
  baseline_vars = c("age", "sex", "region", "baseline_stage"),
  effectiveness = rr,
  survival = km,
  subgroup = subgroup,
  safety_analysis = safety_analysis,
  safety_report = rwe_safety_report(
    data = safety_records,
    group_var = "treatment_group",
    ae_var = "adverse_event",
    severity_var = "severity",
    serious_var = "serious"
  ),
  consort = consort
)

# 8. Export HTML bundle
export_regulatory_report(reg_report, "phase-iii-report.html")
```

## Package Architecture

The package is organized into modular components:

```
R/
├── data-ingestion/     # Data readers for EHR, claims, registries
├── data-quality/       # Quality assessment and validation
├── harmonization/      # Schema and terminology mapping
├── ai-features/        # ML imputation and propensity scoring
├── study-design/       # External control arms and study templates
├── analysis/           # Statistical analysis methods
├── reporting/          # Regulatory reports and tables
└── utils/              # Helper functions and constants
```

## Regulatory Compliance

**rwevidence** aligns with:

- FDA's Real-World Evidence Framework (December 2018)
- EMA guidance on registry-based studies
- ICH E6 Good Clinical Practice
- Data Quality Assessment Framework (DQAF)

All functions include:
- Audit trails for transformations
- Reproducible analysis pipelines
- Validation against standard schemas
- Regulatory-compliant reporting

## Target Users

- Clinical data scientists
- Biostatisticians
- Regulatory affairs professionals
- Medical affairs teams
- Health economics researchers

## Development Status

This package is under active development. The current version (0.1.0) includes:

- ✅ Package infrastructure, utilities, and logging/audit trails
- ✅ Data ingestion and quality assessment modules
- ✅ Harmonization scaffolding and terminology mapping
- ✅ AI/ML utilities (imputation, propensity scoring, causal inference)
- ✅ Study design workflows (external controls, survival, subgroup analyses)
- ✅ Reporting ecosystem (tables, figures, automated HTML export)

## Documentation

- [Getting Started Guide](https://singhg039.github.io/rwe_evidence_R_package/articles/getting-started.html)
- [CLAUDE.md](https://github.com/singhg039/rwe_evidence_R_package/blob/main/CLAUDE.md) - Development guide for Claude Code
- [Package Website](https://singhg039.github.io/rwe_evidence_R_package)

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## Citation

If you use rwevidence in your research, please cite:

``` r
citation("rwevidence")
```

## License

MIT License - see [LICENSE](LICENSE.md) for details.

## Support

- 📝 [File an issue](https://github.com/singhg039/rwe_evidence_R_package/issues)
- 💬 [Discussions](https://github.com/singhg039/rwe_evidence_R_package/discussions)

---

**Disclaimer**: This package is designed to assist with RWE generation but does not replace the need for expert statistical and regulatory review. Users are responsible for ensuring compliance with applicable regulations.
