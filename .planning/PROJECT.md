# rwevidence - RWE Orchestration Platform

## What This Is

An R package that serves as a unified orchestration layer for Real-World Evidence (RWE) generation, integrating best-in-class packages (WeightIt, cobalt, admiral, MatchIt, OHDSI tools) into a cohesive workflow. Designed specifically for Agentic AI systems to programmatically generate complete RWE studies from raw healthcare data to regulatory-ready reports.

## Core Value

**Agentic AI can call a single, consistent API to orchestrate an entire RWE study pipeline - from data ingestion through regulatory reporting - without needing to understand the intricacies of 20+ specialized R packages.**

## Requirements

### Validated

<!-- Existing capabilities from current codebase -->

- Data ingestion with format detection (OMOP CDM, FHIR, CSV, databases) — existing
- Data quality assessment across 5 dimensions — existing
- Schema harmonization to OMOP CDM — existing
- Propensity score estimation (logistic, GBM, RF, XGBoost) — existing
- IPTW with stabilized weights — existing
- Doubly robust estimation — existing
- Survival analysis (Cox regression, Kaplan-Meier) — existing
- Safety surveillance and adverse event analysis — existing
- Regulatory report generation — existing
- REST API exposure via Plumber — existing
- Comprehensive audit trails — existing

### Active

<!-- New orchestration layer requirements -->

- [ ] Unified orchestration API for Agentic AI
- [ ] Deep integration with WeightIt for propensity weighting
- [ ] Deep integration with cobalt for balance diagnostics
- [ ] Deep integration with admiral for ADaM dataset generation
- [ ] Deep integration with MatchIt for matching methods
- [ ] OHDSI CohortMethod integration for standardized workflows
- [ ] Machine-readable output schemas for AI consumption
- [ ] Workflow state management for long-running studies
- [ ] Declarative study specification (YAML/JSON)

### Out of Scope

- Building our own propensity score algorithms — use WeightIt instead
- Building our own balance diagnostics — use cobalt instead
- Building our own ADaM generation — use admiral instead
- Building our own matching algorithms — use MatchIt instead
- Real-time data streaming — batch processing focus
- Interactive dashboards — CLI/API focus for AI agents

## Context

**Existing Codebase State:**
- Package has working implementations of core RWE functions
- Some integration with WeightIt/cobalt/MatchIt already present in DESCRIPTION
- Custom implementations duplicate what these packages do better
- CRAN submission fixes completed (PS trimming, weight capping, convergence handling)
- All tests passing

**The Opportunity:**
- Current R ecosystem has fragmented best-in-class tools
- No single package orchestrates the full RWE workflow for AI consumption
- rwevidence's unique value: audit trails, workflow orchestration, API exposure
- Removing duplicate implementations reduces maintenance burden

**Target Users:**
- AI/LLM agents generating RWE studies programmatically
- Pharmaceutical data scientists running standardized studies
- Regulatory submission teams needing audit trails

## Constraints

- **Tech stack**: R package, must maintain CRAN compatibility
- **Dependencies**: Integrate (not fork) existing packages - WeightIt, cobalt, admiral, MatchIt
- **Backwards compatibility**: Existing API must continue working (deprecation warnings ok)
- **Regulatory**: Must maintain full audit trails for FDA/EMA submissions
- **Performance**: Must handle typical RWE study sizes (10K-1M patients)

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Orchestration over implementation | Best-in-class packages already exist; we add value through integration | — Pending |
| Agentic AI as primary user | Unique positioning; humans can use underlying packages directly | — Pending |
| Declarative study specs | AI agents work better with structured input/output | — Pending |
| Keep audit trail system | Unique value for regulatory; no equivalent in other packages | — Pending |
| Keep REST API | Critical for AI agent integration | — Pending |

---
*Last updated: 2026-01-30 after project initialization*
