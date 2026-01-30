# Roadmap: rwevidence RWE Orchestration Platform

**Project:** rwevidence - RWE Orchestration Platform for Agentic AI
**Core Value:** Agentic AI can call a single, consistent API to orchestrate an entire RWE study pipeline
**Depth:** Comprehensive (8-12 phases)
**Created:** 2026-01-30

## Overview

This roadmap transforms rwevidence from a standalone RWE package into an AI-native orchestration platform. The strategy: delegate statistical implementation to best-in-class R packages (WeightIt, cobalt, MatchIt, admiral) while focusing on orchestration, regulatory compliance, and AI-agent-friendly APIs. Phases follow dependency chains - audit trail integrity and data quality validation establish the foundation, statistical delegation builds the core capability, workflow orchestration enables AI agent consumption, and regulatory integration delivers submission-ready outputs.

## Phases

### Phase 1: Audit Trail Foundation

**Goal:** Every operation is traceable through an immutable audit trail capturing all delegated package calls for 21 CFR Part 11 compliance.

**Dependencies:** None (foundation for all subsequent phases)

**Plans:** 3 plans in 2 waves

Plans:
- [ ] 01-01-PLAN.md - Core audit infrastructure (JSONL, hash chain, session management)
- [ ] 01-02-PLAN.md - Data lineage DAG (node management, trace queries, visualization)
- [ ] 01-03-PLAN.md - Delegated call wrapper (capture function, export verification)

**Requirements:**
- AUDIT-01: Audit trail captures all delegated package calls (WeightIt, cobalt, MatchIt, admiral)
- AUDIT-02: Each audit entry includes timestamp, function, package, version, parameters, duration
- AUDIT-03: Audit trail stored in immutable append-only format (21 CFR Part 11 compliance)
- AUDIT-04: Audit trail exportable to JSON and YAML for regulatory review
- AUDIT-05: Data lineage DAG tracks input → transformation → output relationships

**Success Criteria:**
1. Audit trail schema extended with delegated_to field capturing package name, version, function, and parameters
2. Every WeightIt, cobalt, MatchIt, and admiral call generates an audit entry with full parameter capture
3. Audit trail exports to JSON/YAML with complete provenance chain for regulatory review
4. Can reproduce any analysis result from 2 years ago using only the audit trail
5. Data lineage DAG visualizes complete input → transformation → output flow

### Phase 2: Data Quality Gateway

**Goal:** OMOP CDM data passes comprehensive quality validation before entering statistical analysis pipeline.

**Dependencies:** Phase 1 (audit trail must log validation results)

**Requirements:** None mapped (gap addressed - data quality is prerequisite to STAT-* requirements)

**Success Criteria:**
1. OMOP validation layer checks chronological integrity (birth < observation < death)
2. Referential integrity validated (all concept_ids exist in vocabulary)
3. Standard concept mapping verified (source concepts map to standard)
4. DataQualityDashboard integration runs 3300+ validation rules
5. Validation failures block analysis with actionable error messages showing which records failed

### Phase 3: WeightIt Propensity Score Integration

**Goal:** Propensity score estimation delegates to WeightIt with full method coverage and backwards compatibility.

**Dependencies:** Phase 1 (audit trail), Phase 2 (validated data)

**Requirements:**
- STAT-01: Propensity score estimation delegates to WeightIt package
- STAT-02: WeightIt integration supports all methods (logistic, GBM, CBPS, entropy balancing, super learner)
- STAT-07: Existing rwe_propensity_score() function wraps WeightIt with backwards compatibility
- STAT-08: Existing rwe_iptw() function uses WeightIt-generated scores

**Success Criteria:**
1. rwe_propensity_score() delegates to WeightIt for all estimation methods (logistic, GBM, CBPS, entropy balancing, super learner)
2. WeightIt adapter (weightit_adapter.R) translates rwevidence parameters to WeightIt API
3. Propensity score trimming uses WeightIt's native trim.weights parameter (no double-trimming)
4. All existing test cases pass with WeightIt backend (SMD values match within 0.01)
5. Backwards compatibility maintained - existing scripts work with deprecation warnings

### Phase 4: cobalt Balance Assessment Integration

**Goal:** Balance diagnostics delegate to cobalt for standardized metrics across all covariate types.

**Dependencies:** Phase 3 (cobalt assesses WeightIt output)

**Requirements:**
- STAT-03: Balance diagnostics delegate to cobalt package
- STAT-04: cobalt integration returns standardized metrics (SMD, variance ratios, KS statistics)

**Success Criteria:**
1. All balance assessment delegates to cobalt (replaces manual SMD calculation)
2. cobalt adapter (cobalt_adapter.R) normalizes results to rwe_balance_assessment S3 class
3. Balance metrics include SMD, variance ratios, and KS statistics for continuous/binary/categorical variables
4. Categorical variables and interactions handled correctly (pooled SD denominator)
5. Balance assessment results match manual calculations within 0.01 on test datasets

### Phase 5: MatchIt Matching Integration

**Goal:** Matching operations delegate to MatchIt for optimal, genetic, and specialized matching algorithms.

**Dependencies:** Phase 1 (audit trail), Phase 2 (validated data) - parallel to Phases 3-4

**Requirements:**
- STAT-05: Matching operations delegate to MatchIt package
- STAT-06: MatchIt integration supports nearest neighbor, optimal, genetic, CEM, exact matching
- STAT-09: New rwe_match() function exposes MatchIt capabilities

**Success Criteria:**
1. rwe_match() function delegates to MatchIt for all matching methods (nearest neighbor, optimal, genetic, CEM, exact)
2. MatchIt adapter (matchit_adapter.R) translates parameters and returns rwe_matched_cohort S3 class
3. Matching results 10-100x faster than manual implementations (Rcpp-optimized)
4. Caliper and ratio parameters mapped correctly to MatchIt conventions
5. Matched cohorts include match IDs, weights, and balance assessment

### Phase 6: Backwards Compatibility Layer

**Goal:** All existing rwevidence functions continue working with delegation backends and clear migration paths.

**Dependencies:** Phases 3, 4, 5 (all statistical delegation complete)

**Requirements:**
- COMPAT-01: All existing rwe_* functions continue to work with deprecation warnings
- COMPAT-02: Deprecation warnings include migration path to new API
- COMPAT-03: Existing tests pass with new implementation

**Success Criteria:**
1. v0.1.0 example scripts run in v0.2.0 with helpful deprecation warnings
2. Deprecation warnings include specific migration instructions (e.g., "truncate parameter renamed to trim, use trim=0.025")
3. Auto-migration handles common parameter changes (truncate → trim.weights)
4. All existing tests pass with delegated implementations (100% test suite coverage)
5. Migration vignette provides runnable before/after examples for each deprecated function

### Phase 7: Session Management & State Externalization

**Goal:** Multi-step RWE workflows persist across API calls via externalized session state.

**Dependencies:** Phases 3-6 (unified interface stable)

**Requirements:**
- WORK-03: Intermediate results (PS models, balance assessments) cached to filesystem
- WORK-04: Cache keys based on data hash + parameters for reproducibility

**Success Criteria:**
1. Session manager (create_session, save_session_data, load_session, delete_session) implemented
2. Filesystem backend stores sessions in tempdir with TTL expiration for development
3. Redis backend implemented for production with session replication
4. Intermediate results (propensity score models, balance assessments) cached with data hash + parameter keys
5. Sessions survive R process restarts (state externalized from memory)

### Phase 8: OpenAPI Specification & Structured Errors

**Goal:** REST API is self-describing with machine-readable contracts and actionable error messages for AI agents.

**Dependencies:** Phase 7 (session-aware endpoints defined)

**Requirements:**
- API-01: API exposes OpenAPI 3.0 specification at /openapi.json endpoint
- API-02: All endpoints return structured errors with error_code, message, type, and hint fields
- API-03: All endpoints versioned under /v1/ prefix

**Success Criteria:**
1. OpenAPI 3.0 specification auto-generated at /openapi.json with rich descriptions for AI agent reasoning
2. All endpoints under /v1/ prefix (versioned API)
3. Structured error responses include error_code, message, details, and suggested_fix fields
4. Error messages include examples ("Did you mean age_years not age?")
5. OpenAPI spec validates in Swagger UI without errors

### Phase 9: Async Job Management

**Goal:** Long-running RWE analyses execute asynchronously with job status polling and cancellation.

**Dependencies:** Phase 8 (API structure defined)

**Requirements:**
- API-04: Long-running operations return job_id with polling endpoint GET /v1/jobs/{id}
- API-05: Job status endpoint returns progress, status (queued/running/completed/failed), and result_url
- API-06: Async jobs support cancellation via DELETE /v1/jobs/{id}

**Success Criteria:**
1. Long-running endpoints (propensity score estimation, matching) return job_id immediately
2. GET /v1/jobs/{id} returns progress percentage, status (queued/running/completed/failed), and result_url
3. DELETE /v1/jobs/{id} cancels running jobs gracefully
4. Job queue persists across R process restarts (externalized state)
5. Agents can poll job status and retrieve results asynchronously

### Phase 10: Declarative Study Specifications

**Goal:** AI agents specify complete RWE studies via validated YAML/JSON instead of imperative API calls.

**Dependencies:** Phases 3-6 (statistical functions stable), Phase 7 (workflow orchestration)

**Requirements:**
- WORK-01: Study specification accepted as YAML with schema (treatment, outcome, covariates, method, options)
- WORK-02: Study spec YAML validated against JSON Schema before execution
- WORK-05: Webhook endpoint registration for job completion notifications
- WORK-06: Webhooks POST to registered URL with job_id, status, and result summary

**Success Criteria:**
1. Study spec YAML defines treatment, outcome, covariates, method, and options in single document
2. JSON Schema validates study specs before execution (catch errors early)
3. Single POST /v1/studies endpoint accepts YAML and orchestrates full pipeline
4. Webhook registration (POST /v1/webhooks) enables job completion notifications
5. Webhooks POST to registered URLs with job_id, status, and result summary when jobs complete

### Phase 11: admiral ADaM Integration

**Goal:** Regulatory-ready ADaM datasets (ADSL, ADAE, ADTTE) generated from OMOP CDM via admiral.

**Dependencies:** Phase 2 (OMOP validation), Phases 3-5 (statistical analysis complete)

**Requirements:**
- REG-01: ADaM ADSL (Subject-Level Analysis Dataset) generation via admiral
- REG-02: ADaM ADAE (Adverse Events Analysis Dataset) generation via admiral
- REG-03: ADaM ADTTE (Time-to-Event Analysis Dataset) generation via admiral
- REG-04: admiral integration accepts OMOP-harmonized data as input
- REG-05: ADaM datasets include required CDISC metadata and validation

**Success Criteria:**
1. OMOP-to-SDTM transformation layer prepares data for admiral (bridge pattern)
2. ADSL (subject-level) generation from OMOP in <50 lines of code via admiral
3. ADAE (adverse events) generation with MedDRA coding from OMOP condition_occurrence
4. ADTTE (time-to-event) generation with censoring indicators from OMOP observation_period
5. ADaM datasets validate against CDISC rules and export to XPT format for FDA submission

### Phase 12: Production Readiness & Monitoring

**Goal:** Platform runs in production with authentication, monitoring, and horizontal scaling.

**Dependencies:** Phases 8-10 (API complete), Phase 7 (Redis session management)

**Requirements:** None mapped (infrastructure, not v1 feature requirements)

**Success Criteria:**
1. API key authentication enforced on all endpoints (plumber security features)
2. Request/response logging captures all API calls for troubleshooting
3. Prometheus metrics endpoint (/metrics) exposes latency, throughput, and error rates
4. Docker containerization with multi-stage builds for minimal image size
5. Kubernetes manifests enable horizontal scaling (multiple pods share Redis state)

## Progress

| Phase | Status | Requirements | Completion |
|-------|--------|--------------|------------|
| Phase 1: Audit Trail Foundation | Planned | 5 requirements | 0% |
| Phase 2: Data Quality Gateway | Pending | 0 requirements (prerequisite) | 0% |
| Phase 3: WeightIt Propensity Score Integration | Pending | 4 requirements | 0% |
| Phase 4: cobalt Balance Assessment Integration | Pending | 2 requirements | 0% |
| Phase 5: MatchIt Matching Integration | Pending | 3 requirements | 0% |
| Phase 6: Backwards Compatibility Layer | Pending | 3 requirements | 0% |
| Phase 7: Session Management & State Externalization | Pending | 2 requirements | 0% |
| Phase 8: OpenAPI Specification & Structured Errors | Pending | 3 requirements | 0% |
| Phase 9: Async Job Management | Pending | 3 requirements | 0% |
| Phase 10: Declarative Study Specifications | Pending | 4 requirements | 0% |
| Phase 11: admiral ADaM Integration | Pending | 5 requirements | 0% |
| Phase 12: Production Readiness & Monitoring | Pending | 0 requirements (infrastructure) | 0% |

**Overall Progress:** 0/12 phases complete (0%)

## Notes

### Coverage Analysis

Total v1 requirements: 34
- AUDIT-*: 5 requirements → Phase 1
- STAT-01, STAT-02, STAT-07, STAT-08: 4 requirements → Phase 3
- STAT-03, STAT-04: 2 requirements → Phase 4
- STAT-05, STAT-06, STAT-09: 3 requirements → Phase 5
- COMPAT-*: 3 requirements → Phase 6
- WORK-03, WORK-04: 2 requirements → Phase 7
- API-01, API-02, API-03: 3 requirements → Phase 8
- API-04, API-05, API-06: 3 requirements → Phase 9
- WORK-01, WORK-02, WORK-05, WORK-06: 4 requirements → Phase 10
- REG-*: 5 requirements → Phase 11

**Coverage: 34/34 requirements mapped (100%)**

### Phase Dependencies

```
Phase 1 (Audit Trail) ──┬──> Phase 2 (Data Quality)
                        │
                        ├──> Phase 3 (WeightIt) ──> Phase 4 (cobalt) ──┐
                        │                                               │
                        ├──> Phase 5 (MatchIt) ────────────────────────┤
                        │                                               │
                        └─────────────────────────────────────────> Phase 6 (Compatibility) ──> Phase 7 (Session Mgmt) ──> Phase 8 (OpenAPI) ──> Phase 9 (Async Jobs) ──> Phase 10 (Declarative Specs)
                                                                        │                                                                                                   │
Phase 2 (Data Quality) ─────────────────────────────────────────────────┴──> Phase 11 (admiral ADaM) <──────────────────────────────────────────────────────────────────┘

Phase 7 (Session Mgmt) ──> Phase 12 (Production)
Phase 8-10 (API Complete) ──> Phase 12 (Production)
```

### Research Flags

**Phase 3 (WeightIt Integration):** Needs deep dive on parameter mapping (truncate/caliper/trim) to avoid double-trimming pitfall. Research WeightIt's trim.weights conventions.

**Phase 4 (cobalt Integration):** Needs research on cobalt conventions for categorical variables (pooled SD denominator, rare category handling).

**Phase 11 (admiral Integration):** Needs validation of OMOP-to-SDTM transformation patterns. admiral expects SDTM structure, not raw OMOP normalized tables.

### Depth Calibration

Comprehensive depth (8-12 phases) applied. Natural boundaries identified:
- Phases 1-2: Foundation (audit + data quality)
- Phases 3-5: Statistical delegation (WeightIt + cobalt + MatchIt)
- Phase 6: Compatibility layer (stabilization)
- Phases 7-10: Orchestration & API (workflow + async + declarative)
- Phase 11: Regulatory output (admiral ADaM)
- Phase 12: Infrastructure (production readiness)

Avoided artificial compression - each phase delivers one coherent, verifiable capability.

---
*Last updated: 2026-01-30*
*Phase 1 planned: 3 plans in 2 waves*
*Next: `/gsd:execute-phase 1` to begin Phase 1 execution*
