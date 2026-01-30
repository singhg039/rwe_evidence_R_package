# Requirements: rwevidence RWE Orchestration Platform

**Defined:** 2026-01-30
**Core Value:** Agentic AI can call a single, consistent API to orchestrate an entire RWE study pipeline

## v1 Requirements

Requirements for initial release. Each maps to roadmap phases.

### API & AI Consumption

- [ ] **API-01**: API exposes OpenAPI 3.0 specification at /openapi.json endpoint
- [ ] **API-02**: All endpoints return structured errors with error_code, message, type, and hint fields
- [ ] **API-03**: All endpoints versioned under /v1/ prefix
- [ ] **API-04**: Long-running operations return job_id with polling endpoint GET /v1/jobs/{id}
- [ ] **API-05**: Job status endpoint returns progress, status (queued/running/completed/failed), and result_url
- [ ] **API-06**: Async jobs support cancellation via DELETE /v1/jobs/{id}

### Statistical Orchestration (Package Integration)

- [ ] **STAT-01**: Propensity score estimation delegates to WeightIt package
- [ ] **STAT-02**: WeightIt integration supports all methods: logistic, GBM, CBPS, entropy balancing, super learner
- [ ] **STAT-03**: Balance diagnostics delegate to cobalt package
- [ ] **STAT-04**: cobalt integration returns standardized metrics: SMD, variance ratios, KS statistics
- [ ] **STAT-05**: Matching operations delegate to MatchIt package
- [ ] **STAT-06**: MatchIt integration supports: nearest neighbor, optimal, genetic, CEM, exact matching
- [ ] **STAT-07**: Existing rwe_propensity_score() function wraps WeightIt with backwards compatibility
- [ ] **STAT-08**: Existing rwe_iptw() function uses WeightIt-generated scores
- [ ] **STAT-09**: New rwe_match() function exposes MatchIt capabilities

### Regulatory Integration (admiral)

- [ ] **REG-01**: ADaM ADSL (Subject-Level Analysis Dataset) generation via admiral
- [ ] **REG-02**: ADaM ADAE (Adverse Events Analysis Dataset) generation via admiral
- [ ] **REG-03**: ADaM ADTTE (Time-to-Event Analysis Dataset) generation via admiral
- [ ] **REG-04**: admiral integration accepts OMOP-harmonized data as input
- [ ] **REG-05**: ADaM datasets include required CDISC metadata and validation

### Workflow & State Management

- [ ] **WORK-01**: Study specification accepted as YAML with schema: treatment, outcome, covariates, method, options
- [ ] **WORK-02**: Study spec YAML validated against JSON Schema before execution
- [ ] **WORK-03**: Intermediate results (PS models, balance assessments) cached to filesystem
- [ ] **WORK-04**: Cache keys based on data hash + parameters for reproducibility
- [ ] **WORK-05**: Webhook endpoint registration for job completion notifications
- [ ] **WORK-06**: Webhooks POST to registered URL with job_id, status, and result summary

### Audit Trail Enhancement

- [ ] **AUDIT-01**: Audit trail captures all delegated package calls (WeightIt, cobalt, MatchIt, admiral)
- [ ] **AUDIT-02**: Each audit entry includes: timestamp, function, package, version, parameters, duration
- [ ] **AUDIT-03**: Audit trail stored in immutable append-only format (21 CFR Part 11 compliance)
- [ ] **AUDIT-04**: Audit trail exportable to JSON and YAML for regulatory review
- [ ] **AUDIT-05**: Data lineage DAG tracks input → transformation → output relationships

### Backwards Compatibility

- [ ] **COMPAT-01**: All existing rwe_* functions continue to work with deprecation warnings
- [ ] **COMPAT-02**: Deprecation warnings include migration path to new API
- [ ] **COMPAT-03**: Existing tests pass with new implementation

## v2 Requirements

Deferred to future release. Tracked but not in current roadmap.

### Authentication & Security

- **AUTH-01**: API key authentication for all endpoints
- **AUTH-02**: OAuth 2.0 support for enterprise integrations
- **AUTH-03**: Rate limiting per API key

### Advanced AI Features

- **AI-01**: MCP (Model Context Protocol) support for Claude auto-discovery
- **AI-02**: Natural language study specification parser
- **AI-03**: Confidence scores on all statistical outputs
- **AI-04**: Cost estimation before execution

### Advanced Orchestration

- **ORCH-01**: Multi-study parallel execution for sensitivity analyses
- **ORCH-02**: Federated analysis across multiple data sources
- **ORCH-03**: Workflow checkpoint and resume from failure

### Production Readiness

- **PROD-01**: Docker containerization
- **PROD-02**: Kubernetes deployment manifests
- **PROD-03**: Prometheus metrics endpoint
- **PROD-04**: Health check endpoints

## Out of Scope

Explicitly excluded. Documented to prevent scope creep.

| Feature | Reason |
|---------|--------|
| Custom propensity score algorithms | WeightIt implements 10+ validated methods; delegate instead of reimplement |
| Custom balance diagnostic plots | cobalt provides standardized SMD/Love plots; delegate instead of reimplement |
| Custom ADaM generation logic | admiral is pharma industry standard with 100+ contributors |
| Custom matching algorithms | MatchIt implements optimal, genetic, CEM matching |
| Interactive dashboards/GUI | CLI + API focus for AI agents; humans can use Shiny separately |
| Real-time data streaming | RWE studies are batch analyses |
| Multi-tenancy | Defer to v2; single-tenant sufficient for MVP |
| Custom OMOP vocabulary server | Use OHDSI Athena for lookups |
| Data de-identification | Require pre-de-identified data; complex regulatory minefield |
| Causal discovery (DAG generation) | Research-grade, not production-ready |

## Traceability

Which phases cover which requirements. Updated during roadmap creation.

| Requirement | Phase | Status |
|-------------|-------|--------|
| AUDIT-01 | Phase 1 | Pending |
| AUDIT-02 | Phase 1 | Pending |
| AUDIT-03 | Phase 1 | Pending |
| AUDIT-04 | Phase 1 | Pending |
| AUDIT-05 | Phase 1 | Pending |
| STAT-01 | Phase 2 | Pending |
| STAT-02 | Phase 2 | Pending |
| STAT-03 | Phase 2 | Pending |
| STAT-04 | Phase 2 | Pending |
| STAT-05 | Phase 2 | Pending |
| STAT-06 | Phase 2 | Pending |
| STAT-07 | Phase 2 | Pending |
| STAT-08 | Phase 2 | Pending |
| STAT-09 | Phase 2 | Pending |
| COMPAT-01 | Phase 2 | Pending |
| COMPAT-02 | Phase 2 | Pending |
| COMPAT-03 | Phase 2 | Pending |
| API-01 | Phase 3 | Pending |
| API-02 | Phase 3 | Pending |
| API-03 | Phase 3 | Pending |
| API-04 | Phase 3 | Pending |
| API-05 | Phase 3 | Pending |
| API-06 | Phase 3 | Pending |
| WORK-01 | Phase 3 | Pending |
| WORK-02 | Phase 3 | Pending |
| WORK-03 | Phase 3 | Pending |
| WORK-04 | Phase 3 | Pending |
| WORK-05 | Phase 3 | Pending |
| WORK-06 | Phase 3 | Pending |
| REG-01 | Phase 4 | Pending |
| REG-02 | Phase 4 | Pending |
| REG-03 | Phase 4 | Pending |
| REG-04 | Phase 4 | Pending |
| REG-05 | Phase 4 | Pending |

**Coverage:**
- v1 requirements: 33 total
- Mapped to phases: 33
- Unmapped: 0 ✓

---
*Requirements defined: 2026-01-30*
*Last updated: 2026-01-30 after initial definition*
