# Feature Landscape: RWE Orchestration Platform for Agentic AI

**Domain:** Real-World Evidence Orchestration for AI Agent Consumption
**Researched:** 2026-01-30
**Current Package State:** Existing rwevidence v0.1.0 with core RWE features implemented

## Executive Summary

RWE orchestration platforms for AI agents require two distinct feature categories: (1) **AI Consumption Layer** features that make the platform agent-friendly (structured I/O, machine-readable schemas, error handling, workflow state), and (2) **Domain Orchestration** features that integrate best-in-class RWE packages into cohesive workflows. The unique value proposition is not implementing statistical methods (those exist in WeightIt, cobalt, admiral, MatchIt), but rather providing the orchestration, audit trails, and API exposure that AI agents need to conduct end-to-end RWE studies programmatically.

Key insight from research: Most RWE platforms focus on human data scientists. **No platform currently optimizes for AI agent consumption.** This is rwevidence's differentiator.

---

## Table Stakes

Features users (AI agents and humans) expect. Missing these = product feels incomplete or unusable.

### Category 1: API & Structured I/O (AI Agent Consumption)

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| **RESTful API with OpenAPI/Swagger spec** | AI agents need machine-readable API definitions for discovery | Medium | Existing: Basic Plumber API. Need: OpenAPI 3.0 spec generation |
| **Structured JSON input/output** | LLMs require consistent, parseable data contracts; inconsistent JSON breaks agent workflows | Low | Existing: API returns JSON. Need: Formal schemas with validation |
| **HTTP status codes + structured errors** | Agents need to distinguish error types (400 vs 404 vs 500) and parse error details programmatically | Low | Existing: Basic status codes. Need: Consistent error_code, message, type fields |
| **Machine-readable study specification (YAML/JSON)** | Declarative study specs allow agents to define entire workflows without procedural code | High | Missing: Critical for agent orchestration. Industry lacks standards here |
| **Synchronous + async operation modes** | Long-running analyses (Cox models, PS matching) timeout in sync mode; agents need job IDs and polling | Medium | Missing: All operations are synchronous. Need job queue system |
| **Versioned API endpoints** | Breaking changes destroy agent integrations; semantic versioning maintains compatibility | Medium | Missing: No versioning. Critical before public release |
| **Authentication & authorization** | Regulatory requirements (21 CFR Part 11) and PHI protection mandate access controls | Medium | Missing: No auth in API. Table stakes for production |
| **Rate limiting & quotas** | Prevents abuse; allows fair resource allocation across multiple agents | Low | Missing: Unbounded API usage currently |

### Category 2: Data Orchestration (Core RWE Workflow)

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| **Multi-format data ingestion** | RWD comes in OMOP CDM, FHIR, CSV, databases; must handle all | Medium | Existing: CSV, OMOP detection. Need: FHIR parsing, DB connectors |
| **Data quality assessment** | FDA/EMA require quality documentation; agents need automated quality checks | Medium | Existing: 5-dimension quality assessment implemented |
| **Schema harmonization to OMOP CDM** | OMOP is standard for observational research; enables interoperability | High | Existing: Basic harmonization. Need: Full OMOP vocabulary integration |
| **Propensity score estimation** | Core method for causal inference in RWE; industry standard | High | Existing: Custom implementation. Need: Delegate to WeightIt (more methods, better maintained) |
| **Balance diagnostics** | Required to validate PS matching/weighting quality | Medium | Existing: Basic balance. Need: Delegate to cobalt (standardized plots, SMD, KS stats) |
| **Survival analysis (Cox, KM)** | Primary endpoint in oncology, cardiology RWE studies | High | Existing: Cox regression implemented via survival package |
| **Adverse event analysis** | Safety endpoints are regulatory requirement for all RWE submissions | Medium | Existing: Safety surveillance implemented |
| **Regulatory report generation** | Output must match FDA/EMA submission formats (Word, PDF with tables) | High | Existing: Templates for FDA/EMA reports |
| **Audit trails (21 CFR Part 11)** | Regulatory mandate: time-stamped, attributable, secure logs of all operations | High | Existing: Logging system. Need: Non-editable storage, routine review triggers |

### Category 3: Workflow State Management

| Feature | Why Expected | Complexity | Notes |
|---------|--------------|------------|-------|
| **Reproducible analysis pipelines** | Regulatory submissions require exact reproducibility; agents need seed control | Medium | Existing: Seed parameters in functions. Need: Global study-level seed management |
| **Intermediate result caching** | Re-running expensive PS estimation on API retry wastes resources | Medium | Missing: Critical for async workflows |
| **Workflow checkpoint/resume** | Long studies (days to run) need recovery from failures | High | Missing: No state persistence currently |
| **Provenance tracking** | Must document data → method → result lineage for regulatory defense | High | Existing: Audit trails. Need: Explicit data lineage DAG |

---

## Differentiators

Features that set this platform apart from competitors. Not expected, but create competitive moat.

### Category 1: AI-First Design

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| **Self-describing API (MCP protocol support)** | Anthropic's Model Context Protocol allows Claude and other agents to auto-discover capabilities without hardcoded integration | High | Emerging standard (2026). Early adoption = first-mover advantage |
| **Natural language study specification parser** | Agent sends "Compare treatment A vs B in elderly patients using PS matching" → system generates formal spec | Very High | Requires LLM integration. Killer feature for agent UX |
| **Confidence scores on all outputs** | Agents need to know when results are uncertain (small N, poor balance, convergence warnings) | Medium | Statistical models provide this; expose systematically |
| **Automatic covariate selection** | Agents struggle to choose confounders; ML-based selection reduces this cognitive load | High | Use domain knowledge graphs or LASSO for variable selection |
| **Domain-specific error recovery suggestions** | Instead of "PS model failed to converge" → "Consider: (1) trim extreme propensities, (2) reduce covariates, (3) try GBM method" | Medium | Codify expert knowledge into error handlers |
| **Cost estimation before execution** | Agent queries: "How much compute for 100K patient Cox model?" System responds with time/cost | Low | Profile operations, build cost model |

### Category 2: Orchestration Intelligence

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| **Package integration layer (not reimplementation)** | Delegates to WeightIt (PS), cobalt (balance), admiral (ADaM), MatchIt (matching) → always current with ecosystem | High | Core architecture decision. Reduces maintenance burden |
| **Automated workflow validation** | Checks study spec for logical errors before execution: "You specified Cox model but no time-to-event variable" | Medium | Rule-based validation engine |
| **Smart defaults with override** | Agents benefit from expert defaults (caliper=0.1*SD, trim at 95th percentile) but can override | Low | Document defaults clearly in API |
| **Multi-study orchestration** | Run multiple study designs in parallel (PS matching + IPTW + doubly robust) for sensitivity analysis | Medium | Regulatory reviewers expect sensitivity analyses |
| **Federated analysis support** | Execute study across multiple data sources without centralizing data (privacy-preserving) | Very High | Emerging requirement for multi-site RWE |

### Category 3: Regulatory Excellence

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| **Built-in OHDSI CohortMethod integration** | OHDSI is gold standard for observational research; integration provides credibility | High | Already in DESCRIPTION suggests. Need: Seamless workflow handoff |
| **Admiral ADaM dataset generation** | ADaM is mandatory for FDA NDA/BLA submissions; automated generation saves weeks | High | Admiral 1.4 released Jan 2026 with AI integration. Partner here |
| **Automated ICH E9(R1) estimand specification** | New regulatory guidance requires explicit estimand definition; most platforms ignore this | Medium | Structured estimand templates (population, endpoint, treatment strategy, summary measure) |
| **Interactive balance optimization** | Agent iteratively adjusts matching parameters until balance thresholds met (SMD < 0.1) | Medium | Use cobalt's bal.compute() for programmatic balance tuning |
| **Regulatory submission package export** | One-click export of study protocol, SAP, analysis code, results, audit trails in FDA eCTD format | Very High | Ultimate goal: agent generates entire submission package |

### Category 4: Developer Experience for AI Agents

| Feature | Value Proposition | Complexity | Notes |
|---------|-------------------|------------|-------|
| **SDK generation for multiple languages** | OpenAPI spec → auto-generate Python/R/Julia SDKs for agent integration | Low | Use openapi-generator tooling |
| **Comprehensive example gallery** | Pre-built study specs for common use cases (RCT emulation, target trial, external control arm) | Low | Documentation as differentiator |
| **Sandbox environment** | Agents can test with synthetic data before running on real PHI | Low | Generate synthetic OMOP data for testing |
| **Detailed operation logs** | Every API call logged with inputs/outputs for debugging agent behavior | Low | Extend existing logging system |
| **Webhook notifications** | Agent subscribes to study completion events instead of polling | Medium | Modern pattern: "stop polling, start subscribing" (2026 best practice) |

---

## Anti-Features

Features to explicitly NOT build. Common mistakes in this domain.

| Anti-Feature | Why Avoid | What to Do Instead |
|--------------|-----------|-------------------|
| **Custom propensity score algorithms** | WeightIt already implements 10+ methods (logistic, GBM, CBPS, entropy balancing). Reimplementing creates maintenance burden and bugs | Integrate WeightIt via thin wrapper. Focus on orchestration, not implementation |
| **Custom balance diagnostic plots** | cobalt provides standardized SMD plots, Love plots, density plots. Inconsistent visuals confuse reviewers | Use cobalt's plotting functions. Ensure API can return plot data + rendered images |
| **Custom ADaM generation logic** | admiral is pharma industry standard with 100+ contributors. Custom logic won't match breadth/correctness | Integrate admiral templates. Provide convenience wrappers for common ADaM datasets (ADSL, ADAE, ADTTE) |
| **Custom matching algorithms** | MatchIt implements optimal, genetic, CEM, coarsened exact matching. Don't reinvent | Delegate to MatchIt. Expose configuration options via API |
| **Interactive dashboards/GUI** | Scope creep for agent-first platform. Humans can use Shiny/Streamlit separately | CLI + API only. External tools can build UIs on top of API |
| **Real-time data streaming** | RWE studies are batch analyses. Streaming adds complexity without value | Focus on batch workflows with async job handling |
| **Multi-tenancy at MVP** | Premature optimization. Single-tenant deployment is sufficient for early adopters | Defer to post-MVP. Authentication + resource quotas are sufficient initially |
| **Custom OMOP vocabulary server** | OHDSI Athena already provides this. Don't duplicate infrastructure | Use Athena for vocabulary lookups. Cache frequently used concepts locally |
| **Built-in data de-identification** | Complex regulatory minefield (HIPAA Safe Harbor, Expert Determination). External tools do this better | Require pre-de-identified data. Provide validation checks for common PHI patterns |
| **Causal discovery algorithms** | Automated DAG generation from data is research-grade, not production-ready. High false positive rate | Require users/agents to specify DAG explicitly. Provide DAG validation tools |

---

## Feature Dependencies

Critical dependencies that determine implementation order:

```
FOUNDATIONAL LAYER (Phase 1):
├── API versioning → All features
├── Structured error handling → All features
├── OpenAPI spec generation → SDK generation, MCP support
└── Authentication/authorization → Production deployment

DATA INGESTION LAYER (Phase 2):
├── OMOP CDM harmonization → All downstream analyses
├── FHIR parsing → Modern EHR integration
└── Data quality assessment → Regulatory requirement

STATISTICAL ORCHESTRATION LAYER (Phase 3):
├── WeightIt integration → Propensity score methods
├── cobalt integration → Balance assessment → Depends on WeightIt
├── MatchIt integration → Alternative to weighting
└── Survival analysis (already implemented)

WORKFLOW ORCHESTRATION LAYER (Phase 4):
├── Machine-readable study specs → Declarative workflows
├── Async job handling → Long-running analyses
├── Workflow state management → Job recovery
└── Result caching → Performance optimization

REGULATORY LAYER (Phase 5):
├── admiral integration → ADaM generation → Depends on OMOP harmonization
├── 21 CFR Part 11 audit trails → Secure, non-editable logs
├── Estimand specification → Study protocol generation
└── Submission package export → Depends on all outputs

AI EXCELLENCE LAYER (Phase 6):
├── MCP protocol support → Auto-discovery
├── NLP study spec parser → Requires LLM integration
├── Confidence scoring → Depends on statistical layer
└── Federated analysis → Multi-site orchestration
```

**Critical path:** API versioning → OpenAPI spec → WeightIt integration → cobalt integration → Machine-readable study specs → Async jobs

**Parallel tracks:**
- FHIR parsing (independent)
- admiral integration (independent, can start after OMOP harmonization)
- Authentication (independent, infrastructure concern)

---

## MVP Recommendation

For MVP (first external AI agent integration), prioritize:

### Must Have (MVP Blockers):
1. **API versioning** (v1 prefix on all endpoints)
2. **OpenAPI 3.0 specification** (enables agent discovery)
3. **Structured error responses** (error_code, message, type, hint fields)
4. **WeightIt integration** (replace custom PS implementation)
5. **cobalt integration** (standardized balance assessment)
6. **Machine-readable study spec (YAML)** (basic version: treatment, outcome, covariates, method)
7. **Async job handling** (job submission → job_id → polling endpoint)
8. **Authentication** (API key-based, minimum viable)

### Should Have (Strong MVP):
9. **admiral ADSL generation** (most common ADaM dataset)
10. **Audit trail persistence** (write logs to immutable storage)
11. **FHIR parsing** (modern EHR standard)
12. **Confidence scores** (expose model warnings, convergence issues)
13. **Webhook notifications** (job completion events)

### Defer to Post-MVP:
- MCP protocol support (wait for ecosystem maturity)
- NLP study spec parser (requires LLM integration, complex)
- Federated analysis (niche use case, very complex)
- Regulatory submission package export (ultimate goal, but not MVP)
- Multi-study orchestration (sensitivity analysis, defer)
- Smart covariate selection (ML-based, research needed)
- Cost estimation (need operational data first)

**Rationale:** MVP focuses on making the platform **reliably consumable by AI agents** (versioned API, structured I/O, async handling, auth) while integrating **best-in-class packages** (WeightIt, cobalt, admiral). This validates the core hypothesis: "AI agents can orchestrate RWE studies through a unified API."

---

## Implementation Priorities by User Journey

### AI Agent Journey: "Generate RWE Study from Natural Language"
1. Agent receives request: "Compare drug A vs B in diabetic patients, adjusting for age/BMI/HbA1c"
2. Agent queries API for available methods (OpenAPI spec)
3. Agent constructs study spec YAML: treatment, outcome, covariates, PS method
4. Agent submits study via POST /studies (async) → receives job_id
5. Agent polls GET /studies/{job_id} → status: running → queued → completed
6. Agent retrieves results: balance assessment, effect estimates, survival curves
7. Agent generates regulatory report using templates

**Blockers if missing:**
- No OpenAPI spec → agent can't discover endpoints
- No study spec schema → agent doesn't know expected format
- No async jobs → long-running studies timeout
- No structured errors → agent can't recover from failures

### Human Data Scientist Journey: "Validate AI Agent Output"
1. Reviews study spec YAML submitted by agent
2. Inspects audit trail for all operations
3. Re-runs analysis with same seed → verifies reproducibility
4. Examines balance diagnostics (cobalt plots)
5. Reviews regulatory report for submission

**Blockers if missing:**
- No audit trails → can't verify what agent did
- No reproducibility → can't defend results to regulators
- Poor balance diagnostics → can't assess confounding control

### Regulator Journey: "Audit RWE Submission"
1. Requests audit trail showing all data transformations
2. Verifies propensity score specification and balance
3. Checks ADaM datasets match analysis populations
4. Confirms 21 CFR Part 11 compliance (secure, attributable logs)

**Blockers if missing:**
- No 21 CFR Part 11 audit trails → submission rejected
- No ADaM datasets → manual generation required
- Inconsistent balance reporting → reviewer questions validity

---

## Research Confidence Assessment

| Feature Category | Confidence | Evidence Source |
|------------------|------------|-----------------|
| API design for AI agents | HIGH | Multiple 2026 sources: MuleSoft, Gravitee, Nordic APIs on agent-friendly API patterns |
| RWE platform requirements | MEDIUM | AWS, Veradigm, TriNetX case studies. No AI-agent-specific platforms found (gap = opportunity) |
| OHDSI CohortMethod integration | HIGH | Official OHDSI docs, Strategus workflow documentation |
| WeightIt/cobalt capabilities | HIGH | Official package documentation (ngreifer GitHub, CRAN vignettes) |
| admiral ADaM generation | HIGH | Admiral 1.4 release notes (Jan 2026), official pharmaverse docs |
| 21 CFR Part 11 requirements | HIGH | FDA guidance documents, multiple compliance vendor sources |
| MCP protocol adoption | MEDIUM | Anthropic blog post. Emerging standard, limited production evidence |
| Healthcare AI agent trends | MEDIUM | BCG, Kellton, Anthropic healthcare announcements (2026). Forward-looking |
| Federated analysis needs | LOW | Mentioned in academic literature, but no production RWE platforms implement this yet |

---

## Open Questions for Phase-Specific Research

1. **MCP Protocol:** How stable is the spec? Should we wait for v2.0 or implement v1.0 now?
2. **FHIR Profiling:** Which FHIR profiles are most common in pharma RWD sources? (US Core, mCODE, others?)
3. **Async Job Infrastructure:** Existing R packages for job queues (crew, future, callr)? Performance benchmarks?
4. **ADaM Integration:** Does admiral require CDISC Pilot data format, or can it consume arbitrary OMOP? Need to test.
5. **Cost Modeling:** Typical runtime for 100K patient dataset: PS estimation, Cox model, matching? Need benchmarks.
6. **Authentication:** OAuth2 vs API keys for agent auth? OAuth is standard, but adds complexity.
7. **Federated Analysis:** Is this a real need or academic curiosity? Interview pharma data scientists.

---

## Sources

### AI Agent API Design
- [Rethinking API Design for Agentic AI | MuleSoft](https://blogs.mulesoft.com/automation/api-design-for-agentic-ai/)
- [How to Build AI APIs for Scalable Agent-Driven Systems | Gravitee](https://www.gravitee.io/blog/ai-apis-for-scalable-agent-systems)
- [10 AI-Driven API Economy Predictions for 2026 | Nordic APIs](https://nordicapis.com/10-ai-driven-api-economy-predictions-for-2026/)
- [How To Prepare Your API for AI Agents | The New Stack](https://thenewstack.io/how-to-prepare-your-api-for-ai-agents/)

### RWE Platform Landscape
- [Building a Real World Evidence Platform on AWS](https://aws.amazon.com/blogs/big-data/building-a-real-world-evidence-platform-on-aws/)
- [Veradigm Real-World Evidence Analytics Platform](https://veradigm.com/evalytica-real-world-evidence-analytics/)
- [TriNetX Real-world data for life sciences](https://trinetx.com/)
- [20 Real World Evidence Startups to Watch (2025) | StartUs Insights](https://www.startus-insights.com/innovators-guide/real-world-evidence-startups/)

### Healthcare AI Agents
- [Agentic AI in healthcare: Types, trends, and 2026 forecast | Kellton](https://www.kellton.com/kellton-tech-blog/agentic-ai-healthcare-trends-2026)
- [How AI Agents and Tech Will Transform Health Care in 2026 | BCG](https://www.bcg.com/publications/2026/how-ai-agents-will-transform-health-care)
- [Advancing Claude in healthcare and life sciences | Anthropic](https://www.anthropic.com/news/healthcare-life-sciences)
- [GitHub - AgenticHealthAI/Awesome-AI-Agents-for-Healthcare](https://github.com/AgenticHealthAI/Awesome-AI-Agents-for-Healthcare)

### OHDSI & Statistical Packages
- [New-User Cohort Method with Large Scale Propensity and Outcome Models | CohortMethod](https://ohdsi.github.io/CohortMethod/index.html)
- [GitHub - OHDSI/CohortMethod](https://github.com/OHDSI/CohortMethod)
- [Weighting for Covariate Balance in Observational Studies | WeightIt](https://ngreifer.github.io/WeightIt/)
- [Covariate Balance Tables and Plots | cobalt](https://ngreifer.github.io/cobalt/)
- [admiral 1.4 release | R-bloggers](https://www.r-bloggers.com/2026/01/admiral-1-4-release/)
- [ADaM in R Asset Library | admiral](https://pharmaverse.github.io/admiral/)

### Regulatory Compliance
- [FDA Audit Trails Explained: Ensuring Regulatory Compliance | ComplianceG](https://www.complianceg.com/fda-audit-trail/)
- [21 CFR Part 11 Audit Trail Requirements | SimplerQMS](https://simplerqms.com/21-cfr-part-11-audit-trail/)
- [Understanding Audit Trail Requirements in Electronic GxP Systems | Montrium](https://blog.montrium.com/experts/understanding-audit-trail-requirements-in-electronic-gxp-systems)

### Clinical Research APIs
- [Cloud API: Revolutionizing data integration for Clinical Trials | Empatica](https://www.empatica.com/blog/cloud-api-revolutionizing-data-integration-for-clinical-trials/)
- [APIs in Clinical Trials Explained | RealTime](https://realtime-eclinical.com/2025/07/16/apis-in-clinical-trials-explained-what-they-are-and-why-your-site-needs-one/)

### API Best Practices
- [API Versioning Best Practices for Backward Compatibility | Endgrate](https://endgrate.com/blog/api-versioning-best-practices-for-backward-compatibility)
- [Managing API Changes: 8 Strategies That Reduce Disruption by 70% (2026 Guide) | Theneo](https://www.theneo.io/blog/managing-api-changes-strategies)
- [Structured model outputs | OpenAI API](https://platform.openai.com/docs/guides/structured-outputs)

### Healthcare Data Architecture
- [Why we need to transform our healthcare data architecture | World Economic Forum](https://www.weforum.org/stories/2026/01/ai-healthcare-data-architecture/)
- [Architecting the Synchronized Digital Health System: Top Trends for 2026 | PubNub](https://www.pubnub.com/blog/architecting-the-synchronized-digital-health-system-2026-trends/)
- [Building a Unified Healthcare Data Platform: Architecture | Doctolib](https://medium.com/doctolib/building-a-unified-healthcare-data-platform-architecture-2bed2aaaf437)

---

**Total Sources:** 30+ authoritative sources spanning AI agent design, RWE platforms, statistical packages, regulatory compliance, and healthcare data architecture.

**Research Date:** 2026-01-30
**Confidence Level:** HIGH for table stakes features, MEDIUM for emerging differentiators (MCP, federated analysis)
