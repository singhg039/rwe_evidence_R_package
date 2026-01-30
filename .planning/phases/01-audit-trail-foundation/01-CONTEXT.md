# Phase 1: Audit Trail Foundation - Context

**Gathered:** 2026-01-30
**Status:** Ready for planning

<domain>
## Phase Boundary

Establish an immutable audit trail system that captures all delegated package calls (WeightIt, cobalt, MatchIt, admiral) for 21 CFR Part 11 regulatory compliance. Every operation becomes traceable with timestamp, function, package version, parameters, and execution metadata. Enables reproducibility from audit trail alone.

</domain>

<decisions>
## Implementation Decisions

### Audit Entry Schema
- Capture all warnings/errors from delegated packages (convergence messages, warnings, errors) - critical for regulatory defense
- User/session attribution deferred to Phase 12 (production readiness) - handle when authentication is added
- Claude's discretion: exact fields for parameters, checksums, output handling based on 21 CFR Part 11 requirements

### Immutability Approach
- Local file per session: `audit_YYYYMMDD_HHMMSS.jsonl` - simple, portable
- Audit enabled by default for API calls, opt-in for interactive console use
- On write failure: buffer in memory, queue writes, flush periodically, fail only if buffer full
- Claude's discretion: hash chain vs signatures vs external timestamping based on regulatory requirements and complexity

### Export Formats
- Both JSON and YAML exports: `export_audit_trail(format = 'json'|'yaml')`
- PDF summary optional via parameter: `export_audit_trail(pdf = TRUE)` generates formatted report for regulatory binders
- Full export only - no filtering, let reviewers use their own tools
- Separate session_info.txt file alongside audit export with R version, platform, loaded packages

### Data Lineage DAG
- Both visualization formats: `visualize_lineage()` for interactive (DiagrammeR/Mermaid), `export_lineage(format='dot')` for portability
- Support provenance queries: `trace_result()` to trace backwards from final output to all inputs
- Configurable granularity: default to stage-level (ingestion, harmonization, analysis, reporting), expand to function-level on request
- Claude's discretion: internal representation (node/edge lists vs parent references)

### Claude's Discretion
- Exact audit entry schema fields beyond timestamp, function, package, version (what to capture for parameters, checksums)
- Output/result capture strategy (summary vs full object vs cached pointer)
- Hash chain implementation details for tamper-evidence
- DAG internal data structure

</decisions>

<specifics>
## Specific Ideas

- Per-session files make it easy to bundle audit trail with analysis results for regulatory submission
- Buffering on write failure prevents lost audits during long-running analyses
- Provenance query `trace_result()` should answer "show me everything that went into this ATE estimate"
- Stage-level DAG for executives, function-level for technical reviewers

</specifics>

<deferred>
## Deferred Ideas

- User/session attribution for multi-user scenarios - Phase 12 (Production Readiness)
- Filtering/querying audit exports - not needed for MVP, reviewers have own tools

</deferred>

---

*Phase: 01-audit-trail-foundation*
*Context gathered: 2026-01-30*
