# External Integrations

**Analysis Date:** 2026-01-30

## APIs & External Services

**EHR Data Sources:**
- OMOP CDM (Observational Medical Outcomes Partnership) - Healthcare data standard format
  - SDK/Client: `DBI` + `RPostgres` / `odbc` via `rwe_read_ehr()`
  - Support: File-based (CSV) or database connections

- FHIR (Fast Healthcare Interoperability Resources) - HL7 standard format
  - SDK/Client: `httr2` for REST API calls, `jsonlite` for JSON parsing
  - Auth: Bearer token, basic auth, API key, or no auth
  - Implementation: `read_ehr_api()` with FHIR-specific extraction

- Cerner EHR System - Hospital EHR vendor format
  - SDK/Client: `DBI` + `RPostgres` / `odbc` for database reads, or `httr2` for APIs
  - Auth: Configurable (bearer, basic, API key)

- Epic EHR System - Hospital EHR vendor format
  - SDK/Client: `DBI` + database connections or `httr2` for SMART on FHIR APIs
  - Auth: Configurable authentication methods

- Claims Data (837 format) - Insurance claims standard
  - SDK/Client: `rwe_read_claims()` - delegates to `rwe_read_ehr()` with custom format
  - Support: File-based or database sources

- Registry Data - Clinical registries
  - SDK/Client: `rwe_read_registry()` - wrapper around `rwe_read_ehr()`
  - Support: Multiple registry formats (SEER, etc.)

**Generic REST API Integration:**
- Location: `R/ehr_readers.R` - `read_ehr_api()` function
- Capabilities:
  - Bearer token authentication
  - Basic authentication (username/password)
  - API key authentication (X-API-Key header)
  - Custom HTTP headers
  - Query parameters
  - Automatic pagination handling (limit, offset, page parameters)
  - JSON response parsing
  - Support for nested data extraction (common wrapper keys: data, results, records, entries, items, resources)

## Data Storage

**Databases:**
- PostgreSQL
  - Connection: Via `DBI::dbConnect(RPostgres::Postgres(), ...)`
  - Client: `RPostgres` (>= 1.4.0)
  - Usage: `rwe_read_ehr()` with DBIConnection object
  - Schema support: Configurable schema parameter

- Generic ODBC Databases
  - Connection: Via `DBI::dbConnect(odbc::odbc(), ...)`
  - Client: `odbc` (>= 1.3.0)
  - Usage: `rwe_read_ehr()` with DBIConnection object

**File Storage:**
- Local filesystem (primary)
  - CSV files - `readr::read_csv()` or `utils::read.csv()`
  - TSV files - Tab-separated variant of CSV
  - Parquet files - Apache columnar format via `arrow::read_parquet()`
  - Arrow IPC files - Arrow interchange format via `arrow::read_ipc_file()`
  - Feather files - Arrow columnar format via `arrow::read_feather()`
  - RDS files - R native serialized format via `readRDS()`
  - Automatic format detection via file extension in `read_ehr_file()`

- Remote file URLs (API endpoints)
  - HTTP/HTTPS URLs detected and routed to `read_ehr_api()`

**Caching:**
- None built-in
- Lazy evaluation available via `arrow::open_dataset()` for large files
- In-memory API state cache in `inst/api/plumber.R` (session-based, not persistent)

## Authentication & Identity

**Auth Provider:**
- Custom implementation (no external auth service integrated)
- Implemented in `R/ehr_readers.R` - `add_api_auth()` function

**Authentication Methods:**
- Bearer Token
  - Header: `Authorization: Bearer {token}`
  - Function: `httr2::req_auth_bearer_token()`
  - Usage: Pass `auth_token` parameter to `read_ehr_api()`

- Basic Authentication
  - Header: `Authorization: Basic {base64(username:password)}`
  - Function: `httr2::req_auth_basic()`
  - Usage: Pass `username` and `password` parameters

- API Key
  - Header: `X-API-Key: {api_key}`
  - Function: Manual header addition
  - Usage: Pass `api_key` parameter

- No Authentication
  - Default: Public endpoints supported
  - Usage: `auth_type = "none"`

**Credential Storage:**
- Environment variables (recommended pattern, not enforced)
- Function parameters (for API calls)
- No secrets management library integrated

## Monitoring & Observability

**Error Tracking:**
- None (no external integration)
- Error handling via `tryCatch()` blocks
- Errors logged via `log_error()` function

**Logs:**
- Approach: Internal logging system via `R/logging.R`
  - Setup via `setup_logger()` function
  - Log levels: DEBUG, INFO, WARNING, ERROR
  - Optional file output: `setup_logger(file = "path/to/log.log")`
  - In-memory log history: `.logger_env$log_history`
  - Console output configurable
  - Context tracking for operations (operation name in logs)

- Integration points: Logging used throughout package
  - `R/ehr_readers.R` - Data load operations
  - `R/quality_assessment.R` - QA assessment steps
  - `R/causal_inference.R` - Matching operations
  - `R/helpers.R` - Validation checks
  - `inst/api/plumber.R` - API operations

## CI/CD & Deployment

**Hosting:**
- CRAN (Comprehensive R Archive Network) - Primary distribution
- GitHub repository - Source code hosting
  - Repository: https://github.com/singhg039/rwe_evidence_R_package
  - Issue tracking: https://github.com/singhg039/rwe_evidence_R_package/issues

**API Deployment:**
- Plumber framework (`inst/api/plumber.R`)
  - Deploy via `plumber::plumb()` and `plumber::pr_run()`
  - Standalone: Can run as independent HTTP server
  - Docker: Can be containerized
  - CORS enabled for cross-origin requests

**CI Pipeline:**
- Not detected
- Tests present in `tests/testthat/` directory
- CRAN submission via standard R package submission process

## Environment Configuration

**Required env vars:**
- Database connections: Connection strings (dbname, host, user, password)
  - Recommended: Store in `.Rprofile` or use `dotenv` pattern

- API authentication: Bearer tokens, API keys, credentials
  - Recommended: Environment variables (e.g., FHIR_API_TOKEN, EPIC_API_KEY)
  - No automatic loading from .env files

**Secrets location:**
- Function parameters (passed directly)
- R environment variables (Sys.getenv())
- .Renviron file (R session startup)
- No built-in secrets vault integration

## Webhooks & Callbacks

**Incoming:**
- Plumber API endpoints for inbound requests
  - POST `/data/upload` - File upload
  - POST `/quality/run` - Quality assessment trigger
  - POST `/matching/run` - Propensity score matching trigger
  - POST `/survival/run` - Survival analysis trigger
  - POST `/effectiveness/run` - Effectiveness metrics trigger
  - POST `/safety/run` - Safety surveillance trigger
  - POST `/reports/<format>` - Report generation trigger
  - GET `/health` - Health check
  - GET `/data/summary` - Dataset summary retrieval

**Outgoing:**
- None detected
- Package consumes APIs but does not send webhooks/callbacks

## Data Format Standards

**EHR Formats Supported:**
- OMOP CDM v5.x - Main table: person
- FHIR - Bundle entry extraction from FHIR API responses
- Cerner vendor format - Main table: person
- Epic vendor format - Main table: patient
- Custom format - Generic table handling

**Output Formats:**
- CSV - via `write.csv()`, `readr::write_csv()`
- Parquet - via `arrow::write_parquet()`
- RDS - R native binary format
- Word/PowerPoint - via `officer` + `flextable` (regulatory reports)
- HTML - via `htmltools` (interactive reports)
- LaTeX - via `kableExtra` / `xtable` (PDF reports)

---

*Integration audit: 2026-01-30*
