# Global Trade & Supply Chain Explorer

> An interactive R Shiny platform for exploring global HS Chapter 85 trade, bilateral commodity flows, trade networks, import concentration, supply-chain shocks, and monthly forecasting workflows.

## Overview

The **Global Trade & Supply Chain Explorer** is a modular analytics platform built with **R and Shiny** for exploring international trade patterns, bilateral commodity relationships, supply-chain dependencies, network structure, and disruption scenarios.

The application combines global country-level HS Chapter 85 trade data with a selected bilateral HS4 analytical universe, enriches the data with World Bank macroeconomic indicators, and presents the results through nine interactive analytical modules.

The platform supports:

- Executive-level trade analysis
- Bilateral trade-flow exploration
- Trade-balance mapping
- Commodity and country time-series analysis
- Trade-network analysis
- Import-dependency diagnostics
- Deterministic supplier-shock simulation
- Monthly forecasting workflows
- Data-quality and pipeline diagnostics

The public deployment operates from processed local data in a **read-only runtime profile** and does not require live UN Comtrade or World Bank API access during a user session.

---

## Key Highlights

- **Nine interactive analytical modules**
- Global HS-85 analytics covering **177 economies**
- Analytical period spanning **2019–2024**
- **87,609 enriched bilateral HS4 records**
- Selected detailed universe covering **20 reporters, 20 partners, and 20 HS4 categories**
- Interactive Sankey diagrams, choropleths, time-series charts, treemaps, network graphs, heatmaps, rankings, and tables
- Directed trade-network analysis using **PageRank, degree, strength, and betweenness**
- Supplier-concentration analysis using **HHI, top-supplier shares, and effective supplier counts**
- Deterministic shock simulation with supplier substitution and residual unmet-import accounting
- Forecasting using **Seasonal Naïve, Naïve, Drift, ETS, and ARIMA**
- Reproducible R environment management using **renv**
- Automated testing using **testthat**
- CI workflows through **GitHub Actions**
- Docker-based release validation
- Public deployment through **shinyapps.io**
- Dependency-trend computation reduced from approximately **3.54 seconds to 0.10 seconds** in local validation for the tested 2019–2024 workflow

---

## Application Modules

| Module | What it provides |
|---|---|
| **Executive Overview** | Country-year KPIs, trade rankings, composition views, balances, macroeconomic comparisons, and downloads |
| **Trade Flows** | Bilateral HS4 filtering, Sankey diagrams, composition views, time series, reporter-partner matrices, and CSV exports |
| **Trade Balance Map** | Interactive Leaflet choropleths of country-level trade totals and balances, rankings, profiles, and trend views |
| **Time Series & Commodity Analysis** | Global, single-economy, comparison, and detailed scopes with YoY growth, indexed series, commodity movers, treemaps, and tables |
| **Trade Network** | Directed trade graphs, PageRank and other centrality measures, corridor analysis, diagnostics, and GraphML/CSV downloads |
| **Dependency Explorer** | Supplier concentration, HHI, top-supplier shares, effective supplier counts, heatmaps, rankings, and historical trends |
| **Shock Simulator** | Supplier-shock configuration, substitution, residual exposure, optional propagation, impact rankings, maps, and downloads |
| **Forecasting** | Model comparison, residual diagnostics, point forecasts, and 80%/95% prediction intervals using fixture-labelled monthly data |
| **Data Quality** | Pipeline status, validation outputs, analytical coverage summaries, and performance notices |

---

## Data Coverage

### Verified Analytical Snapshot

| Layer | Coverage | Records |
|---|---|---:|
| Global enriched HS-85 trade | 177 economies, 2019–2024 | 1,955 |
| Country-year analytical table | 177 economies | 981 |
| Detailed bilateral HS4 dataset | Selected 20-reporter, 20-partner, 20-HS4 universe, 2019–2024 | 87,609 |
| WDI wide table | Production analytical universe | 1,050 |
| WDI long table | Production analytical universe | 4,200 |
| Monthly forecasting table | Fixture/live pipeline context | 864 |

---

## Data Sources

### UN Comtrade

Used for annual Harmonized System trade data and the project's monthly trade-ingestion architecture.

### World Bank World Development Indicators

Used to enrich the trade dataset with macroeconomic variables including:

- GDP
- Population
- Consumer Price Index
- Related country-level indicators

### Natural Earth

Used for simplified world geometries powering the interactive geographical modules.

### Local Analytical Bundles

Processed **Parquet, RDS, and JSON** artefacts support offline analysis and the public deployment runtime.

The deployed application does **not** call UN Comtrade or World Bank APIs while a user is interacting with the dashboard.

---

## Architecture

### Runtime Design

The application follows a modular offline-processing and interactive-analysis architecture.

1. Offline ingestion scripts retrieve and standardise Comtrade and WDI data.
2. Processing pipelines validate and transform the raw observations.
3. Processed analytical artefacts are written to local storage.
4. `app.R` loads application configuration and dependencies.
5. `app_server()` loads the processed analytical snapshot.
6. The snapshot is shared across the individual Shiny modules.
7. Runtime profiles control demo, public, read-only, diagnostic, and scenario-write behaviour.
8. Docker is used to validate release bundles before deployment.
9. The public shinyapps.io deployment uses an **RDS-preferred runtime path** for efficient startup.

---

## Technology Stack

### Language and Application Framework

- R
- Shiny
- bslib

### Data Engineering

- data.table
- arrow
- Parquet
- RDS
- fst
- jsonlite
- yaml
- httr2
- memoise
- cachem

### Visualisation and Analytics

- Plotly
- Leaflet
- networkD3
- DT
- igraph
- sf
- rnaturalearth

### Forecasting

- forecast
- Seasonal Naïve
- Naïve
- Drift
- ETS
- ARIMA via `forecast::auto.arima()`

The codebase retains an optional Prophet implementation path. Prophet is **not exposed in the current public hosted application** because its required package and runtime stack are not included in the shinyapps.io deployment.

### Testing and Deployment

- testthat
- renv
- Docker
- tini
- GitHub Actions
- shinyapps.io

---

## Analytical Methods

### Trade Balance

$$
\text{Trade Balance} = \text{Exports} - \text{Imports}
$$

A positive value represents a trade surplus, while a negative value represents a trade deficit.

### Year-on-Year Growth

For consecutive valid years with a non-zero prior observation:

$$
\text{YoY Growth}_t =
\frac{x_t - x_{t-1}}{x_{t-1}} \times 100
$$

### Indexed Series

$$
\text{Index}_t =
100 \times \frac{x_t}{x_{\text{baseline}}}
$$

The baseline is the first valid positive observation within the selected analytical period.

### Supplier Concentration

For supplier shares \(s_i\):

$$
\text{HHI} = \sum_i s_i^2
$$

The effective number of suppliers is calculated as:

$$
\text{Effective Suppliers} = \frac{1}{\text{HHI}}
$$

The Dependency Explorer additionally reports:

- Largest supplier share
- Top-three supplier share
- Supplier count
- Effective supplier count
- Import-concentration rankings
- Historical dependency trends

---

## Trade Network Analysis

Filtered bilateral trade observations are represented as a **directed weighted graph**.

Nodes represent economies, while edges represent observed bilateral trade relationships.

The application calculates network measures including:

- In-degree
- Out-degree
- Weighted strength
- PageRank
- Betweenness centrality
- Trade-corridor values

These measures describe the **available filtered observation network** and should not be interpreted as a complete global firm-level supply network.

---

## Shock Simulation Engine

The Shock Simulator provides a deterministic framework for analysing potential supplier disruptions.

### Workflow

1. **Build the baseline**  
   Observed bilateral import relationships are transformed into the initial dependency structure.

2. **Select shock parameters**  
   Users configure supplier, reporter, commodity, year, and shock magnitude.

3. **Apply the direct shock**  
   Targeted supplier edges are reduced according to the selected shock percentage.

4. **Allocate substitution**  
   Alternative suppliers absorb disrupted trade using proportional or capacity-constrained allocation rules.

5. **Calculate residual exposure**  
   Any disrupted imports that cannot be reallocated are recorded as residual unmet imports.

6. **Optionally propagate effects**  
   Residual effects can be propagated through additional analytical steps.

7. **Aggregate results**  
   Impacts are summarised by reporter, supplier, commodity, and scenario.

8. **Visualise outputs**  
   Results are presented through rankings, charts, maps, tables, diagnostics, and downloadable outputs.

### Interpretation

Shock-simulation outputs represent **scenario sensitivities and residual unmet imports**.

They should not be interpreted as forecasts of:

- Realised GDP loss
- Company failure
- Market collapse
- Macroeconomic damage

The simulator is intended for comparative analytical exploration rather than causal economic prediction.

---

## Forecasting

The public forecasting module supports:

- Seasonal Naïve
- Naïve
- Drift
- ETS
- ARIMA

Models can provide:

- Point forecasts
- 80% prediction intervals
- 95% prediction intervals
- Residual diagnostics
- Historical comparison views
- Backtest metrics where available

The current public forecasting profile uses **synthetic monthly fixture data**.

Forecast results should therefore be interpreted as a demonstration of the forecasting architecture and interface rather than live production trade predictions.

An optional Prophet implementation remains in the broader codebase, but Prophet is not included in the current public shinyapps.io runtime.

---

## Performance

The repository contains a dedicated server-side benchmark framework.

Some historical benchmark artefacts were generated against an earlier detailed dataset containing approximately 21,931 bilateral records. These results should therefore not automatically be interpreted as benchmarks for the current 87,609-record analytical dataset without re-running the complete performance suite.

| Operation | Recorded Result |
|---|---:|
| Overview year aggregation | ~10 ms median |
| Trade-flow filtering and Sankey preparation | ~50–60 ms median |
| Network construction and PageRank | ~60–80 ms median |
| Cold analytical snapshot load | ~505–610 ms p95 |
| Shock simulation without persistence | ~567–574 ms p95 |
| Dependency-trend calculation | ~3.54 s → ~0.10 s after optimisation |

The project does **not** claim sub-250 ms shock-simulation performance.

Browser-level end-to-end performance has not yet been established through the benchmark harness.

---

## Testing and Quality Assurance

The project includes:

- **66 `testthat` files**
- **317 `test_that()` blocks**
- Unit and integration coverage across:
  - Data access
  - Executive Overview
  - Trade Flows
  - Mapping
  - Time Series
  - Network analysis
  - Dependency calculations
  - Shock simulation
  - Forecasting
  - Runtime profiles
  - Release packaging
  - Performance
  - Container behaviour

The repository also includes GitHub Actions workflows for automated testing, container builds, and security-oriented checks.

The exact public deployment bundle is additionally smoke-tested locally using Docker before deployment.

### Running Tests

```bash
Rscript -e "renv::status()"
Rscript -e "testthat::test_dir('tests/testthat', reporter='summary')"
Rscript -e "source('app.R'); cat('APP_SOURCE_OK\n')"
```

---

## Security and Public Deployment Safeguards

The project includes several safeguards for public deployment:

- Docker runtime executes as a non-root `gtsc` user
- Container startup rejects root execution
- `.Renviron` files are rejected from the container image
- The Comtrade API key is not required by the public runtime
- Public runtime defaults to read-only mode
- Scenario persistence is disabled by default
- Technical diagnostics are disabled by default
- Dedicated health-check support exists for container deployments
- Secrets are excluded from version control
- Generated deployment artefacts are excluded from version control
- Local runtime bundles are excluded from version control

---

## Repository Structure

```text
.
├── app.R
├── R/
│   ├── Shiny modules
│   ├── Analytics
│   ├── Forecasting
│   ├── Data pipelines
│   └── Runtime helpers
├── www/
│   └── CSS and static application resources
├── data/
│   ├── processed/
│   ├── release/demo/
│   ├── scenarios/
│   └── performance/
├── docker/
├── scripts/
├── tests/testthat/
├── config.yml
├── DESCRIPTION
├── renv.lock
├── Dockerfile
└── .github/workflows/
```

Local deployment-only directories such as:

```text
runtime-rds/
shinyapps-deploy/
```

are intentionally excluded from Git.

---

## Known Limitations

### Data Scope

- Bilateral analysis covers a selected **20-reporter, 20-partner, 20-HS4** analytical universe rather than complete global bilateral trade.
- The project focuses specifically on **HS Chapter 85**.
- Dependency metrics represent observed trade relationships rather than firm-level supply-chain relationships.
- The model does not include a complete global input-output framework.

### Forecasting

- The current public forecasting profile uses synthetic monthly fixture data.
- A validated live monthly production forecasting pipeline has not yet been established.
- Prophet is not included in the current public hosted deployment.

### Scenario Modelling

- Shock outputs represent deterministic analytical sensitivities.
- Results should not be treated as realised-loss forecasts.
- Supplier-substitution rules are analytical assumptions.
- Propagation logic does not represent a complete economic-equilibrium model.

### Performance and Reproducibility

- Some historical benchmark artefacts were generated on an earlier, smaller detailed dataset.
- Large processed and runtime artefacts are intentionally excluded from Git.
- Full browser-level end-to-end performance has not yet been benchmarked.

---

## Project Purpose

This project demonstrates an end-to-end analytical workflow combining:

- Data engineering
- International trade analytics
- Supply-chain risk analysis
- Network science
- Time-series forecasting
- Interactive data visualisation
- Scenario modelling
- Reproducible software engineering
- Automated testing
- Containerisation
- Public application deployment

The objective is not only to visualise trade data, but to provide an integrated analytical environment for exploring how global trade relationships, supplier concentration, and potential disruptions interact.

---
