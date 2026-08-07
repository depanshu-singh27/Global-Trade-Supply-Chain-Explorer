#!/usr/bin/env bash
set -euo pipefail

export GTSC_RUNTIME_PROFILE="${GTSC_RUNTIME_PROFILE:-demo}"
export GTSC_HOST="${GTSC_HOST:-0.0.0.0}"
export GTSC_PORT="${GTSC_PORT:-3838}"
export GTSC_PUBLIC_MODE="${GTSC_PUBLIC_MODE:-true}"
export GTSC_ALLOW_SCENARIO_WRITES="${GTSC_ALLOW_SCENARIO_WRITES:-false}"
export GTSC_READ_ONLY_MODE="${GTSC_READ_ONLY_MODE:-true}"
export GTSC_ENABLE_TECHNICAL_DIAGNOSTICS="${GTSC_ENABLE_TECHNICAL_DIAGNOSTICS:-false}"
export GTSC_DATA_ROOT="${GTSC_DATA_ROOT:-/opt/gtsc/data}"
export GTSC_SCENARIO_ROOT="${GTSC_SCENARIO_ROOT:-/opt/gtsc/scenarios}"
export GTSC_PERFORMANCE_ROOT="${GTSC_PERFORMANCE_ROOT:-/opt/gtsc/performance}"
export GTSC_HEALTHCHECK_PATH="${GTSC_HEALTHCHECK_PATH:-/__gtsc_health__}"
export TMPDIR="${TMPDIR:-/tmp/gtsc}"
export HOME="${HOME:-/home/gtsc}"
export GTSC_SKIP_RENV_ACTIVATE=true
export RENV_CONFIG_AUTOLOADER_ENABLED=FALSE
export R_LIBS_USER="${RENV_PATHS_LIBRARY:-/opt/gtsc/renv/library}"
export R_LIBS_SITE="${RENV_PATHS_LIBRARY:-/opt/gtsc/renv/library}"

cd /opt/gtsc/app

if [[ "$(id -u)" -eq 0 ]]; then
  echo '{"level":"ERROR","msg":"refusing_root_runtime"}' >&2
  exit 1
fi

mkdir -p "${TMPDIR}" || true
if [[ "${GTSC_ALLOW_SCENARIO_WRITES}" == "true" ]]; then
  mkdir -p "${GTSC_SCENARIO_ROOT}/results" || true
fi

if [[ "${GTSC_RUNTIME_PROFILE}" == "demo" ]]; then
  if [[ ! -f "${GTSC_DATA_ROOT}/release_bundle_manifest.json" ]]; then
    if [[ -f /opt/gtsc/app/data/release/demo/release_bundle_manifest.json ]]; then
      export GTSC_DATA_ROOT="/opt/gtsc/app/data/release/demo"
    fi
  fi
fi

if [[ "${GTSC_RUNTIME_PROFILE}" == "release" || "${GTSC_RUNTIME_PROFILE}" == "external" ]]; then
  if [[ ! -f "${GTSC_DATA_ROOT}/release_bundle_manifest.json" ]]; then
    echo "{\"level\":\"ERROR\",\"msg\":\"missing_bundle_manifest\",\"profile\":\"${GTSC_RUNTIME_PROFILE}\"}" >&2
    exit 1
  fi
fi

if [[ -f /opt/gtsc/app/.Renviron ]]; then
  echo '{"level":"ERROR","msg":"renviron_present_in_image"}' >&2
  exit 1
fi
unset COMTRADE_PRIMARY || true

Rscript -e '
  .libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))
  source("R/utilities.R")
  source("R/config.R")
  source("R/runtime_profile.R")
  source("R/runtime_logging.R")
  source("R/runtime_health.R")
  source("R/release_config.R")
  source("R/release_security.R")
  source("R/release_bundle.R")
  source("R/release_manifest.R")
  source("R/release_validation.R")
  if (file.exists("R/forecast_formatters.R")) source("R/forecast_formatters.R")
  cfg <- normalise_runtime_config()
  set_runtime_config(cfg)
  runtime_log("INFO", "entrypoint_validation_start", list(profile = cfg$runtime_profile))
  validate_runtime_profile_or_stop(cfg, cfg$data_root)
  if (!file.exists("www/__gtsc_health__")) {
    write_health_www_resource("/opt/gtsc/app", cfg)
  }
  man <- safe_read_json(file.path(cfg$data_root, "release_bundle_manifest.json"))
  log_startup_metadata(cfg, man)
  runtime_log("INFO", "entrypoint_validation_ok", list(profile = cfg$runtime_profile))
'

exec Rscript -e ".libPaths(c(Sys.getenv('R_LIBS_USER'), .libPaths())); shiny::runApp('.', host=Sys.getenv('GTSC_HOST','0.0.0.0'), port=as.integer(Sys.getenv('GTSC_PORT','3838')), launch.browser=FALSE)"
