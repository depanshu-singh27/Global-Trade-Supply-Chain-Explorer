#!/usr/bin/env bash
set -euo pipefail
HOST="${GTSC_HOST:-127.0.0.1}"
if [[ "${HOST}" == "0.0.0.0" ]]; then HOST="127.0.0.1"; fi
PORT="${GTSC_PORT:-3838}"
PATH_H="${GTSC_HEALTHCHECK_PATH:-/__gtsc_health__}"
URL="http://${HOST}:${PORT}${PATH_H}"
curl -fsS --max-time 4 "${URL}" | grep -q '"status"[[:space:]]*:[[:space:]]*"ok"'
