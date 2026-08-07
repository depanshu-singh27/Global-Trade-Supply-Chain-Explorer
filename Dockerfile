ARG R_VER=4.6.1
ARG ROCKER_DIGEST=sha256:555a0e7734b17f3901f01c8e379f87d797a0e6344a4cc3b246329ed3f0689809

FROM rocker/r-ver:${R_VER}@${ROCKER_DIGEST} AS build

ENV RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/__linux__/noble/latest \
    RENV_PATHS_LIBRARY=/opt/gtsc/renv/library \
    RENV_PATHS_CACHE=/opt/gtsc/renv/cache \
    USE_BUNDLED_LIBUV=1 \
    NOT_CRAN=true

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
    curl \
    git \
    pandoc \
    pkg-config \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libuv1-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libglpk-dev \
    libgmp-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/gtsc/app
COPY renv.lock DESCRIPTION ./
COPY renv/activate.R renv/settings.json renv/

RUN mkdir -p /opt/gtsc/renv/library \
 && R -q -e "options(repos = Sys.getenv('RENV_CONFIG_REPOS_OVERRIDE')); \
             install.packages('renv'); \
             renv::consent(provided = TRUE); \
             renv::restore(library = '/opt/gtsc/renv/library', exclude = c('prophet','rstan','rstantools','StanHeaders','RcppParallel'), prompt = FALSE); \
             stopifnot(dir.exists('/opt/gtsc/renv/library')); \
             cats <- list.files('/opt/gtsc/renv/library', recursive = FALSE); \
             message('LIBRARY_PKGS=', length(cats))"

FROM rocker/r-ver:${R_VER}@${ROCKER_DIGEST} AS runtime

LABEL org.opencontainers.image.title="Global Trade & Supply Chain Explorer" \
      org.opencontainers.image.description="Phase 14 release-candidate container" \
      org.opencontainers.image.version="0.1.0-rc.14" \
      phase="14-release-candidate"

ENV RENV_PATHS_LIBRARY=/opt/gtsc/renv/library \
    R_LIBS_USER=/opt/gtsc/renv/library \
    R_LIBS_SITE=/opt/gtsc/renv/library \
    RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
    GTSC_SKIP_RENV_ACTIVATE=true \
    GTSC_HOST=0.0.0.0 \
    GTSC_PORT=3838 \
    GTSC_RUNTIME_PROFILE=demo \
    GTSC_PUBLIC_MODE=true \
    GTSC_ALLOW_SCENARIO_WRITES=false \
    GTSC_READ_ONLY_MODE=true \
    GTSC_ENABLE_TECHNICAL_DIAGNOSTICS=false \
    GTSC_MATERIALIZE_WEB_DEPS=true \
    GTSC_PREFER_RDS=true \
    GTSC_RDS_ROOT=/opt/gtsc/runtime-rds \
    GTSC_DATA_ROOT=/opt/gtsc/data \
    GTSC_SCENARIO_ROOT=/opt/gtsc/scenarios \
    GTSC_PERFORMANCE_ROOT=/opt/gtsc/performance \
    GTSC_HEALTHCHECK_PATH=/__gtsc_health__ \
    MALLOC_ARENA_MAX=1 \
    MALLOC_TRIM_THRESHOLD_=65536 \
    MALLOC_MMAP_THRESHOLD_=65536 \
    R_GC_MEM_GROW=0 \
    OMP_NUM_THREADS=1 \
    OPENBLAS_NUM_THREADS=1 \
    MKL_NUM_THREADS=1 \
    R_DATATABLE_NUM_THREADS=1 \
    TMPDIR=/tmp/gtsc \
    HOME=/home/gtsc

RUN apt-get update && apt-get install -y --no-install-recommends \
    tini \
    curl \
    ca-certificates \
    libcurl4 \
    libssl3 \
    libxml2 \
    libfontconfig1 \
    libfreetype6 \
    libpng16-16 \
    libtiff6 \
    libjpeg62-turbo \
    libharfbuzz0b \
    libfribidi0 \
    gdal-bin \
    libgdal34t64 \
    libgeos3.12.1t64 \
    libgeos-c1t64 \
    libproj25 \
    libudunits2-0 \
    libglpk40 \
    libgmp10 \
    libuv1 \
    && rm -rf /var/lib/apt/lists/* \
    || (apt-get update && apt-get install -y --no-install-recommends \
        tini curl ca-certificates \
        libcurl4 libssl3 libxml2 \
        libfontconfig1 libfreetype6 libpng16-16 \
        gdal-bin libgdal-dev libgeos-dev libproj-dev libudunits2-0 \
        libglpk40 libgmp10 libuv1 \
        && rm -rf /var/lib/apt/lists/*)

RUN useradd --create-home --uid 10001 --shell /usr/sbin/nologin gtsc \
    && mkdir -p /opt/gtsc/app /opt/gtsc/data /opt/gtsc/scenarios/results \
               /opt/gtsc/performance /tmp/gtsc /opt/gtsc/renv/library \
    && chown -R gtsc:gtsc /opt/gtsc /tmp/gtsc /home/gtsc

COPY --from=build /opt/gtsc/renv/library /opt/gtsc/renv/library
WORKDIR /opt/gtsc/app

COPY --chown=gtsc:gtsc app.R config.yml DESCRIPTION renv.lock ./
COPY --chown=gtsc:gtsc R ./R
COPY --chown=gtsc:gtsc www ./www
COPY --chown=gtsc:gtsc data/scenarios/examples ./data/scenarios/examples
COPY --chown=gtsc:gtsc data/scenarios/definitions/.gitkeep ./data/scenarios/definitions/
COPY --chown=gtsc:gtsc data/release/demo ./data/release/demo
COPY --chown=gtsc:gtsc docker/entrypoint.sh docker/healthcheck.sh /opt/gtsc/
COPY --chown=gtsc:gtsc docker/permission_smoke.R /opt/gtsc/
COPY --chown=gtsc:gtsc docker/convert_demo_parquet_to_rds.R /opt/gtsc/

RUN Rscript /opt/gtsc/convert_demo_parquet_to_rds.R \
    /opt/gtsc/app/data/release/demo /opt/gtsc/runtime-rds

COPY --chown=gtsc:gtsc renv/activate.R renv/settings.json ./renv/

RUN chmod 755 /opt/gtsc/entrypoint.sh /opt/gtsc/healthcheck.sh \
    && chmod -R a-w /opt/gtsc/app \
    && chmod u+w /opt/gtsc/app/www \
    && chown -R root:root /opt/gtsc/renv/library \
    && chmod -R u+w,go-w /opt/gtsc/renv/library \
    && install -d -o gtsc -g gtsc -m 700 /tmp/gtsc \
    && chmod -R u+w /opt/gtsc/scenarios \
    && chown -R gtsc:gtsc /opt/gtsc/scenarios /opt/gtsc/app/www

USER gtsc

RUN Rscript /opt/gtsc/permission_smoke.R

EXPOSE 3838
HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=5 \
  CMD ["/opt/gtsc/healthcheck.sh"]

ENTRYPOINT ["/usr/bin/tini", "--", "/opt/gtsc/entrypoint.sh"]
