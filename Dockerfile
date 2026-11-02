# syntax=docker/dockerfile:1

# ---- builder: restore runtime packages from renv.lock and install acstar ----
FROM rocker/r-ver:4.6.2 AS builder

# Shared libraries the pre-built binaries link against (fs/httpuv: libuv,
# sodium: libsodium). curl, OpenSSL and ICU are already in the base image.
RUN apt-get update \
    && apt-get install -y --no-install-recommends libuv1t64 libsodium23 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build
ENV RENV_CONFIG_CACHE_ENABLED=FALSE \
    RENV_CONFIG_SANDBOX_ENABLED=FALSE

COPY DESCRIPTION NAMESPACE renv.lock ./
# Only the packages acstar imports (and their dependencies) are restored; dev
# tools such as testthat, covr and caret stay out of the image. The versions
# come from renv.lock; Posit Package Manager (the image's default repository)
# serves them as pre-built Linux binaries.
RUN Rscript -e ' \
    install.packages("renv", quiet = TRUE); \
    imports <- trimws(strsplit(read.dcf("DESCRIPTION")[, "Imports"], ",")[[1]]); \
    imports <- setdiff(sub("\\s.*", "", imports), rownames(installed.packages(priority = "base"))); \
    dir.create("/opt/rlib"); \
    renv::restore(lockfile = "renv.lock", library = "/opt/rlib", packages = imports, \
                  repos = getOption("repos"), prompt = FALSE)'

COPY R ./R
RUN R CMD INSTALL --no-docs --no-html --no-multiarch --library=/opt/rlib .

# Slimming. Help pages, HTML docs, vignettes, package tests and C headers are
# not used at runtime, nor are R's manuals, the littler site library, the
# X11/cairo graphics devices or Tcl/Tk. A missing shared library fails the build here
# rather than at request time (libR.so is resolved by R itself).
RUN echo "before: $(du -sh /opt/rlib | cut -f1) packages, $(du -sh /usr/local/lib/R | cut -f1) R" \
    && cd /opt/rlib \
    && rm -rf */help */html */doc */tests */include */po */demo BH \
    && cd /usr/local/lib/R \
    && rm -rf doc site-library library/*/help library/*/html library/*/doc library/*/tests \
              modules/R_X11.so modules/R_de.so library/grDevices/libs/cairo.so library/tcltk \
    && echo "after:  $(du -sh /opt/rlib | cut -f1) packages, $(du -sh /usr/local/lib/R | cut -f1) R" \
    && if find /opt/rlib /usr/local/lib/R -name '*.so' -exec ldd {} \; | grep 'not found' | grep -v 'libR.so'; then exit 1; fi

# ---- runtime: plain Ubuntu + R copied from the builder -----------------------
FROM ubuntu:26.04 AS runtime

# Run-time libraries of R and of the restored packages (found with ldd);
# libcurl brings its own TLS/Kerberos dependencies.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libbz2-1.0 libcurl4t64 libdeflate0 libgfortran5 libgmp10 libgomp1 libicu78 \
        liblzma5 libopenblas0-pthread libpcre2-8-0 libreadline8t64 libsodium23 \
        libuv1t64 libzstd1 zlib1g \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --create-home --uid 10001 app

COPY --from=builder /usr/local/lib/R /usr/local/lib/R
COPY --from=builder /usr/local/bin/R /usr/local/bin/Rscript /usr/local/bin/
COPY --from=builder /opt/rlib /opt/rlib

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    TZ=UTC \
    R_LIBS=/opt/rlib \
    APP_HOST=0.0.0.0 \
    APP_PORT=8000 \
    APP_MODEL_PATH=/app/models/model.rds \
    APP_REPORTS_DIR=/app/reports

WORKDIR /app
COPY --chown=app:app exec ./exec
COPY --chown=app:app config ./config
COPY --chown=app:app data/raw ./data/raw
# Tuned hyper-parameters, so `train` in the container skips the grid search.
COPY --chown=app:app reports/tuning_default.json reports/tuning_original.json ./reports/
RUN mkdir -p /app/models && chown app:app /app /app/models

USER app
EXPOSE 8000

HEALTHCHECK --interval=30s --timeout=5s --start-period=30s --retries=3 \
    CMD ["Rscript", "-e", "con <- url(paste0('http://127.0.0.1:', Sys.getenv('APP_PORT', '8000'), '/health')); on.exit(close(con)); stopifnot(grepl('\"status\":\"ok\"', paste(readLines(con, warn = FALSE), collapse = '')))"]

# Run through the CLI so application logs reach `docker logs`.
CMD ["Rscript", "exec/acstar.R", "serve"]
