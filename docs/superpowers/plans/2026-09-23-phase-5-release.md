# Phase 5 — Release & Cleanup Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A reproducible production Docker image for Helios (with the Rust NIF built in release mode), accurate docs and scripts, a CI `docker` job that smoke-tests the image, and no remaining references to deleted legacy folders.

**Architecture:** `mix phx.gen.release` adds the release scripts (`bin/server`, `bin/migrate`, `Helios.Release`). A multi-stage root `Dockerfile` builds the release in `hexpm/elixir` (plus a rustup toolchain pinned to `mise.toml`), keeping `/app/core` next to `/app/helios` so Rustler's `path: "../core"` resolves, then copies only the release into a matching `debian:*-slim` runtime that runs as `nobody` with SQLite on a `/data` volume. `bin/build.sh` / `bin/run.sh` / `bin/smoke.sh` are the only entry points, and CI uses the same scripts, so what CI checks is exactly what a person runs.

**Tech Stack:** Docker (BuildKit), `hexpm/elixir:1.19.5-erlang-28.5.0.5-debian-trixie-20260824-slim`, `debian:trixie-20260824-slim`, rustup (minimal profile), Elixir releases (`mix release`), Rustler (version pinned in Phase 0; `mode: :release` is its default — verified in Task 1 Step 4), bash + curl, GitHub Actions.

**Spec:** `docs/superpowers/specs/2026-09-23-phase-5-release-design.md` (overview: `docs/superpowers/specs/2026-09-23-migration-overview-design.md`)

## Global Constraints

- Repo parts after this phase: `helios/`, `core/`, `e2e/` (plus `bin/`, `docs/`, root config files). `backend/`, `backend_old/`, `frontend/`, `websocket-client/`, `integration-tests/`, `proto/` are already deleted (Phases 0–4).
- Builder image: `hexpm/elixir:1.19.5-erlang-<28.x>-debian-<release>-<date>`, exact tag pinned, matching `mise.toml` (`elixir = "1.19.5-otp-28"`, `erlang = "28"`). This plan pins `1.19.5-erlang-28.5.0.5-debian-trixie-20260824-slim` (verified on Docker Hub 2026-09-23).
- Runtime image: matching `debian:<release>-slim` → `debian:trixie-20260824-slim` (same Debian release as the builder, so glibc matches ERTS and the NIF).
- Rust in the image: installed via `rustup`, minimal profile, version = the `rust = "…"` value in `mise.toml` (single source of truth; passed as build arg `RUST_VERSION` by `bin/build.sh`).
- Builder apt packages: `build-essential`, `git`, `curl` (+ `ca-certificates`). Runtime apt packages: `libstdc++6 openssl libncurses6 locales ca-certificates`; UTF-8 locale; non-root `nobody`.
- Container layout: `/app/core`, `/app/helios` in the builder; release copied from `_build/prod/rel/helios` to `/app` in the runtime.
- `MIX_ENV=prod`; `ENV PHX_SERVER=true`; `EXPOSE 4000`; `CMD ["/app/bin/server"]`.
- Migrations run at boot via the existing `Ecto.Migrator` child in `helios/lib/helios/application.ex` (active when `RELEASE_NAME` is set). `bin/migrate` exists for manual use.
- Required env: `SECRET_KEY_BASE`, `DATABASE_PATH` (e.g. `/data/helios.db` on a mounted volume), `ACCESS_TOKEN`, `PHX_HOST`; optional `PORT`, `POOL_SIZE`.
- `bin/build.sh`: `docker build -t seven_wonders:$(cat VERSION) .` behaviour. `bin/run.sh`: `docker run -p 4000:4000 -v seven_wonders_data:/data --env-file "$1" seven_wonders:$(cat VERSION)` behaviour.
- CI jobs: exactly `core`, `helios`, `e2e`, `docker`. `docker` builds without pushing and polls `GET /login` for HTTP 200 within 60 s.
- Cleanup check: `grep -rE "backend_old|websocket-client|integration-tests|frontend/|proto/"` outside `docs/` and `MIGRATION.md` returns nothing.
- Commits: explicit `git add <paths>` only; every message ends with `Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>`.
- Never stage the user's untracked personal files or `.DS_Store` files.

## Review Focus

1. **Volume ownership** — a fresh named volume mounted at `/data` must be writable by `nobody`; otherwise SQLite fails at boot. Expected: the image pre-creates `/data` owned by `nobody` so Docker seeds new named volumes with that owner. Pinned by the `stat -c %U /data` check in Task 2 Step 6 and the volume run in Task 3.
2. **Missing required env var** — starting the image without e.g. `ACCESS_TOKEN` must exit non-zero with a message naming the variable, not start with a default. Pinned by Task 3 Step 7.
3. **Graceful stop** — `docker stop` (SIGTERM) must shut the BEAM down cleanly (exit code 0, well before Docker's 10 s SIGKILL) so SQLite WAL writes are not cut off. Pinned by Task 3 Step 8.
4. **Non-localhost Host header** — `config/prod.exs` has `force_ssl` excluding only `localhost`/`127.0.0.1`; any other Host gets a 301 to https. Expected and documented (README: run behind a TLS proxy that sets `X-Forwarded-Proto`); pinned by Task 3 Step 9 so nobody "fixes" it by accident or is surprised by it.
5. **Env-file format** — `docker --env-file` keeps quotes literally and needs `SECRET_KEY_BASE` ≥ 64 bytes; `bin/.env.example` must be usable as-is for a local try (spec: "`bin/run.sh bin/.env.example`-style run"). Pinned by the no-quotes / length check in Task 3 Step 3 and by running the example file in Task 3 Step 5.

---

## File Structure

| Path | Action | Responsibility |
|---|---|---|
| `helios/rel/overlays/bin/server`, `server.bat` | Create (generated) | Start the release with `PHX_SERVER=true` |
| `helios/rel/overlays/bin/migrate`, `migrate.bat` | Create (generated) | Run `Helios.Release.migrate` in a release |
| `helios/lib/helios/release.ex` | Create (generated) | `Helios.Release.migrate/0`, `rollback/2` |
| `helios/lib/helios/core/native.ex` | Modify only if it forces `mode: :debug` | Rustler NIF module; must build `:release` in prod |
| `Dockerfile` | Rewrite | Multi-stage build of the Helios release incl. NIF |
| `.dockerignore` | Rewrite | Keep build context small and free of host artifacts |
| `bin/build.sh` | Rewrite | Build `seven_wonders:<VERSION>` with toolchain versions from `mise.toml` |
| `bin/run.sh` | Rewrite | Run the image with the env file and `/data` volume |
| `bin/smoke.sh` | Create | Wait for `/login` 200, log in via curl, print own lobby path |
| `bin/.env.example` | Create | Documented env file for the image |
| `.github/workflows/ci.yml` | Modify | Add `docker` job; ensure only `core`/`helios`/`e2e`/`docker` |
| `README.md` | Rewrite | Project, architecture, setup, tests, Docker, env vars |
| `core/README.md` | Rewrite | Replace stale "NIF for Elixir.Core" text |
| `MIGRATION.md` | Modify | Prepend completed-status note |
| `.gitignore` | Modify | Drop lines for deleted folders |

---

### Task 1: Release scaffolding in Helios (`mix phx.gen.release`, NIF in release mode)

**Files:**
- Create: `helios/rel/overlays/bin/server`, `helios/rel/overlays/bin/server.bat`, `helios/rel/overlays/bin/migrate`, `helios/rel/overlays/bin/migrate.bat`, `helios/lib/helios/release.ex` (all generated)
- Modify (conditional, Step 4): `helios/lib/helios/core/native.ex`

**Interfaces:**
- Consumes: `Helios.Core.Native` (`use Rustler, otp_app: :helios, crate: "seven_wonders_core", path: "../core"`) from Phase 0; `Helios.Core.game_settings/0` returning `%{engine_version: 1, …}` from Phase 3; `Ecto.Migrator` child in `Helios.Application`.
- Produces: release named `helios` at `helios/_build/prod/rel/helios` with `bin/server` (starts with `PHX_SERVER=true`), `bin/migrate` (runs `Helios.Release.migrate/0`), `bin/helios` (standard release script: `start`, `eval`, `rpc`, `remote`), and `lib/helios-0.1.0/priv/native/libseven_wonders_core.so`. Task 2 copies this release into the image and runs `/app/bin/server`.

All commands in this task run from `helios/` (prefix with `mise exec --` if mise is not activated in your shell).

- [ ] **Step 1: Show the release scripts are missing (failing check)**

Run:
```bash
cd helios
MIX_ENV=prod mix release --overwrite
ls _build/prod/rel/helios/bin/server _build/prod/rel/helios/bin/migrate
```
Expected: the release builds (first prod compile takes several minutes, includes `Compiling crate seven_wonders_core in release mode (../core)`), but `ls` fails with `No such file or directory` for both `bin/server` and `bin/migrate`.

- [ ] **Step 2: Generate the release files**

Run:
```bash
cd helios
mix phx.gen.release
```
Expected output lists these created files (answer `y` if asked to overwrite — nothing should exist yet):
```
* creating rel/overlays/bin/server
* creating rel/overlays/bin/server.bat
* creating rel/overlays/bin/migrate
* creating rel/overlays/bin/migrate.bat
* creating lib/helios/release.ex
```
Do **not** pass `--docker`; the root `Dockerfile` (Task 2) is hand-written because the build context is the repo root, not `helios/`.

- [ ] **Step 3: Verify the generated content**

Run:
```bash
cd helios
cat rel/overlays/bin/server rel/overlays/bin/migrate
ls -l rel/overlays/bin
grep -n "def migrate\|def rollback\|@app :helios" lib/helios/release.ex
```
Expected — `rel/overlays/bin/server`:
```sh
#!/bin/sh
set -eu

cd -P -- "$(dirname -- "$0")"
PHX_SERVER=true exec ./helios start
```
`rel/overlays/bin/migrate`:
```sh
#!/bin/sh
set -eu

cd -P -- "$(dirname -- "$0")"
exec ./helios eval Helios.Release.migrate
```
All four files in `rel/overlays/bin` are executable (`-rwxr-xr-x`). `lib/helios/release.ex` defines `defmodule Helios.Release` with `@app :helios`, `def migrate do`, `def rollback(repo, version) do`. Leave the generated code unchanged.

- [ ] **Step 4: Make sure the NIF is compiled in release mode for prod**

Rustler's default (checked in 0.36.2; re-check for the version Phase 0 pinned by reading `deps/rustler/lib/rustler/compiler/config.ex`) is `mode: :release` (see `Rustler.Compiler.Config`, `mode: :release` in the defaults), so no option is needed. Only a Phase 0 override to `:debug` would break that. Run:
```bash
grep -n "mode:" helios/lib/helios/core/native.ex || echo "no mode option (Rustler default :release)"
```
- If it prints `no mode option …` → change nothing.
- If it prints `mode: :debug` (unconditional) → replace that option so the `use` line reads:
```elixir
  use Rustler,
    otp_app: :helios,
    crate: "seven_wonders_core",
    path: "../core",
    mode: if(Mix.env() == :prod, do: :release, else: :debug)
```

- [ ] **Step 5: Build the release and verify scripts + NIF are inside**

Run:
```bash
cd helios
MIX_ENV=prod mix compile 2>&1 | grep "Compiling crate" || true
MIX_ENV=prod mix release --overwrite
ls _build/prod/rel/helios/bin
ls _build/prod/rel/helios/lib/helios-0.1.0/priv/native
```
Expected: if the crate is recompiled, the line reads `Compiling crate seven_wonders_core in release mode (../core)` (no line = already up to date from Step 1, which also said `release`). `bin` lists `helios  helios.bat  migrate  migrate.bat  server  server.bat`. `priv/native` lists `libseven_wonders_core.so`.

- [ ] **Step 6: Run `bin/migrate` and load the NIF from the release**

Run:
```bash
cd helios
export DATABASE_PATH=/tmp/sw_release_check.db
export SECRET_KEY_BASE="$(mix phx.gen.secret)"
export ACCESS_TOKEN=release-check
export PHX_HOST=localhost
rm -f /tmp/sw_release_check.db*
_build/prod/rel/helios/bin/migrate; echo "migrate exit=$?"
_build/prod/rel/helios/bin/helios eval 'Application.load(:helios); IO.puts(Helios.Core.game_settings().engine_version)'
rm -f /tmp/sw_release_check.db*
```
Expected: `migrate` logs `[info] == Running … Helios.Repo.Migrations.CreateUsers.change/0 forward` (and the other migrations) and prints `migrate exit=0`; the eval prints `1` (the NIF loaded from the release's `priv/native`).

- [ ] **Step 7: Run the Helios checks**

Run:
```bash
cd helios
mix precommit
```
Expected: compile with no warnings, `mix format` leaves no diff (`git status --short helios` shows only the new files from Step 2 and, if applicable, `native.ex`), all tests pass.

- [ ] **Step 8: Commit**

```bash
git add helios/rel/overlays/bin/server helios/rel/overlays/bin/server.bat \
        helios/rel/overlays/bin/migrate helios/rel/overlays/bin/migrate.bat \
        helios/lib/helios/release.ex
# only if Step 4 changed it:
git add helios/lib/helios/core/native.ex
git commit -m "$(cat <<'EOF'
Add Helios release scripts via mix phx.gen.release

bin/server starts the release with PHX_SERVER=true; bin/migrate runs
Helios.Release.migrate. The Rustler NIF builds in release mode for prod.

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Multi-stage Dockerfile, `.dockerignore`, `bin/build.sh`

**Files:**
- Rewrite: `Dockerfile`
- Rewrite: `.dockerignore`
- Rewrite: `bin/build.sh`

**Interfaces:**
- Consumes: Task 1's `helios/rel/` overlays and release name `helios`; `mise.toml` keys `elixir = "1.19.5-otp-28"`, `erlang = "28"`, `rust = "<x.y.z>"`; `VERSION` (currently `0.1.0`).
- Produces: image `seven_wonders:<VERSION>` whose default command `/app/bin/server` listens on port 4000, runs as `nobody`, with `/data` pre-created and owned by `nobody`. `bin/build.sh [extra docker build args…]` (exit non-zero if `mise.toml` and `Dockerfile` disagree). Tasks 3 and 4 depend on this image tag and script.

- [ ] **Step 1: Show the current build is broken (failing check)**

Run from the repo root:
```bash
bin/build.sh
```
Expected: FAIL — the old Dockerfile does `COPY backend .` and Docker reports `"/backend": not found`.

- [ ] **Step 2: Write `Dockerfile`**

Replace the whole file with:
```dockerfile
# syntax=docker/dockerfile:1
#
# Production image for Helios (Phoenix) with the seven_wonders_core Rust NIF.
# Build with bin/build.sh, which passes RUST_VERSION from mise.toml and checks
# that ELIXIR_VERSION / OTP_VERSION below match mise.toml.
#
# Image tags: https://hub.docker.com/r/hexpm/elixir/tags?name=1.19.5-erlang-28
# Builder and runner must use the same Debian release (glibc for ERTS + NIF).

ARG ELIXIR_VERSION=1.19.5
ARG OTP_VERSION=28.5.0.5
ARG DEBIAN_VERSION=trixie-20260824-slim

ARG BUILDER_IMAGE="docker.io/hexpm/elixir:${ELIXIR_VERSION}-erlang-${OTP_VERSION}-debian-${DEBIAN_VERSION}"
ARG RUNNER_IMAGE="docker.io/debian:${DEBIAN_VERSION}"

# ---------------------------------------------------------------------------
FROM ${BUILDER_IMAGE} AS builder

ARG RUST_VERSION
RUN test -n "${RUST_VERSION}" \
  || (echo "RUST_VERSION build arg is required - build with bin/build.sh" >&2 && exit 1)

RUN apt-get update \
  && apt-get install -y --no-install-recommends build-essential git curl ca-certificates \
  && rm -rf /var/lib/apt/lists/*

# Rust toolchain pinned to mise.toml (Rustler compiles ../core during mix compile)
ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
    | sh -s -- -y --no-modify-path --profile minimal --default-toolchain "${RUST_VERSION}" \
  && rustc --version \
  && cargo --version

# Helios.Core.Native uses `path: "../core"`, so keep /app/helios next to /app/core
WORKDIR /app/helios

RUN mix local.hex --force \
  && mix local.rebar --force

ENV MIX_ENV="prod"

# Dependencies first (cached until mix.exs / mix.lock change)
COPY helios/mix.exs helios/mix.lock ./
RUN mix deps.get --only $MIX_ENV
RUN mkdir config

# Compile-time config before deps.compile so config changes recompile deps
COPY helios/config/config.exs helios/config/prod.exs config/
RUN mix deps.compile

RUN mix assets.setup

# Rust engine (compiled by Rustler in release mode during `mix compile`)
COPY core /app/core

COPY helios/priv priv
COPY helios/lib lib

# Compile before assets.deploy: esbuild imports phoenix-colocated hooks from _build
RUN mix compile

COPY helios/assets assets
RUN mix assets.deploy

# runtime.exs changes don't require recompiling
COPY helios/config/runtime.exs config/

COPY helios/rel rel
RUN mix release

# ---------------------------------------------------------------------------
FROM ${RUNNER_IMAGE} AS final

RUN apt-get update \
  && apt-get install -y --no-install-recommends libstdc++6 openssl libncurses6 locales ca-certificates \
  && rm -rf /var/lib/apt/lists/*

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen \
  && locale-gen

ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8

WORKDIR "/app"

# /data holds the SQLite database (DATABASE_PATH=/data/helios.db). Creating it
# here owned by nobody makes Docker seed new named volumes with that owner.
RUN chown nobody /app \
  && mkdir -p /data \
  && chown nobody:nogroup /data

ENV MIX_ENV="prod" \
    PHX_SERVER=true

COPY --from=builder --chown=nobody:root /app/helios/_build/prod/rel/helios ./

USER nobody

EXPOSE 4000

CMD ["/app/bin/server"]
```

- [ ] **Step 3: Write `.dockerignore`**

Replace the whole file with:
```
# VCS / CI / docs
.git
.github
docs/
MIGRATION.md

# Not part of the image
e2e/

# Build outputs and dependencies (rebuilt inside the image)
**/_build
**/deps
**/node_modules
core/target
helios/priv/native
helios/priv/static/assets
helios/priv/static/cache_manifest.json

# Local databases and crash dumps
**/*.db*
**/erl_crash.dump

# Secrets
**/.env

# Editor / OS noise
**/.DS_Store
.idea
.vscode
**/.elixir_ls
.dexter
```

- [ ] **Step 4: Write `bin/build.sh`**

Replace the whole file with:
```bash
#!/usr/bin/env bash
# Build the production image seven_wonders:<VERSION> from the repo root.
# Toolchain versions come from mise.toml so the image matches local dev and CI.
# Extra arguments are passed to `docker build` (e.g. --no-cache, --progress=plain).
set -eo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
version="$(tr -d '[:space:]' < "$root/VERSION")"
rust_version="$(sed -n 's/^rust *= *"\(.*\)"$/\1/p' "$root/mise.toml")"
elixir_version="$(sed -n 's/^elixir *= *"\([0-9.]*\)-otp-.*"$/\1/p' "$root/mise.toml")"
erlang_major="$(sed -n 's/^erlang *= *"\([0-9]*\).*"$/\1/p' "$root/mise.toml")"

if [ -z "$rust_version" ] || [ -z "$elixir_version" ] || [ -z "$erlang_major" ]; then
  echo "build.sh: could not read rust/elixir/erlang versions from mise.toml" >&2
  exit 1
fi
if ! grep -q "^ARG ELIXIR_VERSION=${elixir_version}\$" "$root/Dockerfile"; then
  echo "build.sh: Dockerfile ELIXIR_VERSION does not match mise.toml (${elixir_version})" >&2
  exit 1
fi
if ! grep -q "^ARG OTP_VERSION=${erlang_major}\." "$root/Dockerfile"; then
  echo "build.sh: Dockerfile OTP_VERSION is not OTP ${erlang_major} (mise.toml)" >&2
  exit 1
fi

echo "build.sh: seven_wonders:${version} (elixir ${elixir_version}, otp ${erlang_major}, rust ${rust_version})" >&2

docker build \
  --build-arg "RUST_VERSION=${rust_version}" \
  -t "seven_wonders:${version}" \
  "$@" \
  "$root"
```
Keep it executable: `chmod +x bin/build.sh` (git already tracks mode 100755).

- [ ] **Step 5: Build the image**

Run:
```bash
bin/build.sh --progress=plain 2>&1 | tee /tmp/sw_build.log | tail -n 5
grep -E "Compiling crate seven_wonders_core in release mode|Release created" /tmp/sw_build.log
docker image ls seven_wonders
```
Expected: first line of output `build.sh: seven_wonders:0.1.0 (elixir 1.19.5, otp 28, rust <mise rust version>)`; the build ends with `naming to docker.io/library/seven_wonders:0.1.0`; the grep shows both `Compiling crate seven_wonders_core in release mode (../core)` and `* assembling helios-0.1.0 on MIX_ENV=prod` / `Release created at _build/prod/rel/helios`; `docker image ls` lists tag `0.1.0`. First build takes 10–20 minutes.

- [ ] **Step 6: Inspect the image contents (user, /data ownership, scripts, NIF)**

Run:
```bash
docker run --rm --entrypoint sh seven_wonders:0.1.0 -c \
  'id -un; stat -c %U /data; ls /app/bin; ls /app/lib/helios-*/priv/native; echo "PHX_SERVER=$PHX_SERVER"'
```
Expected output:
```
nobody
nobody
helios
helios.bat
migrate
migrate.bat
server
server.bat
libseven_wonders_core.so
PHX_SERVER=true
```

- [ ] **Step 7: Verify the mise.toml consistency guard**

Run:
```bash
sed -i.bak 's/^ARG ELIXIR_VERSION=.*/ARG ELIXIR_VERSION=1.18.0/' Dockerfile
bin/build.sh; echo "exit=$?"
mv Dockerfile.bak Dockerfile
git diff --stat Dockerfile
```
Expected: `build.sh: Dockerfile ELIXIR_VERSION does not match mise.toml (1.19.5)` and `exit=1`; after restoring, `git diff --stat Dockerfile` shows only the Step 2 rewrite (compare with `grep '^ARG ELIXIR_VERSION=1.19.5$' Dockerfile` → one line).

- [ ] **Step 8: Commit**

```bash
git add Dockerfile .dockerignore bin/build.sh
git commit -m "$(cat <<'EOF'
Build the Helios release in a multi-stage Docker image

hexpm/elixir 1.19.5 / OTP 28.5.0.5 on Debian trixie builds the release
(Rust via rustup pinned to mise.toml, NIF in release mode); the runtime is
debian trixie-slim as nobody with /data for SQLite. build.sh reads the
toolchain versions from mise.toml and refuses a mismatched Dockerfile.

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: `bin/run.sh`, `bin/.env.example`, `bin/smoke.sh` — run, log in, persist

**Files:**
- Rewrite: `bin/run.sh`
- Create: `bin/.env.example`
- Create: `bin/smoke.sh`

**Interfaces:**
- Consumes: image `seven_wonders:<VERSION>` (Task 2). Phase 1/2 HTTP contract: `GET /login` (LiveView page with `<meta name="csrf-token" content="…">`); `POST /session` with form params `_csrf_token`, `login[access_token]`, `login[name]` → `302 Location: /` on success, `302 Location: /login` on error; `GET /` for a logged-in user → `302 Location: /lobby/<uuid>` (own lobby, stable per user).
- Produces:
  - `bin/run.sh <env-file> [extra docker run args…]` — runs `seven_wonders:<VERSION>` with `--rm -p 4000:4000 -v ${SEVEN_WONDERS_VOLUME:-seven_wonders_data}:/data --env-file <env-file>`; adds `-it` only when stdin/stdout are a TTY.
  - `bin/smoke.sh <base-url> <access-token> <name>` — exit 0 and prints exactly one line `/lobby/<uuid>` on stdout (progress on stderr); exit 1 with a message if `/login` is not 200 within 60 s or login fails.
  - `bin/.env.example` — valid `--env-file` with `SECRET_KEY_BASE`, `DATABASE_PATH=/data/helios.db`, `ACCESS_TOKEN`, `PHX_HOST=localhost`.
  Task 4 (CI) calls `bin/run.sh` and `bin/smoke.sh` with these exact signatures.

Before starting: nothing else may listen on port 4000 (`lsof -i :4000` prints nothing — stop any `mix phx.server`).

- [ ] **Step 1: Show the smoke check fails today (failing check)**

Run:
```bash
bin/smoke.sh http://localhost:4000 change-me smoke; echo "exit=$?"
```
Expected: `bin/smoke.sh: No such file or directory` and `exit=127`.

- [ ] **Step 2: Write `bin/.env.example`**

```
# Environment for the seven_wonders production image: bin/run.sh <this file>
# Docker --env-file format: KEY=value, one per line, no quotes, no `export`,
# no inline comments. Copy to bin/.env (git-ignored) and change every value
# before real use; the values below only work for a local try.

# Signs/encrypts session cookies; must be at least 64 bytes.
# Generate: cd helios && mix phx.gen.secret   (or: openssl rand -base64 64 | tr -d '\n')
SECRET_KEY_BASE=replace-me-with-output-of-mix-phx-gen-secret-this-placeholder-is-only-for-local-tries

# SQLite database file. /data is the volume mounted by bin/run.sh.
DATABASE_PATH=/data/helios.db

# Shared token every player types on the login page.
ACCESS_TOKEN=change-me

# Public host name: used for URL generation and the LiveView websocket origin check.
PHX_HOST=localhost

# Optional
# PORT=4000       (if changed, also change the -p mapping in bin/run.sh)
# POOL_SIZE=5
```

- [ ] **Step 3: Check the example file format**

Run:
```bash
! grep -n '"' bin/.env.example && echo "no quotes: ok"
awk -F= '/^SECRET_KEY_BASE=/{ print (length($2) >= 64 ? "secret length ok" : "secret too short") }' bin/.env.example
grep -E '^(SECRET_KEY_BASE|DATABASE_PATH|ACCESS_TOKEN|PHX_HOST)=' bin/.env.example | cut -d= -f1
git check-ignore -q bin/.env.example && echo "IGNORED (bad)" || echo "tracked: ok"
```
Expected: `no quotes: ok`, `secret length ok`, the four variable names, `tracked: ok`.

- [ ] **Step 4: Write `bin/run.sh` and `bin/smoke.sh`**

`bin/run.sh` (replace the whole file):
```bash
#!/usr/bin/env bash
# Run the production image with a persistent SQLite volume on /data.
# Usage: bin/run.sh <env-file> [extra docker run args...]
#   bin/run.sh bin/.env
#   bin/run.sh bin/.env -d --name seven_wonders
# Volume name: seven_wonders_data (override with SEVEN_WONDERS_VOLUME).
set -eo pipefail

if [ -z "$1" ]; then
  echo "usage: bin/run.sh <env-file> [docker run args...]   (see bin/.env.example)" >&2
  exit 1
fi
env_file="$1"
shift

root="$(cd "$(dirname "$0")/.." && pwd)"
version="$(tr -d '[:space:]' < "$root/VERSION")"
volume="${SEVEN_WONDERS_VOLUME:-seven_wonders_data}"

# Interactive terminal: allow Ctrl-C / colored logs. CI has no TTY.
if [ -t 0 ] && [ -t 1 ]; then
  set -- -it "$@"
fi

exec docker run --rm \
  -p 4000:4000 \
  -v "${volume}:/data" \
  --env-file "$env_file" \
  "$@" \
  "seven_wonders:${version}"
```

`bin/smoke.sh` (new):
```bash
#!/usr/bin/env bash
# Smoke-test a running Helios: wait for GET /login -> 200 (max 60 s), log in via
# POST /session, and print the user's own lobby path (/lobby/<uuid>) on stdout.
# The lobby path is stable for a name, so comparing it before and after replacing
# the container proves the SQLite volume persisted.
# Usage: bin/smoke.sh <base-url> <access-token> <name>
#   bin/smoke.sh http://localhost:4000 change-me smoke
set -eo pipefail

base_url="$1"
access_token="$2"
name="$3"
if [ -z "$base_url" ] || [ -z "$access_token" ] || [ -z "$name" ]; then
  echo "usage: bin/smoke.sh <base-url> <access-token> <name>" >&2
  exit 1
fi

jar="$(mktemp)"
trap 'rm -f "$jar"' EXIT

status=""
for _ in $(seq 1 60); do
  status="$(curl -s -m 5 -o /dev/null -w '%{http_code}' "$base_url/login" || true)"
  if [ "$status" = "200" ]; then
    break
  fi
  sleep 1
done
if [ "$status" != "200" ]; then
  echo "smoke: GET /login did not return 200 within 60 s (last status: ${status:-none})" >&2
  exit 1
fi
echo "smoke: GET /login -> 200" >&2

csrf="$(curl -s -c "$jar" -b "$jar" "$base_url/login" \
  | sed -n 's/.*<meta name="csrf-token" content="\([^"]*\)".*/\1/p' | head -n 1)"
if [ -z "$csrf" ]; then
  echo "smoke: no csrf-token meta tag on /login" >&2
  exit 1
fi

login="$(curl -s -o /dev/null -c "$jar" -b "$jar" -w '%{http_code} %{redirect_url}' \
  --data-urlencode "_csrf_token=${csrf}" \
  --data-urlencode "login[access_token]=${access_token}" \
  --data-urlencode "login[name]=${name}" \
  "$base_url/session")"
if [ "$login" != "302 ${base_url}/" ]; then
  echo "smoke: POST /session expected '302 ${base_url}/', got '${login}'" >&2
  exit 1
fi
echo "smoke: POST /session -> logged in as ${name}" >&2

lobby_url="$(curl -s -o /dev/null -b "$jar" -w '%{redirect_url}' "$base_url/")"
case "$lobby_url" in
  "${base_url}/lobby/"*)
    echo "/lobby/${lobby_url##*/lobby/}"
    ;;
  *)
    echo "smoke: GET / expected a redirect to /lobby/<id>, got '${lobby_url}'" >&2
    exit 1
    ;;
esac
```
Then:
```bash
chmod +x bin/run.sh bin/smoke.sh
```

- [ ] **Step 5: Run the image with the example env file and a fresh volume; smoke + login**

Run:
```bash
export SEVEN_WONDERS_VOLUME=sw_verify_$$
bin/run.sh bin/.env.example -d --name sw_verify_1
bin/smoke.sh http://localhost:4000 change-me smoke | tee /tmp/sw_lobby_1
docker exec sw_verify_1 ls -l /data
docker exec sw_verify_1 /app/bin/helios rpc 'IO.puts(Helios.Repo.aggregate(Helios.Accounts.User, :count))'
docker exec sw_verify_1 /app/bin/migrate; echo "migrate exit=$?"
```
Expected: stderr `smoke: GET /login -> 200` and `smoke: POST /session -> logged in as smoke`; stdout one line `/lobby/<uuid>`; `/data` contains `helios.db` (plus `-shm`/`-wal`) owned by `nobody`; the rpc prints `1`; `bin/migrate` exits 0 without running any migration (all already applied at boot by the `Ecto.Migrator` child).

Also check a wrong token is rejected:
```bash
bin/smoke.sh http://localhost:4000 wrong-token smoke-bad; echo "exit=$?"
```
Expected: `smoke: POST /session expected '302 http://localhost:4000/', got '302 http://localhost:4000/login'` and `exit=1`.

- [ ] **Step 6: Replace the container (same volume) and verify persistence**

Run:
```bash
docker stop sw_verify_1
bin/run.sh bin/.env.example -d --name sw_verify_2
bin/smoke.sh http://localhost:4000 change-me smoke > /tmp/sw_lobby_2
diff /tmp/sw_lobby_1 /tmp/sw_lobby_2 && echo "lobby persisted: $(cat /tmp/sw_lobby_2)"
docker exec sw_verify_2 /app/bin/helios rpc 'IO.puts(Helios.Repo.aggregate(Helios.Accounts.User, :count))'
```
Expected: `lobby persisted: /lobby/<same uuid as Step 5>`; the rpc still prints `1` (same user re-entered, not recreated).

Manual browser check (LiveView websocket + `PHX_HOST` origin check, which curl does not exercise): open `http://localhost:4000`, log in with access token `change-me` and a new name. Expected: you land on `/lobby/<uuid>`, the header shows your name, and no "Attempting to reconnect" / disconnected flash appears within 10 s. Log out.

Clean up:
```bash
docker stop sw_verify_2
docker volume rm "$SEVEN_WONDERS_VOLUME"
unset SEVEN_WONDERS_VOLUME
```

- [ ] **Step 7: Missing required env var fails loudly (Review Focus 2)**

Run:
```bash
grep -v '^ACCESS_TOKEN=' bin/.env.example > /tmp/sw_no_token.env
docker run --rm --env-file /tmp/sw_no_token.env seven_wonders:0.1.0 > /tmp/sw_no_token.log 2>&1; echo "exit=$?"
grep -c ACCESS_TOKEN /tmp/sw_no_token.log
```
Expected: `exit=` a non-zero code (typically 1) within a few seconds, and the grep count ≥ 1 (the log contains `ERROR! Config provider Config.Reader failed with:` and the Phase 1 `ACCESS_TOKEN is missing` message).

- [ ] **Step 8: Graceful stop on SIGTERM (Review Focus 3)**

Run (no `--rm`, so the exit code can be inspected; no volume, which also proves `/data` is writable in the image itself):
```bash
docker run -d --name sw_sigterm -p 4000:4000 --env-file bin/.env.example seven_wonders:0.1.0
bin/smoke.sh http://localhost:4000 change-me sigterm > /dev/null
time docker stop sw_sigterm
docker inspect -f '{{.State.ExitCode}}' sw_sigterm
docker rm sw_sigterm
```
Expected: `docker stop` returns in well under 10 s (`real` ≈ 1–3 s) and the exit code is `0`.

- [ ] **Step 9: Non-localhost Host is redirected to https (Review Focus 4)**

Run:
```bash
SEVEN_WONDERS_VOLUME=sw_host_check bin/run.sh bin/.env.example -d --name sw_host
bin/smoke.sh http://localhost:4000 change-me hostcheck > /dev/null
curl -s -o /dev/null -w '%{http_code} %{redirect_url}\n' -H 'Host: seven-wonders.example' http://localhost:4000/login
curl -s -o /dev/null -w '%{http_code}\n' -H 'Host: seven-wonders.example' -H 'X-Forwarded-Proto: https' http://localhost:4000/login
docker stop sw_host
docker volume rm sw_host_check
```
Expected: first curl `301 https://seven-wonders.example/login`; second curl `200` (what a TLS-terminating proxy sends). This is intended `force_ssl` behaviour, documented in the README (Task 5).

- [ ] **Step 10: Commit**

```bash
git add bin/run.sh bin/smoke.sh bin/.env.example
git commit -m "$(cat <<'EOF'
Add run/smoke scripts and env example for the Docker image

run.sh mounts the seven_wonders_data volume on /data and maps port 4000;
smoke.sh waits for /login, logs in with curl and prints the user's lobby
path so persistence can be checked across container replacements.

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: CI `docker` job with smoke and persistence test

**Files:**
- Modify: `.github/workflows/ci.yml`

**Interfaces:**
- Consumes: `bin/build.sh [docker build args…]` (Task 2); `bin/run.sh <env-file> [docker run args…]` honouring `SEVEN_WONDERS_VOLUME`, and `bin/smoke.sh <base-url> <access-token> <name>` printing `/lobby/<uuid>` (Task 3). Existing jobs `core`, `helios`, `e2e` from Phases 0–2.
- Produces: final workflow with jobs exactly `core`, `helios`, `e2e`, `docker`.

- [ ] **Step 1: Check the current job list (failing check)**

Run:
```bash
ruby -ryaml -e 'puts YAML.load_file(".github/workflows/ci.yml")["jobs"].keys'
```
Expected: `core`, `helios`, `e2e` — no `docker` (fails the target of exactly four jobs). If any other job is listed (e.g. `old_backend`, `frontend`, `integration-tests`, `backend` left over from earlier phases), delete that job block entirely in Step 2 — their folders no longer exist.

- [ ] **Step 2: Append the `docker` job**

Leave the `core`, `helios`, `e2e` job blocks unchanged. At the end of the `jobs:` mapping (same two-space indentation as `core:`), add:
```yaml
  docker:
    name: Docker
    runs-on: ubuntu-latest
    env:
      SEVEN_WONDERS_VOLUME: seven_wonders_ci_${{ github.run_id }}
    steps:
      - uses: actions/checkout@v4
      - name: Build image (no push)
        run: bin/build.sh --progress=plain
      - name: Write smoke-test env file
        run: |
          {
            echo "SECRET_KEY_BASE=$(openssl rand -base64 64 | tr -d '\n')"
            echo "DATABASE_PATH=/data/helios.db"
            echo "ACCESS_TOKEN=ci-smoke-token"
            echo "PHX_HOST=localhost"
          } > "$RUNNER_TEMP/smoke.env"
      - name: Start container
        run: bin/run.sh "$RUNNER_TEMP/smoke.env" -d --name sw_smoke_1
      - name: Smoke test (GET /login 200 within 60 s, log in)
        run: bin/smoke.sh http://localhost:4000 ci-smoke-token smoke > "$RUNNER_TEMP/lobby_1"
      - name: Replace container and verify the volume persisted
        run: |
          docker stop sw_smoke_1
          bin/run.sh "$RUNNER_TEMP/smoke.env" -d --name sw_smoke_2
          bin/smoke.sh http://localhost:4000 ci-smoke-token smoke > "$RUNNER_TEMP/lobby_2"
          echo "before: $(cat "$RUNNER_TEMP/lobby_1")  after: $(cat "$RUNNER_TEMP/lobby_2")"
          diff "$RUNNER_TEMP/lobby_1" "$RUNNER_TEMP/lobby_2"
      - name: Container logs
        if: failure()
        run: |
          docker ps -a
          docker logs sw_smoke_2 || docker logs sw_smoke_1 || true
      - name: Clean up
        if: always()
        run: |
          docker rm -f sw_smoke_1 sw_smoke_2 || true
          docker volume rm "$SEVEN_WONDERS_VOLUME" || true
```

- [ ] **Step 3: Validate the workflow**

Run:
```bash
ruby -ryaml -e 'puts YAML.load_file(".github/workflows/ci.yml")["jobs"].keys'
command -v actionlint >/dev/null && actionlint .github/workflows/ci.yml || echo "actionlint not installed; YAML parse above is the check"
```
Expected: exactly
```
core
helios
e2e
docker
```
and, if installed, `actionlint` prints nothing. (If `ruby` is unavailable: `python3 -c 'import yaml,sys; print(*yaml.safe_load(open(".github/workflows/ci.yml"))["jobs"], sep="\n")'`.)

- [ ] **Step 4: Dry-run the job's shell steps locally**

Run from the repo root (reuses the image from Task 2; mirrors the workflow steps):
```bash
export SEVEN_WONDERS_VOLUME=seven_wonders_ci_local RUNNER_TEMP="$(mktemp -d)"
{
  echo "SECRET_KEY_BASE=$(openssl rand -base64 64 | tr -d '\n')"
  echo "DATABASE_PATH=/data/helios.db"
  echo "ACCESS_TOKEN=ci-smoke-token"
  echo "PHX_HOST=localhost"
} > "$RUNNER_TEMP/smoke.env"
bin/run.sh "$RUNNER_TEMP/smoke.env" -d --name sw_smoke_1 < /dev/null
bin/smoke.sh http://localhost:4000 ci-smoke-token smoke > "$RUNNER_TEMP/lobby_1"
docker stop sw_smoke_1
bin/run.sh "$RUNNER_TEMP/smoke.env" -d --name sw_smoke_2 < /dev/null
bin/smoke.sh http://localhost:4000 ci-smoke-token smoke > "$RUNNER_TEMP/lobby_2"
diff "$RUNNER_TEMP/lobby_1" "$RUNNER_TEMP/lobby_2" && echo "persisted"
docker rm -f sw_smoke_1 sw_smoke_2; docker volume rm "$SEVEN_WONDERS_VOLUME"
unset SEVEN_WONDERS_VOLUME RUNNER_TEMP
```
Expected: `persisted`. (`< /dev/null` makes `run.sh` behave as in CI — no `-it`.)

- [ ] **Step 5: Commit**

```bash
git add .github/workflows/ci.yml
git commit -m "$(cat <<'EOF'
Add Docker build and smoke-test job to CI

Builds the image with bin/build.sh (no push), starts it with a temp volume,
waits for GET /login 200 within 60 s, logs in, then replaces the container
and checks the user's lobby survived.

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 5: Rewrite `README.md` and verify it from a clean clone

**Files:**
- Rewrite: `README.md`

**Interfaces:**
- Consumes: `mise.toml` (erlang, elixir, rust, node); `helios` aliases `setup`, `precommit`; `e2e` scripts (Playwright config starts `MIX_ENV=e2e` Helios on port 4004 with access token `e2e`); dev `access_token: "dev"`; `bin/build.sh`, `bin/run.sh`, `bin/smoke.sh`, `bin/.env.example` (Tasks 2–3).
- Produces: README whose commands are exercised verbatim in Step 3.

- [ ] **Step 1: Confirm the README is stale (failing check)**

Run:
```bash
grep -nE "Go 1\.14|Elm|Heroku|protobuf|gen_proto|backend|websocket-client" README.md | head
```
Expected: several hits (Go/Elm prerequisites, Heroku badge, protobuf sections).

- [ ] **Step 2: Write `README.md`**

Replace the whole file with:
````markdown
# Seven Wonders — digital edition

[![CI](https://github.com/Szetty/seven_wonders/actions/workflows/ci.yml/badge.svg)](https://github.com/Szetty/seven_wonders/actions/workflows/ci.yml)

A browser version of the 7 Wonders board game: the full base game for 3–7 players,
all wonder abilities, no expansions. Players log in with a shared access token and
a name, gather at a table (lobby), invite each other and play in real time.

## Architecture

| Folder | What it is |
|---|---|
| `helios/` | Phoenix 1.8 / LiveView 1.1 app: UI, auth, lobby, game orchestration, persistence (SQLite via `ecto_sqlite3`). |
| `core/` | Rust crate `seven_wonders_core`: the rules engine, loaded into Helios as a Rustler NIF. It owns all game rules. |
| `e2e/` | Playwright end-to-end tests that drive a running Helios. No application code. |

```
e2e/ (Playwright) ──HTTP/WS──▶ helios (Phoenix 1.8)
                                ├── LiveViews: LoginLive, LobbyLive, GameLive
                                ├── Accounts (users, user_tokens)  ─┐
                                ├── Lobbies (lobbies, lobby_invites) ├─ Ecto / SQLite
                                ├── Games (games, game_players,     ─┘
                                │          game_actions)
                                ├── Presence: users:online, lobby:<id>, game:<id>
                                ├── PubSub: user:<id>, lobby:<id>, game:<id>
                                ├── GameServer (DynamicSupervisor + Registry), one per game
                                └── Helios.Core ─▶ Helios.Core.Native (Rustler NIF)
                                                     └── core/ (seven_wonders_core crate)
```

A game is stored as its seed plus the log of accepted actions. The engine is
deterministic, so Helios rebuilds any game after a restart by replaying the log.

## Prerequisites

- [mise](https://mise.jdx.dev) — installs the pinned Erlang, Elixir, Rust and Node
  versions from `mise.toml`.
- [Docker](https://docs.docker.com/get-docker/) — only for building/running the
  production image.

If mise is not activated in your shell, prefix the commands below with `mise exec --`.

## Setup

```sh
mise trust
mise install
mix local.hex --force
mix local.rebar --force

cd helios
mix setup
mix phx.server
```

`mix setup` fetches dependencies, creates and migrates the SQLite database and builds
the assets. Compiling Helios also compiles the Rust engine through Rustler, so the first
build takes a few minutes.

Open <http://localhost:4000> and log in with any name and access token **`dev`**
(set `ACCESS_TOKEN` to use another one).

## Tests

```sh
# Rust engine
cd core
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test

# Helios (compile without warnings, unused deps, format, ExUnit)
cd helios
mix precommit

# End-to-end (run `mix setup` in helios/ first)
cd e2e
npm ci
npx playwright install chromium
npx playwright test
```

The Playwright config starts Helios itself (`MIX_ENV=e2e`, port 4004, access token
`e2e`, a fresh `helios_e2e.db`). Locally it reuses a server already listening on 4004.

## Docker

```sh
bin/build.sh                      # builds seven_wonders:$(cat VERSION)
cp bin/.env.example bin/.env      # then edit every value
bin/run.sh bin/.env               # serves http://localhost:4000
```

- `bin/build.sh` reads the Elixir/OTP/Rust versions from `mise.toml` and fails if the
  `Dockerfile` builder image does not match. Extra arguments go to `docker build`.
- `bin/run.sh <env-file> [docker run args…]` maps port 4000 and mounts the named volume
  `seven_wonders_data` on `/data` (override the name with `SEVEN_WONDERS_VOLUME`), e.g.
  `bin/run.sh bin/.env -d --name seven_wonders`. Use a named volume: the container
  runs as `nobody`, and a bind-mounted host directory must be writable by that user.
- Database migrations run automatically at boot. To run them by hand:
  `docker exec <container> /app/bin/migrate`. Remote console:
  `docker exec -it <container> /app/bin/helios remote`.
- `bin/smoke.sh http://localhost:4000 <access-token> <name>` waits for the login page,
  logs in with curl and prints that user's lobby path. CI uses it to test the image.

### Environment variables

| Variable | Required | Description |
|---|---|---|
| `SECRET_KEY_BASE` | yes | Signs/encrypts session cookies, at least 64 bytes. Generate with `cd helios && mix phx.gen.secret`. |
| `DATABASE_PATH` | yes | SQLite file, e.g. `/data/helios.db` (on the mounted volume). |
| `ACCESS_TOKEN` | yes | Shared token every player enters on the login page. |
| `PHX_HOST` | yes | Public host name, used for URLs and the LiveView websocket origin check. |
| `PORT` | no | HTTP port inside the container (default `4000`; change the `-p` mapping too). |
| `POOL_SIZE` | no | Database connection pool size (default `5`). |

The env file uses Docker's `--env-file` format: `KEY=value`, no quotes.

### TLS

Production config enforces HTTPS: requests whose `Host` is not `localhost` or
`127.0.0.1` are redirected to `https://`. Run the container behind a TLS-terminating
reverse proxy that sets `X-Forwarded-Proto: https`, with `PHX_HOST` set to the public
host name.

## License

MIT — see [LICENSE](LICENSE).
````

- [ ] **Step 3: Commit, then verify every README command from a clean clone**

Commit first (the clone only sees committed files):
```bash
git add README.md
git commit -m "$(cat <<'EOF'
Rewrite README for the helios/core/e2e repository

Covers architecture, mise-based setup, tests, the Docker scripts and env
vars, and replaces the Heroku badge with the GitHub Actions CI badge.

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>
EOF
)"
```
Then, with nothing listening on ports 4000 and 4004 (`lsof -i :4000 -i :4004` prints nothing):
```bash
rm -rf /tmp/sw-clean
git clone "$(git rev-parse --show-toplevel)" /tmp/sw-clean
cd /tmp/sw-clean
mise trust
mise install
mise exec -- bash -c '
  set -e
  mix local.hex --force
  mix local.rebar --force
  (cd core && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test)
  (cd helios && mix setup && mix precommit)
  (cd e2e && npm ci && npx playwright install chromium && npx playwright test)
'
echo "clean clone exit=$?"
```
Expected: `cargo test` and `mix precommit` pass, the Playwright suite passes, `clean clone exit=0`. Then check the dev server command:
```bash
cd /tmp/sw-clean/helios
mise exec -- mix phx.server &
sleep 20; curl -s -o /dev/null -w '%{http_code}\n' http://localhost:4000/login; kill %1
```
Expected: `200`. If any README command fails, fix `README.md` in the main repo, commit with the same message style, and repeat this step. Finally `rm -rf /tmp/sw-clean`.

---

### Task 6: Reference cleanup, `MIGRATION.md` status, stale docs, stray files

**Files:**
- Modify: `.gitignore`
- Modify: `MIGRATION.md`
- Modify: `AGENTS.md` (root)
- Rewrite: `core/README.md`
- Modify: any other tracked file reported by Step 1 (outside `docs/` and `MIGRATION.md`)

**Interfaces:**
- Consumes: final tree from Tasks 1–5.
- Produces: `git grep -nE "backend_old|websocket-client|integration-tests|frontend/|proto/" -- . ':!docs' ':!MIGRATION.md'` prints nothing.

- [ ] **Step 1: List remaining references (failing check)**

Run:
```bash
git grep -nE "backend_old|websocket-client|integration-tests|frontend/|proto/" -- . ':!docs' ':!MIGRATION.md'
git grep -nE "backend/|JWT_SECRET|[Hh]eroku|[Tt]ravis|elm-app|protobuf|Elixir\.Core\b" -- . ':!docs' ':!MIGRATION.md'
```
Expected: hits including at least these `.gitignore` lines:
```
.gitignore:…:backend/backend
.gitignore:…:backend/main
.gitignore:…:frontend/build
.gitignore:…:frontend/src/style/*.css
.gitignore:…:websocket-client/build
```
and `core/README.md` (`NIF for Elixir.Core`). Note every other hit for Step 4.

- [ ] **Step 2: Clean `.gitignore`**

Delete exactly these lines from the block near the end of `.gitignore` (they refer to deleted folders or an unrelated Haskell artifact):
```
backend/backend
backend/main
frontend/build
frontend/src/style/*.css
websocket-client/build
core.cabal
```
Keep `**/.env`, `*.iml`, and the Phase 0 additions (`.DS_Store`, `erl_crash.dump`).

- [ ] **Step 3: Replace `core/README.md`**

Skip this step only if `grep -nE 'Elixir\.Core|crate: "core"|rustler_crates' core/README.md` prints nothing (Phase 0 already rewrote it). Otherwise replace the whole file with:
````markdown
# seven_wonders_core

The 7 Wonders rules engine (base game, 3–7 players, all wonder abilities), written in
Rust. It owns every game rule: dealing, legality, payments, hand passing, battles, ages
and scoring. The engine is deterministic (seeded `ChaCha8Rng`), so a game can be rebuilt
from its seed and action log.

Helios loads it as a Rustler NIF (`Helios.Core.Native`, `path: "../core"`); `mix compile`
in `helios/` builds this crate automatically. The NIF boundary uses Rustler native terms
(no JSON, except `debug_game`).

## Development

```sh
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
```
````

- [ ] **Step 4: Fix any other hits from Step 1**

For each remaining hit outside `docs/` and `MIGRATION.md`, apply this rule: if the line only concerns a deleted folder or the old Go/Elm/protobuf/Heroku setup, delete it; if it describes something that still exists under a new home, rewrite it to name `helios/`, `core/` or `e2e/` (e.g. a comment "ported from integration-tests/src/lobby.test.ts" becomes "ported from the legacy lobby integration tests"). Do not edit files under `docs/`.

- [ ] **Step 5: Prepend the status note to `MIGRATION.md`**

Insert these two lines at the very top of `MIGRATION.md` (above `# Migration Plan: Consolidating on Helios + Core`):
```markdown
> **Status: completed** — see docs/superpowers/specs/2026-09-23-migration-overview-design.md. This file is kept as the historical inventory; its phase list was superseded by the specs in `docs/superpowers/specs/`.

```

- [ ] **Step 5b: Update the root `AGENTS.md` for the finished migration**

In `AGENTS.md` (repo root; not `helios/AGENTS.md`):
1. In "Repository layout", change the line `Target layout (see ...):` to `Layout (design: docs/superpowers/specs/2026-09-23-migration-overview-design.md):`, and delete the paragraph starting `Legacy folders —` entirely.
2. Replace the whole "## Migration in progress" section (heading and paragraph) with:
```markdown
## History

The repo was consolidated from a Go backend, an Elm SPA and a custom websocket protocol into `helios/` + `core/` + `e2e/` in 2026. Design specs and implementation plans for that migration live in `docs/superpowers/`; `MIGRATION.md` is the historical inventory.
```
3. Keep every other section unchanged.

Check: `git grep -nE "backend_old|websocket-client|integration-tests|frontend/|proto/|Migration in progress" -- AGENTS.md; echo "exit=$?"` prints nothing and `exit=1`.

- [ ] **Step 6: Delete stray root artifacts**

Run:
```bash
rm -f erl_crash.dump helios/erl_crash.dump
git status --short
```
Expected: no `erl_crash.dump` anywhere; `git status --short` shows only `.gitignore`, `MIGRATION.md`, `AGENTS.md`, `core/README.md` and any Step 4 files as modified (plus the user's own untracked/unstaged files, which you leave alone).

- [ ] **Step 7: Re-run the reference checks**

Run:
```bash
git grep -nE "backend_old|websocket-client|integration-tests|frontend/|proto/" -- . ':!docs' ':!MIGRATION.md'; echo "exit=$?"
git grep -nE "backend/|JWT_SECRET|[Hh]eroku|[Tt]ravis|elm-app|protobuf|Elixir\.Core\b" -- . ':!docs' ':!MIGRATION.md'; echo "exit=$?"
head -n 1 MIGRATION.md
```
Expected: both greps print nothing and `exit=1` (git grep's "no match"); the first line of `MIGRATION.md` is the status note.

- [ ] **Step 8: Commit**

```bash
git add .gitignore MIGRATION.md AGENTS.md core/README.md
# plus every file changed in Step 4, listed explicitly, e.g.:
# git add e2e/tests/lobby.spec.ts
git commit -m "$(cat <<'EOF'
Remove references to deleted legacy folders

Drop .gitignore entries for backend/frontend/websocket-client, rewrite the
stale core README, mark MIGRATION.md as completed, and update the root
AGENTS.md for the finished layout.

Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

### Task 7: Final verification against the acceptance criteria

**Files:** none (verification only; commit only if a fix is needed, using explicit `git add <paths>`).

**Interfaces:**
- Consumes: everything above.
- Produces: evidence for each acceptance criterion.

- [ ] **Step 1: Local equivalents of the `core`, `helios`, `e2e` CI jobs**

Run:
```bash
(cd core && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test)
(cd helios && mix precommit)
(cd e2e && npx playwright test)
```
Expected: all pass.

- [ ] **Step 2: Rebuild the image from the final tree and run the `docker` job equivalent**

Run Task 2 Step 5 (`bin/build.sh --progress=plain …`) and Task 4 Step 4 again. Expected: build succeeds, `persisted`.

- [ ] **Step 3: Acceptance checklist**

Confirm and record in the final report:
- `bin/build.sh` succeeds locally (Step 2).
- A `bin/run.sh bin/.env.example`-style run serves `/login`, a user can log in (curl in Task 3 Step 5 + manual browser check in Task 3 Step 6), and data persists across container replacement on the volume (Task 3 Step 6, Task 4 Step 4).
- README instructions work from a clean clone (Task 5 Step 3).
- `git grep` cleanup returns nothing (Task 6 Step 7).
- CI jobs are exactly `core`, `helios`, `e2e`, `docker` (Task 4 Step 3).

- [ ] **Step 4: CI run**

Ask your human partner before pushing. Once they approve and the branch is pushed:
```bash
gh run watch --exit-status "$(gh run list --branch "$(git branch --show-current)" --limit 1 --json databaseId -q '.[0].databaseId')"
```
Expected: `core`, `helios`, `e2e`, `docker` all succeed.
