# Seven Wonders Digital version

![CI](https://github.com/Szetty/seven_wonders/actions/workflows/ci.yml/badge.svg)
![Heroku](https://heroku-badge.herokuapp.com/?app=seven-wonders-szetty)

## Prerequisites

- [Install Rust](https://www.rust-lang.org)
- [Install Go 1.14.2](https://golang.org/doc/install)

## Development

### Core

In core folder:
```shell script
cargo build
cargo test
```

### Backend old

In backend folder:
```shell script
go run main.go
```

Run tests:
```shell script
ACCESS_TOKEN="TEST" JWT_SECRET="test" go test -v -race ./...
```

### Web app

The UI (login, lobby, game table) is served by Helios:

```shell script
cd helios
mix setup
mix phx.server
```

## Deployment
```shell script
bin/build.sh
bin/run.sh
```

