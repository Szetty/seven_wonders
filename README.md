# Seven Wonders Digital version

![CI](https://github.com/Szetty/seven_wonders/actions/workflows/ci.yml/badge.svg)
![Heroku](https://heroku-badge.herokuapp.com/?app=seven-wonders-szetty)

## Prerequisites

- [Install Rust](https://www.rust-lang.org)
- [Install Go 1.14.2](https://golang.org/doc/install)
- [Install Elm 0.19.1](https://guide.elm-lang.org/install/elm.html)

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

### Backend new

In backend folder:
```shell script
iex -S mix
```

Run tests:
```shell script
ACCESS_TOKEN="TEST" JWT_SECRET="test" go test -v -race ./...
```

### Frontend

In *websocket-client* folder:

```shell script
npm install
npm run build
```

In frontend folder:
```shell script
npm install
elm make
elm-app start
```

## Deployment
```shell script
bin/build.sh
bin/run.sh
```

## Generate Protobuf modules

The communication between the backend and core happens using binary encoding using Protobuf.

### Backend

To generate protobuf modules, use the Mix task provided:
```shell script
mix gen_proto ping.proto
```

### Core

To generate protobuf modules, use the Cargo make task provided:
```shell script
cargo make gen_proto start_game.proto
```