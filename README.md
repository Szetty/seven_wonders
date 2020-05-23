# Seven Wonders Digital version

## Prerequisites

- [Install Haskell](https://www.haskell.org/platform/)
- [Install Stack 2.1.3](https://docs.haskellstack.org/en/stable/README/)
- [Install Go 1.14.2](https://golang.org/doc/install)
- [Install Elm 0.19.1](https://guide.elm-lang.org/install/elm.html)

## Development

### Core

In core folder:
```shell script
stack build
stack test
stack run
```

### Backend

In backend folder:
```shell script
go run main.go
```

### Frontend

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