# Seven Wonders Digital version

[![Build Status](https://travis-ci.com/Szetty/seven_wonders.svg?branch=master)](https://travis-ci.com/Szetty/seven_wonders)
![Heroku](https://heroku-badge.herokuapp.com/?app=seven-wonders-szetty)

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

Run tests:
```shell script
ACCESS_TOKEN="TEST" JWT_SECRET="test" go test -v -race ./...
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