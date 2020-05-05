# Seven Wonders Digital version

## Prerequisites

- [Install Haskell](https://www.haskell.org/platform/)
- [Install Stack 2.1.3](https://docs.haskellstack.org/en/stable/README/)
- [Install Go 1.14.2](https://golang.org/doc/install)
- [Install Elm 0.19.1](https://guide.elm-lang.org/install/elm.html)

## Core

In core folder:
```shell script
stack build
stack test
stack run
```

## Backend

In backend folder:
```shell script
go build github.com/Szetty/seven_wonder/backend
./backend
```

## Frontend

In frontend folder:
```shell script
elm reactor
```