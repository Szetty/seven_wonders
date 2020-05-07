FROM node:8 AS frontend_builder
RUN npm install -g elm-github-install create-elm-app@4.2.16 --unsafe-perm=true
COPY frontend /source/frontend
WORKDIR /source/frontend
RUN elm-app build

FROM golang:1.14-alpine3.11 AS backend_builder
WORKDIR /go/src/app
COPY backend .
RUN go build -o seven_wonders_web
RUN chmod 777 seven_wonders_web
ENTRYPOINT ["./seven_wonders_web"]

FROM alpine:3.11
WORKDIR /home
COPY --from=frontend_builder /source/frontend/build ./build
COPY --from=backend_builder /go/src/app/seven_wonders_web .
ENTRYPOINT ["./seven_wonders_web"]