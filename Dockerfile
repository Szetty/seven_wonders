FROM golang:1.14-buster AS backend_builder
WORKDIR /go/src/app
COPY backend .
RUN go build -o seven_wonders_web
RUN chmod 777 seven_wonders_web

FROM node:9 AS frontend_builder
COPY frontend /source/frontend
WORKDIR /source/frontend
RUN rm -rf node_modules
RUN rm -rf elm_stuff
RUN npm install -g elm-github-install create-elm-app@4.2.16 --unsafe-perm=true
RUN npm install
RUN elm-app build

FROM ubuntu:bionic
COPY --from=frontend_builder /source/frontend/build ./build
COPY --from=backend_builder /go/src/app/seven_wonders_web .
COPY backend/scripts ./scripts
ENTRYPOINT ["./seven_wonders_web"]