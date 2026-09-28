FROM golang:1.14-buster AS backend_builder
WORKDIR /go/src/app
COPY backend .
RUN go build -o seven_wonders_web
RUN chmod 777 seven_wonders_web

FROM ubuntu:bionic
COPY --from=backend_builder /go/src/app/seven_wonders_web .
COPY backend/scripts ./scripts
ENTRYPOINT ["./seven_wonders_web"]