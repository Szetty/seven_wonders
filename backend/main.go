package main

import (
	"github.com/Szetty/seven_wonders/backend/web"
	"github.com/sirupsen/logrus"
)

func init() {
	logrus.SetFormatter(&logrus.TextFormatter{FullTimestamp: true})
}

func main() {
	web.StartWebServer()
}