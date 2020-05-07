package main

import (
	"github.com/Szetty/seven_wonder/backend/web"
	"github.com/sirupsen/logrus"
)

func main() {
	logrus.SetFormatter(&logrus.TextFormatter{FullTimestamp: true})
	web.StartWebServer()
}