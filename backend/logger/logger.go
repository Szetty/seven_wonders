package logger

import (
	"github.com/sirupsen/logrus"
)

var L = logrus.New()

func init() {
	L.SetFormatter(&logrus.TextFormatter{FullTimestamp: true})
}

func SetDebugLevel() {
	L.SetLevel(logrus.DebugLevel)
}
