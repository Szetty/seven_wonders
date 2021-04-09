package logger

import (
	"github.com/sirupsen/logrus"
)

var L = logrus.New()

func init() {
	L.SetFormatter(&logrus.TextFormatter{FullTimestamp: true})
}

func SetLevel(lvl string) {
	level, err := logrus.ParseLevel(lvl)
	if err == nil {
		L.SetLevel(level)
	}
}

func SetDebugLevel() {
	L.SetLevel(logrus.DebugLevel)
}
