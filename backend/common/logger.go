package common

import (
	"github.com/sirupsen/logrus"
)

func NewLogger(serviceName string) *logrus.Entry {
	logger := logrus.New()
	logger.SetFormatter(&logrus.TextFormatter{FullTimestamp: true})
	return logger.WithField("serviceName", serviceName)
}