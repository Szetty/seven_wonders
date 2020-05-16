package common

import (
	"github.com/joho/godotenv"
	"github.com/sirupsen/logrus"
	"os"
)

var ACCESS_TOKEN string
var JWT_SECRET string

func init() {
	if err := godotenv.Load(); err != nil {
		logrus.Fatal("No .env file found")
	}
	var ok bool
	if ACCESS_TOKEN, ok = os.LookupEnv("ACCESS_TOKEN"); !ok {
		logrus.Fatalf("Could not read ACCESS_TOKEN from environment variables")
	}
	if JWT_SECRET, ok = os.LookupEnv("JWT_SECRET"); !ok {
		logrus.Fatalf("Could not read JWT_SECRET from environment variables")
	}
}