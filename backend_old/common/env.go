package common

import (
	"github.com/Szetty/seven_wonders/backend_old/logger"
	"github.com/joho/godotenv"
	"os"
)

var ACCESS_TOKEN string
var JWT_SECRET string

func init() {
	if err := godotenv.Load(); err != nil {
		logger.L.Warn("No .env file found")
	}
	var ok bool
	if ACCESS_TOKEN, ok = os.LookupEnv("ACCESS_TOKEN"); !ok {
		logger.L.Fatalf("Could not read ACCESS_TOKEN from environment variables")
	}
	if JWT_SECRET, ok = os.LookupEnv("JWT_SECRET"); !ok {
		logger.L.Fatalf("Could not read JWT_SECRET from environment variables")
	}
	if _, ok = os.LookupEnv("DEBUG"); ok {
		logger.SetDebugLevel()
	}
}
