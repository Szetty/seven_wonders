package common

import (
	"github.com/joho/godotenv"
	"os"
)

var ACCESS_TOKEN string
var JWT_SECRET string

func init() {
	logger := NewLogger("Init")
	if err := godotenv.Load(); err != nil {
		logger.Warn("No .env file found")
	}
	var ok bool
	if ACCESS_TOKEN, ok = os.LookupEnv("ACCESS_TOKEN"); !ok {
		logger.Fatalf("Could not read ACCESS_TOKEN from environment variables")
	}
	if JWT_SECRET, ok = os.LookupEnv("JWT_SECRET"); !ok {
		logger.Fatalf("Could not read JWT_SECRET from environment variables")
	}
}
