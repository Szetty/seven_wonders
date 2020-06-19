package main

import (
	"github.com/Szetty/seven_wonders/backend/common"
	"github.com/Szetty/seven_wonders/backend/web"
	"net/http"
	"os"
	"strconv"
	"time"
)

var logger = common.NewLogger("Main")

func main() {
	port, err := strconv.Atoi(os.Getenv("PORT"))
	if err != nil {
		logger.Warn("Could not get port from env variables, falling back to 8080")
		port = 8080
	}

	srv := &http.Server{
		Handler: web.MainHandler(),
		Addr:    "0.0.0.0:" + strconv.Itoa(port),
		// Good practice: enforce timeouts for servers you create!
		WriteTimeout: 15 * time.Second,
		ReadTimeout:  15 * time.Second,
	}

	logger.Info("Listening on port " + strconv.Itoa(port))
	logger.Fatal(srv.ListenAndServe())
}
