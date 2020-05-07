package web

import (
	"fmt"
	"github.com/gorilla/mux"
	"github.com/sirupsen/logrus"
	"net/http"
	"os"
	"strconv"
	"time"
)

func StartWebServer() {
	router := mux.NewRouter()

	router.HandleFunc("/ping", func(w http.ResponseWriter, r *http.Request) {
		_, _ = fmt.Fprintf(w, "Hello, you've requested: %s\n", r.URL.Path)
	})

	spa := SPAHandler{StaticPath: "build", IndexPath: "index.html"}
	router.PathPrefix("/").Handler(spa)

	port, err := strconv.Atoi(os.Getenv("PORT"))
	if err != nil {
		logrus.Warn("Could not get port from env variables, falling back to 8080")
		port = 8080
	}

	srv := &http.Server{
		Handler: router,
		Addr:    "0.0.0.0:" + strconv.Itoa(port),
		// Good practice: enforce timeouts for servers you create!
		WriteTimeout: 15 * time.Second,
		ReadTimeout:  15 * time.Second,
	}

	logrus.Info("Listening on port " + strconv.Itoa(port))
	logrus.Fatal(srv.ListenAndServe())
}