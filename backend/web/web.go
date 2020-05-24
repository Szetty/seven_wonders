package web

import (
	"fmt"
	"github.com/Szetty/seven_wonders/backend/common"
	"github.com/Szetty/seven_wonders/backend/core"
	"github.com/Szetty/seven_wonders/backend/web/errors"
	"github.com/Szetty/seven_wonders/backend/web/websocket"
	"github.com/gorilla/mux"
	"github.com/json-iterator/go"
	"net/http"
	"os"
	"strconv"
	"time"
)

var json = jsoniter.ConfigCompatibleWithStandardLibrary
var logger = common.NewLogger("Web")
var coreServer *core.Server

func StartWebServer() {
	router := mux.NewRouter()
	router.Use(loggingMiddleware)

	api := router.PathPrefix("/api").Subrouter()
	defineAPI(api)

	spa := SPAHandler{StaticPath: "build", IndexPath: "index.html"}
	router.PathPrefix("/").Handler(spa)

	port, err := strconv.Atoi(os.Getenv("PORT"))
	if err != nil {
		logger.Warn("Could not get port from env variables, falling back to 8080")
		port = 8080
	}

	srv := &http.Server{
		Handler: router,
		Addr:    "0.0.0.0:" + strconv.Itoa(port),
		// Good practice: enforce timeouts for servers you create!
		WriteTimeout: 15 * time.Second,
		ReadTimeout:  15 * time.Second,
	}

	logger.Info("Listening on port " + strconv.Itoa(port))
	logger.Fatal(srv.ListenAndServe())
}

func defineAPI(api *mux.Router) {
	api.HandleFunc("/ping", ping)
	api.HandleFunc("/login", login)

	secured := api.PathPrefix("/secured").Subrouter()
	defineSecured(secured)

	api.PathPrefix("/").Handler(errors.ErrorHandler{
		StatusCode: 404,
		Message:    "Endpoint does not exist",
		ErrorType:  errors.InvalidEndpoint,
	})
}

func defineSecured(secured *mux.Router) {
	secured.Use(jwtAuthorizationMiddleware)
	secured.HandleFunc("/game/{game}", websocket.UpgradeToWS)
	secured.HandleFunc("/gameLobby", gameLobby)
}

func ping(w http.ResponseWriter, r *http.Request) {
	var err error
	if coreServer == nil {
		coreServer, err = core.StartCoreServer()
	}
	if err != nil {
		logger.Errorf("Starting core server failed: %v", err)
		_, _ = fmt.Fprint(w, "Could not start core server")
		return
	}
	pong, err := core.Ping(*coreServer)
	if err != nil {
		logger.Errorf("Ping to core server failed: %v", err)
		_, _ = fmt.Fprint(w, "Pong Backend -> Frontend (ping Backend -> Core failed)")
		return
	}
	_, _ = fmt.Fprint(w, pong)
}