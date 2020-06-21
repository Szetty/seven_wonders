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
)

var json = jsoniter.ConfigCompatibleWithStandardLibrary
var logger = common.NewLogger("Web")
var coreServer *core.Server

func MainHandler() http.Handler {
	router := mux.NewRouter()
	router.Use(loggingMiddleware)

	api := router.PathPrefix("/api").Subrouter()
	defineAPI(api)

	spa := SPAHandler{StaticPath: "build", IndexPath: "index.html"}
	router.PathPrefix("/").Handler(spa)

	return router
}

func defineAPI(api *mux.Router) {
	api.HandleFunc("/ping", ping)
	api.HandleFunc("/login", login)

	secured := api.PathPrefix("/secured").Subrouter()
	defineSecured(secured)

	logoutRouter := api.PathPrefix("/logout").Subrouter()
	logoutRouter.Use(jwtWithoutClaimsAuthorizationMiddleware)
	logoutRouter.HandleFunc("", logout)

	api.PathPrefix("/").Handler(errors.ErrorHandler{
		StatusCode: 404,
		Message:    "Endpoint does not exist",
		ErrorType:  errors.InvalidEndpoint,
	})
}

func defineSecured(secured *mux.Router) {
	secured.Use(jwtAuthorizationMiddleware)
	secured.Use(nameVerificationMiddleware)
	secured.HandleFunc("/game/{game}", gameHandler)
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

func gameHandler(w http.ResponseWriter, r *http.Request) {
	gameId := mux.Vars(r)["game"]
	name := r.Context().Value("name").(string)
	if gameId == "" {
		errors.ErrorHandler{
			Message:    fmt.Sprintf("Could not upgrade to WS: game id is empty"),
			StatusCode: 400,
			ErrorType:  errors.InvalidGameID,
		}.ServeHTTP(w, r)
		return
	}
	session := websocket.CreateWSSession(w, r, name)
	if session != nil {
		//game.ConnectPlayer(session, gameId, name)
	}
}
