package web

import (
	"fmt"
	"github.com/Szetty/seven_wonders/backend/common"
	"github.com/Szetty/seven_wonders/backend/core"
	"github.com/Szetty/seven_wonders/backend/game"
	"github.com/Szetty/seven_wonders/backend/web/errorHandling"
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

	apiRouter := router.PathPrefix("/api").Subrouter()
	defineAPIRouter(apiRouter)

	spa := SPAHandler{StaticPath: "build", IndexPath: "index.html"}
	router.PathPrefix("/").Handler(spa)

	return router
}

func defineAPIRouter(apiRouter *mux.Router) {
	apiRouter.HandleFunc("/ping", ping)
	apiRouter.HandleFunc("/login", login)

	securedRouter := apiRouter.PathPrefix("/secured").Subrouter()
	defineSecuredRouter(securedRouter)

	apiRouter.PathPrefix("/").Handler(errorHandling.ErrorHandler{
		StatusCode: 404,
		Message:    "Endpoint does not exist",
		ErrorType:  errorHandling.InvalidEndpoint,
	})
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

func defineSecuredRouter(securedRouter *mux.Router) {
	securedRouter.Use(jwtAuthorizationMiddleware)
	securedRouter.Use(nameVerificationMiddleware)
	securedRouter.HandleFunc("/logout", logout)
	securedRouter.HandleFunc("/checkToken", checkToken)
	gameRouter := securedRouter.PathPrefix("/game").Subrouter()
	defineGameRouter(gameRouter)
}

func defineGameRouter(gameRouter *mux.Router) {
	gameRouter.Use(gameAuthorizationMiddleware)
	gameRouter.HandleFunc("/{gameID}", gameHandler)
	gameRouter.HandleFunc("/lobby/{gameID}", gameLobbyHandler)
}

func gameHandler(w http.ResponseWriter, r *http.Request) {
	gameId := mux.Vars(r)["gameID"]
	username := r.Context().Value("name").(string)
	if gameId == "" {
		errorHandling.ErrorHandler{
			Message:    fmt.Sprintf("Could not upgrade to WS: game id is empty"),
			StatusCode: 400,
			ErrorType:  errorHandling.InvalidGameID,
		}.ServeHTTP(w, r)
		return
	}
	session := websocket.CreateWSSession(w, r, username)
	if session != nil {
		//session.hubCh = game.ConnectPlayer(session, gameId, name)
	}
}

func gameLobbyHandler(w http.ResponseWriter, r *http.Request) {
	gameID := mux.Vars(r)["gameID"]
	name := r.Context().Value("name").(string)
	if gameID == "" {
		errorHandling.ErrorHandler{
			Message:    fmt.Sprintf("Could not upgrade to WS: game id is empty"),
			StatusCode: 400,
			ErrorType:  errorHandling.InvalidGameID,
		}.ServeHTTP(w, r)
		return
	}
	session := websocket.CreateWSSession(w, r, name)
	if session != nil {
		logger.Infof("Registering session %s in lobby %s", session.ID, gameID)
		hubCh := game.RegisterInLobby(name, gameID, session.ClientCh)
		session.EventCh <- websocket.RegisterHubEvent{HubCh: hubCh}
	}
}
