package game

import (
	"fmt"
	"github.com/Szetty/seven_wonders/backend/common"
	"github.com/Szetty/seven_wonders/backend/web/errors"
	"github.com/gorilla/mux"
	"github.com/gorilla/websocket"
	"net/http"
)

var upgrader = websocket.Upgrader{}
var logger = common.NewLogger("WebSocket")

func UpgradeToWS(w http.ResponseWriter, r *http.Request) {
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
	wsConn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		errors.ErrorHandler{
			Message:    fmt.Sprintf("Could not upgrade to WS: %v", err),
			StatusCode: 500,
			ErrorType:  errors.ServerError,
		}.ServeHTTP(w, r)
		return
	}
	connectPlayer(wsConn, gameId, name)
}

func closeWebsocket(conn *websocket.Conn, reason string) {
	cm := websocket.FormatCloseMessage(websocket.CloseNormalClosure, reason)
	if err := conn.WriteMessage(websocket.CloseMessage, cm); err != nil {
		logger.Error(err)
		err = conn.Close()
		if err != nil {
			logger.Error(err)
		}
	}
}

