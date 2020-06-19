package websocket

import (
	"fmt"
	"github.com/Szetty/seven_wonders/backend/common"
	"github.com/Szetty/seven_wonders/backend/web/errors"
	"github.com/gorilla/websocket"
	"net/http"
)

var upgrader = websocket.Upgrader{}
var logger = common.NewLogger("WebSocket")

func CreateWSSession(w http.ResponseWriter, r *http.Request, name string) *Session {
	wsConn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		errors.ErrorHandler{
			Message:    fmt.Sprintf("Could not upgrade to WS: %v", err),
			StatusCode: 500,
			ErrorType:  errors.ServerError,
		}.ServeHTTP(w, r)
		return nil
	}
	return findOrCreateSession(name, wsConn)
}