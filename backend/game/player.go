package game

import (
	"github.com/Szetty/seven_wonders/backend/web/websocket"
	"sync"
)

var players = sync.Map{}

type Player struct {
	name    string
	game    *Game
	session *websocket.Session
}

func ConnectPlayer(session *websocket.Session, gameId string, name string) {
	game := getGame(gameId)
	players.LoadOrStore(name, newPlayer(session, game, name))
}

func newPlayer(session *websocket.Session, game *Game, name string) *Player {
	return &Player{
		name:    name,
		session: session,
		game:    game,
	}
}
