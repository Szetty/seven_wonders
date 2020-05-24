package websocket

import "sync"

var games = sync.Map{}

func getGame(id string) *Game {
	game, _ := games.LoadOrStore(id, &Game{
		broadcast:  make(chan Envelope),
		register:   make(chan *Player),
		unregister: make(chan *Player),
		players:    make(map[*Player]bool),
	})
	return game.(*Game)
}

type Game struct {
	// Registered players.
	players map[*Player]bool

	// Inbound messages from the players.
	broadcast chan Envelope

	// Register requests from the players.
	register chan *Player

	// Unregister requests from players.
	unregister chan *Player
}

func (g *Game) run() {
	for {
		select {
		case player := <-g.register:
			g.players[player] = true
		case player := <-g.unregister:
			if _, ok := g.players[player]; ok {
				delete(g.players, player)
				close(player.incomingCh)
			}
		case message := <-g.broadcast:
			for client := range g.players {
				select {
				case client.incomingCh <- message:
				default:
					close(client.incomingCh)
					delete(g.players, client)
				}
			}
		}
	}
}