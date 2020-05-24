package websocket

import (
	"github.com/gorilla/websocket"
	"sync"
	"time"
)

const (
	// Time allowed to write a message to the peer.
	writeWait = 10 * time.Second

	// Time allowed to read the next pong message from the peer.
	pongWait = 60 * time.Second

	// Send pings to peer with this period. Must be less than pongWait.
	pingPeriod = (pongWait * 9) / 10

	// Maximum message size allowed from peer.
	maxMessageSize = 512
)

var players = sync.Map{}

type Player struct {
	name       string
	conn       *websocket.Conn
	game       *Game
	incomingCh chan Envelope
	queue      Bundle
}

func connectPlayer(conn *websocket.Conn, gameId string, name string) {
	game := getGame(gameId)
	player, alreadyExists := players.LoadOrStore(name, newPlayer(conn, game, name))
	if alreadyExists {
		player.(*Player).updatePlayer(conn, game)
	}
	go player.(*Player).readChannel()
	go player.(*Player).writeChannel()
	player.(*Player).sendWelcomeMessage()
}

func newPlayer(conn *websocket.Conn, game *Game, name string) *Player {
	return &Player{
		name:       name,
		conn:       conn,
		game:       game,
		incomingCh: make(chan Envelope),
		queue:      Bundle{},
	}
}

func (p* Player) updatePlayer(conn *websocket.Conn, game *Game) {
	_ = p.conn.Close()
	close(p.incomingCh)
	p.conn = conn
	p.incomingCh = make(chan Envelope)
	p.game = game
}

func (p* Player) sendWelcomeMessage() {
	p.incomingCh <- EnveloperBuilder{}.
						Data(
							MessageBuilder{}.
							MessageType("welcome").
							Body("").
							Build(),
						).
						UUID("").
						Build()
}

// readChannel sends messages from the websocket connection to the game hub.
//
// The application runs readChannel in a per-connection goroutine. The application
// ensures that there is at most one reader on a connection by executing all
// reads from this goroutine.
func (p *Player) readChannel() {
	conn := p.conn
	game := p.game
	game.register <- p
	logger.Infof("Starting read channel for %s", p.name)
	defer func() {
		game.unregister <- p
		_ = conn.Close()
	}()
	conn.SetReadLimit(maxMessageSize)
	_ = conn.SetReadDeadline(time.Now().Add(pongWait))
	conn.SetPongHandler(func(string) error {_ = conn.SetReadDeadline(time.Now().Add(pongWait)); return nil})
	for {
		message, _, err := conn.ReadMessage()
		if err != nil {
			if websocket.IsUnexpectedCloseError(err, websocket.CloseGoingAway, websocket.CloseAbnormalClosure) {
				logger.Errorf("Fail to read WS message: %v", err)
			}
			return
		}
		logger.Infof("Got message %+v", message)
		//message = bytes.TrimSpace(bytes.Replace(message, newline, space, -1))
		//game.broadcast <- message
	}
}

// writeChannel sends messages from the game hub to the websocket connection.
//
// A goroutine running writePump is started for each connection. The
// application ensures that there is at most one writer to a connection by
// executing all writes from this goroutine.
func (p *Player) writeChannel() {
	logger.Infof("Starting write channel for %s", p.name)
	conn := p.conn
	ticker := time.NewTicker(pingPeriod)
	defer func() {
		ticker.Stop()
		_ = conn.Close()
	}()
	for {
		select {
		case message, ok := <-p.incomingCh:
			if !ok { incomingChannelClosed(conn); return }
			_ = conn.SetWriteDeadline(time.Now().Add(writeWait))
			p.queue = append(p.queue, message)

			readAllMessagesFromChannel(p, conn)

			logger.Infof("Sending message to %s: %+v", p.name, p.queue)

			if err := conn.WriteJSON(p.queue); err != nil {
				logger.Errorf("Could not write bundle message to WS: %v", err)
				return
			}
			p.queue = Bundle{}
		case <-ticker.C:
			_ = conn.SetWriteDeadline(time.Now().Add(writeWait))
			if err := conn.WriteMessage(websocket.PingMessage, nil); err != nil {
				logger.Errorf("Could not write ping message to WS: %v", err)
				return
			}
		}
	}
}

func incomingChannelClosed(conn *websocket.Conn) {
	logger.Info("Incoming channel was closed")
	_ = conn.WriteMessage(websocket.CloseMessage, []byte{})
}

func readAllMessagesFromChannel(p *Player, conn *websocket.Conn) {
	n := len(p.incomingCh)
	for i := 0; i < n; i++ {
		message, ok := <-p.incomingCh
		if !ok { incomingChannelClosed(conn); return }
		p.queue = append(p.queue, message)
	}
}