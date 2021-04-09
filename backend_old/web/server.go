package web

import (
	"context"
	cr "github.com/Szetty/seven_wonders/backend_old/crux"
	"github.com/Szetty/seven_wonders/backend_old/web/websocket"
	"github.com/gorilla/mux"
	"net/http"
)

type Server struct {
	crux     *cr.Crux
	router   *mux.Router
	sessions *websocket.Sessions
}

func InitServer(router *mux.Router) *Server {
	return &Server{
		crux:     cr.Init(),
		router:   router,
		sessions: websocket.InitSessions(),
	}
}

func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	r = r.WithContext(context.WithValue(r.Context(), "server", s))
	s.router.ServeHTTP(w, r)
}

func (s *Server) Crux() *cr.Crux {
	return s.crux
}

func (s *Server) Sessions() *websocket.Sessions {
	return s.sessions
}

func crux(r *http.Request) *cr.Crux {
	return r.Context().Value("server").(*Server).Crux()
}

func sessions(r *http.Request) *websocket.Sessions {
	return r.Context().Value("server").(*Server).Sessions()
}
