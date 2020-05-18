package web

import (
	"fmt"
	"net/http"
)

func gameLobby(w http.ResponseWriter, r *http.Request) {
	welcomeMessage := fmt.Sprintf("Hello, %s", r.Context().Value("name"))
	_, _ = w.Write([]byte(welcomeMessage))
}