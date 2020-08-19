package crux

import (
	"github.com/Szetty/seven_wonders/backend/crux/auth"
	"github.com/Szetty/seven_wonders/backend/crux/lobby"
	"github.com/Szetty/seven_wonders/backend/crux/user"
)

type Crux struct {
	Auth  *auth.Crux
	Lobby *lobby.Crux
	User  *user.Crux
}

func Init() *Crux {
	userCrux := user.Init()
	authCrux := auth.Init()
	return &Crux{
		Auth:  authCrux,
		Lobby: lobby.Init(userCrux, authCrux),
		User:  userCrux,
	}
}
