package auth

import (
	"github.com/Szetty/seven_wonders/backend/common"
	"github.com/Szetty/seven_wonders/backend/logger"
	"github.com/dgrijalva/jwt-go"
	"github.com/google/uuid"
	"sync"
)

type Crux struct {
	users sync.Map
}

type User struct {
	name   string
	gameID string
}

func Init() *Crux {
	return &Crux{
		users: sync.Map{},
	}
}

func (c *Crux) CreateUser(name string) (err error, jwtToken string, gameID string) {
	gameID = uuid.New().String()
	jwtToken, err = createJWTToken(gameID, name)
	if err != nil {
		return
	}
	logger.L.Infof(c.messageWithPrefix("Created user %s with game id %s"), name, gameID)
	c.users.Store(name, User{
		name:   name,
		gameID: gameID,
	})
	return
}

func (c *Crux) NameExists(name string) bool {
	_, exists := c.users.Load(name)
	return exists
}

func (c *Crux) Delete(name string) {
	c.users.Delete(name)
}

func (c *Crux) UserByGameID(gameID string) string {
	username := ""
	c.users.Range(func(key, value interface{}) bool {
		user := value.(User)
		if user.gameID == gameID {
			username = key.(string)
			return false
		}
		return true
	})
	return username
}

func (c *Crux) GameIDBelongsToUser(name, gameID string) bool {
	user, exists := c.users.Load(name)
	if !exists || user.(User).gameID != gameID {
		return false
	}
	return true
}

func createJWTToken(id, name string) (string, error) {
	token := jwt.NewWithClaims(jwt.SigningMethodHS256, jwt.StandardClaims{
		Subject: name,
		Id:      id,
	})
	return token.SignedString([]byte(common.JWT_SECRET))
}

func (c *Crux) messageWithPrefix(s string) string {
	return "AUTH_CRUX: " + s
}
