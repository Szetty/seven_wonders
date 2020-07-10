package users

import (
	"github.com/Szetty/seven_wonders/backend/common"
	"github.com/dgrijalva/jwt-go"
	"github.com/google/uuid"
	"sync"
)

var authenticatedUsers = sync.Map{}

type User struct {
	name   string
	gameID string
}

func CreateUser(name string) (err error, jwtToken string, gameID string) {
	gameID = uuid.New().String()
	jwtToken, err = createJWTToken(gameID, name)
	if err != nil {
		return
	}
	authenticatedUsers.Store(name, User{
		name:   name,
		gameID: gameID,
	})
	return
}

func NameExists(name string) bool {
	_, exists := authenticatedUsers.Load(name)
	return exists
}

func Delete(name string) {
	authenticatedUsers.Delete(name)
}

func DoesBelongGameIDToUser(name, gameID string) bool {
	user, exists := authenticatedUsers.Load(name)
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
