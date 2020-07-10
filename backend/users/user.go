package users

import (
	"github.com/Szetty/seven_wonders/backend/common"
	"github.com/dgrijalva/jwt-go"
	"github.com/google/uuid"
	"sync"
)

var users = sync.Map{}

type User struct {
	name   string
	gameID string
}

func CreateUser(name string) (error, string, string) {
	id := uuid.New().String()
	jwtToken, err := createJWTToken(id, name)
	if err != nil {
		return nil, "", ""
	}
	users.Store(name, User{
		name:   name,
		gameID: id,
	})
	return nil, jwtToken, id
}

func NameExists(name string) bool {
	_, exists := users.Load(name)
	return exists
}

func Delete(name string) {
	users.Delete(name)
}

func DoesBelongGameIDToUser(name, gameID string) bool {
	user, exists := users.Load(name)
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
