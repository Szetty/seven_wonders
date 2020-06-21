package users

import (
	"github.com/Szetty/seven_wonders/backend/common"
	"github.com/dgrijalva/jwt-go"
	"github.com/google/uuid"
	"sync"
	"time"
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

func createJWTToken(id, name string) (string, error) {
	token := jwt.NewWithClaims(jwt.SigningMethodHS256, jwt.StandardClaims{
		Subject:   name,
		//ExpiresAt: time.Now().Add(24 * time.Hour).Unix(),
		ExpiresAt: time.Now().Add(30 * time.Second).Unix(),
		Id:        id,
	})
	return token.SignedString([]byte(common.JWT_SECRET))
}