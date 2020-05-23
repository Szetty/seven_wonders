package web

import (
	"fmt"
	"github.com/Szetty/seven_wonders/backend/common"
	"github.com/dgrijalva/jwt-go"
	"github.com/google/uuid"
	"io/ioutil"
	"net/http"
	"time"
)

var users = make(map[string]User)

type LoginRequest struct {
	Token string `json:"access_token"`
	Name  string `json:"name"`
}

type LoginResponse struct {
	Name      string `json:"name"`
	UserToken string `json:"user_token"`
	GameID    string `json:"game_id"`
}

type User struct {
	name   string
	gameID string
}

func login(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPut && r.Method != http.MethodPost {
		ErrorHandler{
			statusCode: 405,
		}.ServeHTTP(w, r)
		return
	}
	var loginRequest LoginRequest
	bytes, err := ioutil.ReadAll(r.Body)
	if err != nil {
		ErrorHandler{
			message:    fmt.Sprintf("Could not read body: %v", err),
			statusCode: 400,
			errorType:  InvalidBody,
		}.ServeHTTP(w, r)
		return
	}
	payload := string(bytes)
	err = json.UnmarshalFromString(payload, &loginRequest)
	if err != nil {
		ErrorHandler{
			message:    fmt.Sprintf("Could not parse payload %s, because: %v", payload, err),
			statusCode: 400,
			errorType:  CannotParsePayload,
		}.ServeHTTP(w, r)
		return
	}
	if loginRequest.Token != common.ACCESS_TOKEN {
		ErrorHandler{
			message:    fmt.Sprintf("Wrong token %s", loginRequest.Token),
			statusCode: 401,
			errorType:  InvalidAccessToken,
		}.ServeHTTP(w, r)
		return
	}
	if _, exists := users[loginRequest.Name]; loginRequest.Name == "" || exists {
		ErrorHandler{
			message:    fmt.Sprintf("Name was not specified or already taken: %s", loginRequest.Name),
			statusCode: 400,
			errorType:  InvalidName,
		}.ServeHTTP(w, r)
		return
	}
	id := uuid.New().String()
	jwtToken, err := createJWTToken(id, loginRequest.Name)
	if err != nil {
		ErrorHandler{
			message:    fmt.Sprintf("Could not create JWT token: %v", err),
			statusCode: 500,
			errorType:  ServerError,
		}.ServeHTTP(w, r)
		return
	}
	users[loginRequest.Name] = User{
		name:   loginRequest.Name,
		gameID: id,
	}
	response := LoginResponse{UserToken: jwtToken, GameID: id, Name: loginRequest.Name}
	sendResponse(w, 200, response)
}

func createJWTToken(id, name string) (string, error) {
	token := jwt.NewWithClaims(jwt.SigningMethodHS256, jwt.StandardClaims{
		Subject:   name,
		ExpiresAt: time.Now().Add(24 * time.Hour).Unix(),
		Id:        id,
	})
	return token.SignedString([]byte(common.JWT_SECRET))
}