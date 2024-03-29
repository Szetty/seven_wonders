package web

import (
	"fmt"
	"github.com/Szetty/seven_wonders/backend_old/common"
	"github.com/Szetty/seven_wonders/backend_old/web/errorHandling"
	"io/ioutil"
	"net/http"
)

type LoginRequest struct {
	Token string `json:"access_token"`
	Name  string `json:"name"`
}

type LoginResponse struct {
	Name      string `json:"name"`
	UserToken string `json:"user_token"`
	GameID    string `json:"game_id"`
}

func login(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPut && r.Method != http.MethodPost {
		errorHandling.ErrorHandler{
			StatusCode: 405,
		}.ServeHTTP(w, r)
		return
	}
	var loginRequest LoginRequest
	bytes, err := ioutil.ReadAll(r.Body)
	if err != nil {
		errorHandling.ErrorHandler{
			Message:    fmt.Sprintf("Could not read body: %v", err),
			StatusCode: 400,
			ErrorType:  errorHandling.InvalidBody,
		}.ServeHTTP(w, r)
		return
	}
	payload := string(bytes)
	err = json.UnmarshalFromString(payload, &loginRequest)
	if err != nil {
		errorHandling.ErrorHandler{
			Message:    fmt.Sprintf("Could not parse payload %s, because: %v", payload, err),
			StatusCode: 400,
			ErrorType:  errorHandling.CannotParsePayload,
		}.ServeHTTP(w, r)
		return
	}
	if loginRequest.Token != common.ACCESS_TOKEN {
		errorHandling.ErrorHandler{
			Message:    fmt.Sprintf("Wrong token %s", loginRequest.Token),
			StatusCode: 401,
			ErrorType:  errorHandling.InvalidAccessToken,
		}.ServeHTTP(w, r)
		return
	}
	if loginRequest.Name == "" || crux(r).Auth.NameExists(loginRequest.Name) {
		errorHandling.ErrorHandler{
			Message:    fmt.Sprintf("Name was not specified or already taken: %s", loginRequest.Name),
			StatusCode: 400,
			ErrorType:  errorHandling.InvalidName,
		}.ServeHTTP(w, r)
		return
	}
	err, jwtToken, gameID := crux(r).Auth.CreateUser(loginRequest.Name)
	if err != nil {
		errorHandling.ErrorHandler{
			Message:    fmt.Sprintf("Could not create JWT token: %v", err),
			StatusCode: 500,
			ErrorType:  errorHandling.ServerError,
		}.ServeHTTP(w, r)
	}
	response := LoginResponse{UserToken: jwtToken, GameID: gameID, Name: loginRequest.Name}
	sendResponse(w, 200, response)
}

func logout(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPut && r.Method != http.MethodPost {
		errorHandling.ErrorHandler{
			StatusCode: 405,
		}.ServeHTTP(w, r)
		return
	}
	name := r.Context().Value("name").(string)
	crux(r).Auth.Delete(name)
	sendStatus(w, http.StatusNoContent)
}

func checkToken(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPut && r.Method != http.MethodPost {
		errorHandling.ErrorHandler{
			StatusCode: 405,
		}.ServeHTTP(w, r)
		return
	}
	sendStatus(w, http.StatusNoContent)
}
