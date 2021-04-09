package errorHandling

import (
	"encoding/json"
	"github.com/Szetty/seven_wonders/backend/logger"
	"net/http"
)

type ErrorType string

const (
	ServerError        ErrorType = "SERVER_ERROR"
	InvalidBody                  = "INVALID_BODY"
	CannotParsePayload           = "CANNOT_PARSE_PAYLOAD"
	InvalidEndpoint              = "INVALID_ENDPOINT"
	InvalidAccessToken           = "INVALID_ACCESS_TOKEN"
	InvalidName                  = "INVALID_NAME"
	InvalidGameID                = "INVALID_GAME_ID"
	Unauthorized                 = "UNAUTHORIZED"
	InvalidUser                  = "INVALID_USER"
)

type ErrorHandler struct {
	Message    string
	StatusCode int
	ErrorType  ErrorType
}

type ErrorResponse struct {
	ErrorMessage string    `json:"error_message"`
	ErrorType    ErrorType `json:"error_type"`
}

func (h ErrorHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	logger.L.Warn(h.Message)
	response := ErrorResponse{ErrorMessage: h.Message, ErrorType: h.ErrorType}
	respBytes, err := json.Marshal(&response)
	if err != nil {
		logger.L.Errorf("Could not decode to json: %v", err)
		w.WriteHeader(500)
		return
	}
	w.WriteHeader(h.StatusCode)
	_, err = w.Write(respBytes)
	if err != nil {
		logger.L.Errorf("Could not write error Message: %v", err)
		return
	}
}
