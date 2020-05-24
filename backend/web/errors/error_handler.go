package errors

import (
	"encoding/json"
	"github.com/Szetty/seven_wonders/backend/common"
	"net/http"
)

type ErrorType string

const(
	InvalidBody ErrorType = "INVALID_BODY"
	CannotParsePayload = "CANNOT_PARSE_PAYLOAD"
	InvalidAccessToken = "INVALID_ACCESS_TOKEN"
	InvalidName = "INVALID_NAME"
	ServerError = "SERVER_ERROR"
	Unauthorized = "UNAUTHORIZED"
	InvalidEndpoint = "INVALID_ENDPOINT"
	InvalidGameID = "INVALID_GAME_ID"
)

var logger = common.NewLogger("Web")

type ErrorHandler struct {
	Message    string
	StatusCode int
	ErrorType  ErrorType
}

type ErrorResponse struct {
	ErrorMessage string `json:"error_message"`
	ErrorType ErrorType `json:"error_type"`
}

func (h ErrorHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	logger.Warn(h.Message)
	response := ErrorResponse{ErrorMessage: h.Message, ErrorType: h.ErrorType}
	respBytes, err := json.Marshal(&response)
	if err != nil {
		logger.Errorf("Could not decode to json: %v", err)
		w.WriteHeader(500)
		return
	}
	w.WriteHeader(h.StatusCode)
	_, err = w.Write(respBytes)
	if err != nil {
		logger.Errorf("Could not write error Message: %v", err)
		return
	}
}
