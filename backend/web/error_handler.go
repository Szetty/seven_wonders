package web

import (
	"github.com/sirupsen/logrus"
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
)

type ErrorHandler struct {
	message string
	statusCode int
	errorType ErrorType
}

type ErrorResponse struct {
	ErrorMessage string `json:"error_message"`
	ErrorType ErrorType `json:"error_type"`
}

func (h ErrorHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	logrus.Warn(h.message)
	response := ErrorResponse{ErrorMessage: h.message, ErrorType: h.errorType}
	respBytes, err := json.Marshal(&response)
	if err != nil {
		logrus.Errorf("Could not decode to json: %v", err)
		w.WriteHeader(500)
		return
	}
	w.WriteHeader(h.statusCode)
	_, err = w.Write(respBytes)
	if err != nil {
		logrus.Errorf("Could not write error message: %v", err)
		return
	}
}
