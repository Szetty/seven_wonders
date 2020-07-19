package web

import (
	"github.com/Szetty/seven_wonders/backend/logger"
	"net/http"
)

func sendResponse(w http.ResponseWriter, statusCode int, response interface{}) {
	respBytes, err := json.Marshal(&response)
	if err != nil {
		logger.L.Errorf("Could not decode response: %v", err)
		w.WriteHeader(500)
		return
	}
	w.WriteHeader(statusCode)
	_, err = w.Write(respBytes)
	if err != nil {
		logger.L.Errorf("Could not write response: %v", err)
		return
	}
}

func sendStatus(w http.ResponseWriter, statusCode int) {
	w.WriteHeader(statusCode)
}
