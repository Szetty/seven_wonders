package web

import (
	"net/http"
)

func sendResponse(w http.ResponseWriter, statusCode int, response interface{}) {
	respBytes, err := json.Marshal(&response)
	if err != nil {
		logger.Errorf("Could not decode response: %v", err)
		w.WriteHeader(500)
		return
	}
	w.WriteHeader(statusCode)
	_, err = w.Write(respBytes)
	if err != nil {
		logger.Errorf("Could not write response: %v", err)
		return
	}
}

func sendStatus(w http.ResponseWriter, statusCode int) {
	w.WriteHeader(statusCode)
}
