package domain

import (
	"github.com/Szetty/seven_wonders/backend/logger"
	"github.com/mitchellh/mapstructure"
)

const ErrorMessageType MessageType = "Error"

type ErrorCode string

const (
	Unauthorized ErrorCode = "Unauthorized"
)

type ErrorBody struct {
	Code ErrorCode   `json:"code" mapstructure:"code"`
	Info interface{} `json:"info" mapstructure:"info"`
}

func errorDecoder(message Message) Message {
	if message.MessageType == ErrorMessageType {
		var errorBody ErrorBody
		switch body := message.Body.(type) {
		case map[string]interface{}:
			err := mapstructure.Decode(body, &errorBody)
			if err != nil {
				logger.L.Errorf("Could not decode user body map %#v, because: %v", body, err)
			}
		}
		message.Body = errorBody
	}
	return message
}
