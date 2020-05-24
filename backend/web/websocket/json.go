package websocket

import "encoding/json"

func encodeBundle(bundle Bundle) ([]byte, error) {
	return json.Marshal(bundle)
}

func decodeBundle(bundleBytes []byte) (Bundle, error) {
	var bundle Bundle
	err := json.Unmarshal(bundleBytes, &bundle)
	if err != nil {
		return bundle, err
	}
	return bundle, nil
}
