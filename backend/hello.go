package main

import (
	"fmt"
	"net/http"
)

func main() {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		_, _ = fmt.Fprintf(w, "Hello, you've requested: %s\n", r.URL.Path)
	})

	_ = http.ListenAndServe(":8080", nil)
}