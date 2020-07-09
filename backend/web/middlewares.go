package web

import (
	"context"
	"fmt"
	"github.com/Szetty/seven_wonders/backend/common"
	"github.com/Szetty/seven_wonders/backend/game"
	"github.com/Szetty/seven_wonders/backend/users"
	"github.com/Szetty/seven_wonders/backend/web/errorHandling"
	"github.com/dgrijalva/jwt-go"
	"github.com/gorilla/mux"
	"net/http"
	"strings"
)

func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		logger.Infof("accessed: %s", r.RequestURI)
		next.ServeHTTP(w, r)
	})
}

func jwtAuthorizationMiddleware(next http.Handler) http.Handler {
	const prefix = "Bearer "
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		authorizationHeader := r.Header.Get("authorization")
		authorizationQueryValue := r.URL.Query().Get("authorization")
		if !strings.HasPrefix(authorizationHeader, prefix) && authorizationQueryValue == "" {
			errorHandling.ErrorHandler{
				Message:    fmt.Sprintf("Invalid authorization header or query value"),
				StatusCode: 401,
				ErrorType:  errorHandling.Unauthorized,
			}.ServeHTTP(w, r)
			return
		}
		var jwtToken string
		if authorizationQueryValue != "" {
			jwtToken = authorizationQueryValue
		} else {
			jwtToken = strings.TrimPrefix(authorizationHeader, prefix)
		}
		token, err := jwt.ParseWithClaims(
			jwtToken,
			&jwt.StandardClaims{},
			func(token *jwt.Token) (i interface{}, err error) {
				return []byte(common.JWT_SECRET), nil
			},
		)
		if err != nil {
			errorHandling.ErrorHandler{
				Message:    fmt.Sprintf("Unauthorized: %v", err),
				StatusCode: 401,
				ErrorType:  errorHandling.Unauthorized,
			}.ServeHTTP(w, r)
			return
		}

		claims, _ := token.Claims.(*jwt.StandardClaims)
		r = r.WithContext(context.WithValue(r.Context(), "name", claims.Subject))
		next.ServeHTTP(w, r)
	})
}

func nameVerificationMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		name := r.Context().Value("name").(string)
		if !users.NameExists(name) {
			errorHandling.ErrorHandler{
				Message:    "User not found",
				StatusCode: 401,
				ErrorType:  errorHandling.InvalidUser,
			}.ServeHTTP(w, r)
			return
		}
		next.ServeHTTP(w, r)
	})
}

func gameAuthorizationMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gameID := mux.Vars(r)["gameID"]
		username := r.Context().Value("name").(string)
		if !users.DoesBelongGameIDToUser(username, gameID) && !game.AuthorizedForLobby(username, gameID) {
			errorHandling.ErrorHandler{
				Message:    fmt.Sprintf("User not authorized for %s", gameID),
				StatusCode: 401,
				ErrorType:  errorHandling.Unauthorized,
			}.ServeHTTP(w, r)
			return
		}
		next.ServeHTTP(w, r)
	})
}
