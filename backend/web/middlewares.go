package web

import (
	"context"
	"fmt"
	"github.com/Szetty/seven_wonders/backend/common"
	"github.com/dgrijalva/jwt-go"
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
		authorization := r.Header.Get("Authorization")
		if !strings.HasPrefix(authorization, prefix) {
			ErrorHandler{
				message: fmt.Sprintf("Invalid authorization header: %s", authorization),
				statusCode: 401,
				errorType: Unauthorized,
			}.ServeHTTP(w, r)
			return
		}
		jwtToken := strings.TrimPrefix(authorization, prefix)
		token, err := jwt.ParseWithClaims(
			jwtToken,
			&jwt.StandardClaims{},
			func (token *jwt.Token) (i interface{}, err error) {
				return []byte(common.JWT_SECRET), nil
			},
		)
		if err != nil {
			ErrorHandler{
				message: fmt.Sprintf("Unauthorized: %v", err),
				statusCode: 401,
				errorType: Unauthorized,
			}.ServeHTTP(w, r)
			return
		}

		claims, _ := token.Claims.(*jwt.StandardClaims)
		r = r.WithContext(context.WithValue(r.Context(), "name", claims.Subject))
		next.ServeHTTP(w, r)
	})
}