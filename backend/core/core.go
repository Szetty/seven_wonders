package core

import (
	"bufio"
	"context"
	"fmt"
	"github.com/Szetty/seven_wonders/backend/common"
	"google.golang.org/grpc"
	"io"
	"os"
	"os/exec"
	"strings"
)

const coreAddressPrefix = "127.0.0.1:"

var logger = common.NewLogger("Core")

type Server struct {
	address string
}

type requestFunction func(ServiceClient, context.Context) (interface{}, error)

func StartCoreServer() (*Server, error) {
	port, err := getRandomOpenPort()
	if err != nil {
		return nil, fmt.Errorf("could not execute script for port: %v", err)
	}
	coreAddress := coreAddressPrefix + port
	cmd := exec.Command("./core_server")
	cmd.Env = os.Environ()
	cmd.Env = append(cmd.Env, fmt.Sprintf("PORT=%s", port))
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, fmt.Errorf("could not create stdout pipe for core server: %v", err)
	}
	stderr, err := cmd.StderrPipe()
	if err != nil {
		return nil, fmt.Errorf("could not create stderr pipe for core server: %v", err)
	}
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("could not start core server: %v", err)
	}
	scanner := bufio.NewScanner(io.MultiReader(stdout, stderr))
	if scanned := scanner.Scan(); !scanned {
		return nil, fmt.Errorf("could not get output from core server: %v", scanner.Err())
	}
	logger.Info(scanner.Text())
	go handleOutputFromCore(*scanner)
	return &Server{address: coreAddress}, nil
}

func Ping(server Server) (string, error) {
	reqFn := func(client ServiceClient, ctx context.Context) (interface{}, error) {
		return client.Ping(ctx, &PingRequest{Name: "Backend -> Frontend"})
	}
	response, err := sendAndReceive(server.address, reqFn)
	if err != nil {
		return "", fmt.Errorf("ping failed: %v", err)
	}
	pong := response.(*PingReply)
	logger.Infof("Got pong from core: %v", pong.Message)
	return pong.Message, nil
}

func getRandomOpenPort() (string, error) {
	portCmd := exec.Command("./scripts/find_unused_port.sh")
	portBytes, err := portCmd.Output()
	return strings.TrimSpace(string(portBytes)), err
}

func sendAndReceive(address string, reqFn requestFunction) (interface{}, error) {
	conn, err := grpc.Dial(address, grpc.WithInsecure())
	if err != nil {
		return nil, fmt.Errorf("could not open connection to core server: %v", err)
	}
	defer func() {
		err := conn.Close()
		if err != nil {
			logger.Errorf("Could not close connection: %v", err)
		}
	}()
	client := NewServiceClient(conn)
	response, err := reqFn(client, context.Background())
	if err != nil {
		return nil, fmt.Errorf("request failed: %v", err)
	}
	return response, nil
}

func handleOutputFromCore(scanner bufio.Scanner) {
	scanned := true
	for scanned {
		scanned = scanner.Scan()
		if scanner.Err() != nil {
			logger.Errorf("Core read error: %v", scanner.Err())
		} else {
			logger.Info(scanner.Text())
		}
	}
}
