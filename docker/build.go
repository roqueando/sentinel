package docker

import (
	"context"
	"bufio"
	"strings"
	"fmt"
	"io"
	"net"
	"net/http"
	"encoding/json"
	"os"
)

type DockerStreamLine struct {
	Stream string `json:"stream"`
}


func formatDockerBuildLine(line string) string {
	var streamLine DockerStreamLine

	line = strings.TrimSpace(line)

	err := json.Unmarshal([]byte(line), &streamLine)
	if err != nil {
		return ""
	}

	return streamLine.Stream
}

// FIXME: enable a option to see stream if is verbose
func BuildImageFromTar(imageName, tag string) error {
	socketPath := "/var/run/docker.sock"
	currentDir, err := os.Getwd()
	if err != nil {
		return err
	}

	tarPath := currentDir + "/" + "context.tar"

	tarFile, err := os.Open(tarPath)
	if err != nil {
		return err
	}
	defer tarFile.Close()

	// FIXME: pass all transport creation to a builder function
	transport := &http.Transport{
		// TODO: create a private function for this
		DialContext: func(_ context.Context, _, _ string) (net.Conn, error) {
			return net.Dial("unix", socketPath)
		},
	}
	client := &http.Client{Transport: transport}

	url := fmt.Sprintf("http://docker/build?t=%s:%s", imageName, tag)

	req, err := http.NewRequest("POST", url, tarFile)
	if err != nil {
		return fmt.Errorf("error when creating request: %w", err)
	}

	req.Header.Set("Content-Type", "application/x-tar")

	resp, err := client.Do(req)
	if err != nil {
		return fmt.Errorf("error on executing request: %w", err)
	}

	defer resp.Body.Close()

	reader := bufio.NewReader(resp.Body)

	// FIXME: that will be a function to implement
	for {
		line, err := reader.ReadBytes('\n')

		if err == io.EOF {
			break
		}

		if err != nil {
			panic(err)
		}

		formatted := formatDockerBuildLine(string(line))
		fmt.Fprint(os.Stdout, formatted)
	}
	_, err = io.Copy(os.Stdout, resp.Body)

	if err != nil {
		return fmt.Errorf("error on streaming response: %w", err)
	}
	return nil
}
