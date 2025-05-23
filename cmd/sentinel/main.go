package main

import (
	"log"
	"sentinel/docker"
)

func main() {
	err := docker.CompactDirectory()
	if err != nil {
		log.Fatal(err)
	}

	err = docker.BuildImageFromTar("sentinel_test", "0.0.1")
	if err != nil {
		log.Fatal(err)
	}

	// TODO: remove context.tar from current directory
}
