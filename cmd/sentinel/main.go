package main

import (
	"os"
	"fmt"
	"sentinel/ui"

//	"os"
	//"sentinel/docker"
	tea "github.com/charmbracelet/bubbletea"
)

const url = "https://charm.sh/"

func main() {
	p := tea.NewProgram(ui.InitModel())
	if _, err := p.Run(); err != nil {
		fmt.Println("Error:", err)
		os.Exit(1)
	}
	/*
	err := docker.CompactDirectory()
	if err != nil {
		log.Fatal(err)
	}

	err = docker.BuildImageFromTar("sentinel_test", "0.0.1")
	if err != nil {
		log.Fatal(err)
	}

	// TODO: remove context.tar from current directory
	os.Remove("context.tar")
	*/
}
