package ui

import (
	"sentinel/docker"
	//"strings"
	"fmt"
	"github.com/charmbracelet/bubbles/spinner"
	"github.com/charmbracelet/lipgloss"
	tea "github.com/charmbracelet/bubbletea"
)

type model struct {
	spinner  spinner.Model
	step string
	stepMsg string
	output []string
	err error
	done bool
}

type stepDoneMsg string
type buildOutputMsg string
type errMsg error

func InitModel() model {
	s := spinner.New()
	s.Spinner = spinner.Dot
	s.Style = lipgloss.NewStyle().Foreground(lipgloss.Color("205"))
	return model{
		spinner: s,
		step: "compact",
		stepMsg: "Compacting project for Docker build...",
		output: []string{},
		err: nil,
		done: false,
	}
}

func compactProject() tea.Msg {

	err := docker.CompactDirectory()
	if err != nil {
		return errMsg(err)
	}
	return stepDoneMsg("compact-finished")
}

func buildProject() tea.Msg {
	err := docker.BuildImageFromTar("sentinel", "latest")
	if err != nil {
		return errMsg(err)
	}
	return stepDoneMsg("build-finished")
}

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case stepDoneMsg:
		if msg == "compact-finished" {
			m.stepMsg = "Compact finished, building Docker image..."
			m.step = "build"
			return m, buildProject
		}

		if msg == "build-finished" {
			m.stepMsg = "Building finished, reading sentinel file..."
			m.step = "finished"
			m.done = true
			return m, tea.Quit
		}
	case buildOutputMsg:
		m.output = append(m.output, string(msg))
		return m, nil
	case errMsg:
		m.err = msg
		m.done = true
		return m, tea.Quit
	case tea.KeyMsg:
		if msg.Type == tea.KeyCtrlC {
			return m, tea.Quit
		}
	default:
		var cmd tea.Cmd
		m.spinner, cmd = m.spinner.Update(msg)
		return m, cmd
	}
	return m, nil
}

func (m model) View() string {
	//var b strings.Builder
	str := fmt.Sprintf("\n\n %s %s", m.spinner.View(), m.stepMsg)

	/*
	for _, line := range m.output {
		b.WriteString(line)
	}

	if m.err != nil {
		b.WriteString("\n[Error]: " + m.err.Error() + "\n")
	}
	*/

	if m.done {
		return "\nDone! [Press Ctrl+C to exit.]"
	}
	return str
}

func (m model) Init() tea.Cmd {
	return compactProject
}

