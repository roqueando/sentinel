package main

import (
	"archive/tar"
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

func diff(a, b []string) []string {
	bSet := make(map[string]bool)
	for _, item := range b {
		bSet[item] = true
	}
	var diff []string

	for _, item := range a {
		if !bSet[item] {
			diff = append(diff, item)
		}
	}

	return diff
}

func listAllFiles(srcDir string) ([]string, error) {
	listAllFilesCmd := exec.Command("find", ".", "-type", "f")
	listAllFilesCmd.Dir = srcDir
	output, err := listAllFilesCmd.Output()
	if err != nil {
		return nil, err
	}

	lines := strings.Split(string(output), "\n")
	return lines, nil
}

func getGitIgnoredFiles(files []string) ([]string, error) {
	gitCmd := exec.Command("git", "check-ignore", "--stdin")

	var stdin bytes.Buffer
	stdin.WriteString(strings.Join(files, "\n"))
	gitCmd.Stdin = &stdin

	output, err := gitCmd.Output()
	if err != nil {
		return nil, err
	}

	lines := strings.Split(strings.TrimSpace(string(output)), "\n")
	return lines, nil
}

func listGitIgnoredFiles() ([]string, error) {
	currentDir, err := os.Getwd()
	if err != nil {
		return nil, err
	}
	// list all files
	allFiles, err := listAllFiles(currentDir)
	if err != nil {
		return nil, err
	}

	gitIgnoredFiles, err := getGitIgnoredFiles(allFiles)
	if err != nil {
		return nil, err
	}

	files := diff(allFiles, gitIgnoredFiles)

	return files, nil
}

func buildFileInfoAndHeader(tarWriter *tar.Writer, filename string, fileInfo os.FileInfo) error {
	currentDir, err := os.Getwd()
	if err != nil {
		return err
	}

	if fileInfo.IsDir() {
		return nil
	}

	file, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	relativePath, err := filepath.Rel(currentDir, filename)

	if err != nil {
		return err
	}

	relativePath = filepath.ToSlash(relativePath)

	header, err := tar.FileInfoHeader(fileInfo, relativePath)
	if err != nil {
		return err
	}

	header.Name = relativePath

	if err := tarWriter.WriteHeader(header); err != nil {
		return err
	}
	if _, err := io.Copy(tarWriter, file); err != nil {
		return err
	}
	return nil
}

func compact_dir() ([]byte, error) {
	var buffer bytes.Buffer
	tarWriter := tar.NewWriter(&buffer)

	ignoredFiles, err := listGitIgnoredFiles()
	if err != nil {
		return nil, err
	}

	for _, item := range ignoredFiles {
		fileInfo, err := os.Stat(item)
		if item == "" {
			continue
		}

		if err != nil {
			return nil, err
		}
		buildFileInfoAndHeader(tarWriter, item, fileInfo)
	}

	if err := tarWriter.Close(); err != nil {
		return nil, err
	}

	return buffer.Bytes(), nil
}

func main() {
	tarBytes, err := compact_dir()
	if err != nil {
		log.Fatal(err)
	}

	err = os.WriteFile("context.tar", tarBytes, 0644)
	if err != nil {
		panic(err)
	}

	fmt.Println("TAR gerado com sucesso")
}
