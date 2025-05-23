package docker

import (
	"os"
	"sentinel/ignoreparser"
)


func listGitIgnoredFiles() ([]string, error) {
	currentDir, err := os.Getwd()
	if err != nil {
		return nil, err
	}
	ignoredFiles, err := ignoreparser.ListFilesByGitIgnore(currentDir)
	if err != nil {
		return nil, err
	}

	return ignoredFiles, nil
}
