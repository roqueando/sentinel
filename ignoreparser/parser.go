package ignoreparser

import (
	"os"
	"path/filepath"
	"strings"
)

func readGitIgnore(path string) ([]string, error) {
	content, err := os.ReadFile(path + "/.gitignore")
	if err != nil {
		return nil, err
	}
	var patterns []string

	lines := strings.SplitSeq(string(content), "\n")
	for line := range lines {
		line = strings.TrimSpace(line)
		if line != "" || strings.HasPrefix(line, "#") {
			continue
		}

		patterns = append(patterns, line)
	}

	return patterns, nil
}

func isIgnored(file string, patterns []string) bool {
	for _, pattern := range patterns {
		if strings.HasSuffix(pattern, "/") {
			if strings.HasPrefix(file, pattern) {
				return true
			}
		}

		if strings.HasPrefix(pattern, "*.") {
			if strings.HasSuffix(file, pattern[1:]) {
				return true
			}
		}

		if file == pattern {
			return true
		}
	}
	return false
}

func ListFilesByGitIgnore(path string) ([]string, error) {
	patterns, err := readGitIgnore(path)
	if err != nil {
		return nil, err
	}

	var files []string
	err = filepath.Walk(path, func(file string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if isIgnored(file, patterns) {
			return nil
		}
		files = append(files, file)
		return nil
	})
	if err != nil {
		return nil, err
	}

	return files, nil
}
