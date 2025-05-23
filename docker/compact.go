package docker

import (
	"archive/tar"
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
)

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

func CompactDirectory() error {
	var buffer bytes.Buffer
	tarWriter := tar.NewWriter(&buffer)

	ignoredFiles, err := listGitIgnoredFiles()
	if err != nil {
		return err
	}

	for _, item := range ignoredFiles {
		fileInfo, err := os.Stat(item)
		if item == "" {
			continue
		}

		if err != nil {
			return err
		}
		buildFileInfoAndHeader(tarWriter, item, fileInfo)
	}

	if err := tarWriter.Close(); err != nil {
		return err
	}

	tarBytes := buffer.Bytes()

	err = os.WriteFile("context.tar", tarBytes, 0644)

	if err != nil {
		panic(err)
	}

	fmt.Printf("context.tar generated with %d bytes\n", len(tarBytes))
	return nil
}
