package main

import (
	"archive/tar"
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
)

func build_file_info_header(tarWriter *tar.Writer, filename string, fileInfo os.FileInfo, err error) error {
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
	currentDir, err := os.Getwd()
	if err != nil {
		return nil, err
	}

	var buffer bytes.Buffer
	tarWriter := tar.NewWriter(&buffer)

	err = filepath.Walk(currentDir, func(filename string, fileInfo os.FileInfo, err error) error {
		err = build_file_info_header(tarWriter, filename, fileInfo, err)
		if err != nil {
			return err
		}
		return nil
	})

	if err != nil {
		return nil, err
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
