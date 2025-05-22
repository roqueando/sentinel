package main

import (
	"archive/tar"
	"io"
	"bytes"
	"fmt"
	"log"
	"os"
	"path/filepath"
)

func compact_dir(srcDir string) ([]byte, error) {
	var buf bytes.Buffer
	tw := tar.NewWriter(&buf)

	err := filepath.Walk(srcDir, func(file string, fi os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if fi.IsDir() {
			return nil
		}
		f, err := os.Open(file)
		if err != nil {
			return err
		}
		defer f.Close()
		relPath, err := filepath.Rel(srcDir, file)
		if err != nil {
			return err
		}
		relPath = filepath.ToSlash(relPath)
		header, err := tar.FileInfoHeader(fi, relPath)
		if err != nil {
			return err
		}
		header.Name = relPath

		if err := tw.WriteHeader(header); err != nil {
			return err
		}
		if _, err := io.Copy(tw, f); err != nil {
			return err
		}

		return nil
	})
	if err != nil {
		return nil, err
	}
	if err := tw.Close(); err != nil {
		return nil, err
	}

	return buf.Bytes(), nil
}
func main() {
	currentDir, err := os.Getwd()
	if err != nil {
		log.Fatal(err)
	}
	tarBytes, err := compact_dir(currentDir)
	if err != nil {
		log.Fatal(err)
	}

	err = os.WriteFile("context.tar",tarBytes, 0644)
	if err != nil {
		panic(err)
	}

	fmt.Println("TAR gerado com sucesso")
}
