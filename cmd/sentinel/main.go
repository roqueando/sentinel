package main

import (
	"archive/tar"
	"bytes"
	"fmt"
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
	})
}
func main() {
	fmt.Println("hello there!")
}
