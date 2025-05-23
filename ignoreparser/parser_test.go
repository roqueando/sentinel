package ignoreparser

import (
	"testing"
)

func TestIsIgnored(t *testing.T) {
	tests := []struct {
		file     string
		patterns []string
		expected bool
	}{
		{"file.txt", []string{"*.txt"}, true},
		{"file.txt", []string{"*.go"}, false},
		{"dir/file.txt", []string{"dir/"}, true},
		{"dir/file.txt", []string{"dir/file.txt"}, true},
	}

	for _, test := range tests {
		result := isIgnored(test.file, test.patterns)
		if result != test.expected {
			t.Errorf("Expected %v, got %v for file %s with patterns %v", test.expected, result, test.file, test.patterns)
		}
	}
}
