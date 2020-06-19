package test

import "testing"

func TestExample(t *testing.T) {
	if 1 == 2 {
		t.Errorf("Failed")
	}
}
