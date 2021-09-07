package utils

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestExpandSequence(t *testing.T) {
	type test struct {
		input    string
		expected []string
	}
	data := []test{
		{"run[001:002].mod", []string{"run001.mod", "run002.mod"}},
		{"run001.mod", []string{"run001.mod"}},
		{"run001", []string{"run001"}},
		{"run[001:002]", []string{"run001", "run002"}},
		{"run[1:3]", []string{"run1", "run2", "run3"}},
		{"run_1_[1:3]", []string{"run_1_1", "run_1_2", "run_1_3"}},
		{"[1:10].ctl", []string{"1.ctl", "2.ctl", "3.ctl", "4.ctl", "5.ctl", "6.ctl", "7.ctl", "8.ctl", "9.ctl", "10.ctl"}},
	}

	for _, d := range data {
		res, err := ExpandNameSequence(d.input)
		if err != nil {
			t.Error(err)
		}
		assert.Equal(t, d.expected, res)
	}
}
