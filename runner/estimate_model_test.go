package runner

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestDefaultEstimateModel(t *testing.T) {
	r := RunSettings{
		OutputDir: "{{ .Name }}",
	}

	out, err := processDirectoryTemplate("cat", r)

	assert.Nil(t, err)
	assert.NotEmpty(t, out)
	assert.Equal(t, "cat", out)
}

func TestCustomEstimateModel(t *testing.T) {
	r := RunSettings{
		OutputDir: "oh_{{ .Name }}_hai",
	}

	out, err := processDirectoryTemplate("cat", r)

	assert.Nil(t, err)
	assert.NotEmpty(t, out)
	assert.Equal(t, out, "oh_cat_hai")
}

//Should just get back the provided output dir.
func TestLogiclessTemplateForEstimateModel(t *testing.T) {
	r := RunSettings{
		OutputDir: "notatemplate",
	}

	out, err := processDirectoryTemplate("cat", r)

	assert.NotEmpty(t, out)
	assert.Equal(t, out, r.OutputDir)
	assert.Nil(t, err)
}
