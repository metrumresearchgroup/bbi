package runner

import (
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestDefaultEstimateModel(tt *testing.T) {
	testId := "UNIT-RUN-003"
	tt.Run(testId, func(tt *testing.T) {
		t := wrapt.WrapT(tt)

		r := RunSettings{
			OutputDir: "{{ .Name }}",
		}

		out, err := processDirectoryTemplate("cat", r)

		t.A.Nil(err)
		t.A.NotEmpty(out)
		t.A.Equal("cat", out)
	})
}

func TestCustomEstimateModel(tt *testing.T) {
	testId := "UNIT-RUN-004"
	tt.Run(testId, func(tt *testing.T) {
		t := wrapt.WrapT(tt)
		r := RunSettings{
			OutputDir: "oh_{{ .Name }}_hai",
		}

		out, err := processDirectoryTemplate("cat", r)

		t.A.Nil(err)
		t.A.NotEmpty(out)
		t.A.Equal(out, "oh_cat_hai")
	})
}

//Should just get back the provided output dir.
func TestLogiclessTemplateForEstimateModel(tt *testing.T) {
	testId := "UNIT-RUN-005"
	tt.Run(testId, func(tt *testing.T) {
		t := wrapt.WrapT(tt)
		r := RunSettings{
			OutputDir: "notatemplate",
		}

		out, err := processDirectoryTemplate("cat", r)

		t.A.NotEmpty(out)
		t.A.Equal(out, r.OutputDir)
		t.A.Nil(err)
	})
}
