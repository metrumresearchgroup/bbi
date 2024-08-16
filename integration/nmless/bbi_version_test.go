package nmless

import (
	"context"
	"strings"
	"testing"

	"github.com/metrumresearchgroup/wrapt"

	bi "github.com/metrumresearchgroup/bbi/integration"
)

func TestVersion(tt *testing.T) {
	t := wrapt.WrapT(tt)

	out, err := bi.ExecuteCommand(context.Background(), "bbi", "version")
	if err != nil {
		t.Fatal(err)
	}

	t.A.NotEqual(strings.TrimSuffix(out, "\n"), "")
}
