package main

import (
	"github.com/metrumresearchgroup/bbi/cmd"
)

// buildTime  can be set from LDFLAGS during development.
var buildTime string

func main() {
	cmd.Execute(buildTime)
}
