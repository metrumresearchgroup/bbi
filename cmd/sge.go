package cmd

import (
	"fmt"
	"strings"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

const sgeTemplate string = `#!/bin/bash
#$ -N {{.JobName | shquote}}
#$ -V
#$ -j y
{{- if .Config.Parallel}}
#$ -pe orte {{.Config.Threads}}{{end}}
#$ -wd {{.WorkingDirectory | shquote}}

{{range .Command}}{{. | shquote}} {{end}}
`

func NewSgeCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:     "sge [flags] <model> [<model>...]",
		Short:   "Run models on the Sun Grid Engine",
		Example: fmt.Sprintf(runExamples, "sge"),
		Run:     sge,
	}

	//String Variables
	cmd.PersistentFlags().String("bbi_binary", "",
		"bbi executable to use in the SGE submission script (default: current process's executable)")
	errpanic(viper.BindPFlag("bbi_binary", cmd.PersistentFlags().Lookup("bbi_binary")))

	const gridNamePrefixIdentifier string = "grid_name_prefix"
	cmd.PersistentFlags().String(gridNamePrefixIdentifier, "",
		"prefix to add to the name of submitted jobs")
	errpanic(viper.BindPFlag(gridNamePrefixIdentifier, cmd.PersistentFlags().Lookup(gridNamePrefixIdentifier)))

	return cmd
}

func sge(_ *cobra.Command, args []string) {
	gs := &gridSpec{
		Name:          "SGE",
		Template:      sgeTemplate,
		SubmitCommand: "qsub",
		IgnoreError:   sgeIgnoreError,
	}
	gs.run(args)
}

func sgeIgnoreError(_ error, output string) bool {
	// Ignore the error that occurs when no workers are available yet.
	return strings.Contains(output, "job is not allowed to run in any queue")
}
