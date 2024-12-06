package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

const slurmTemplate string = `#!/bin/bash
#SBATCH --job-name={{.JobName | shquote}}
#SBATCH --output=slurm.out
#SBATCH --export=ALL
{{- if .Config.Parallel}}
#SBATCH --ntasks={{.Config.Threads}}{{end}}
#SBATCH --chdir={{.WorkingDirectory | shquote}}

{{range .Command}}{{. | shquote}} {{end}}
`

func NewSlurmCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:     "slurm [flags] <model> [<model>...]",
		Short:   "Run models via Slurm",
		Example: fmt.Sprintf(runExamples, "slurm"),
		Run:     slurm,
	}

	cmd.PersistentFlags().String("bbi_binary", "",
		"bbi executable to use in the Slurm submission script (default: current process's executable)")
	errpanic(viper.BindPFlag("bbi_binary", cmd.PersistentFlags().Lookup("bbi_binary")))

	const gridNamePrefixIdentifier string = "grid_name_prefix"
	cmd.PersistentFlags().String(gridNamePrefixIdentifier, "",
		"prefix to add to the name of submitted jobs")
	errpanic(viper.BindPFlag(gridNamePrefixIdentifier, cmd.PersistentFlags().Lookup(gridNamePrefixIdentifier)))

	return cmd
}

func slurm(_ *cobra.Command, args []string) {
	gs := &gridSpec{
		Name:          "Slurm",
		Template:      slurmTemplate,
		SubmitCommand: "sbatch",
		IgnoreError:   func(_ error, _ string) bool { return false },
	}
	gs.run(args)
}
