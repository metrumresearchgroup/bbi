package cmd

import (
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

const runLongDescription string = `run nonmem model(s), for example: 
bbi nonmem run <local|sge> run001.mod
bbi nonmem run  --cleanLvl=1 <local|sge> run001.mod run002.mod
bbi nonmem run <local|sge> run[001:006].mod // expand to run001.mod run002.mod ... run006.mod local
bbi nonmem run <local|sge> .// run all models in directory
 `

// RunCmd represents the run command
var runCmd = &cobra.Command{
	Use:   "run",
	Short: "run a (set of) models locally or on the grid",
	Long:  runLongDescription,
	Run:   run,
}

func run(cmd *cobra.Command, args []string) {
	println(runLongDescription)
}

func init() {

	//String Variables
	runCmd.PersistentFlags().String("cacheDir", "", "directory path for cache of nonmem executables for NM7.4+")
	viper.BindPFlag("cacheDir", runCmd.PersistentFlags().Lookup("cacheDir"))

	runCmd.PersistentFlags().String("cacheExe", "", "name of executable stored in cache")
	viper.BindPFlag("cacheExe", runCmd.PersistentFlags().Lookup("cacheExe"))

	runCmd.PersistentFlags().String("saveExe", "", "what to name the executable when stored in cache")
	viper.BindPFlag("saveExe", runCmd.PersistentFlags().Lookup("saveExe"))

	runCmd.PersistentFlags().String("outputDir", "{{ .Name }}", "Go template for the output directory to use for storging details of each executed model")
	viper.BindPFlag("outputDir", runCmd.PersistentFlags().Lookup("outputDir"))
	viper.SetDefault("outputDir", "{{ .Name }}")

	//Int Variables
	runCmd.PersistentFlags().Int("cleanLvl", 1, "clean level used for file output from a given (set of) runs")
	viper.BindPFlag("cleanLvl", runCmd.PersistentFlags().Lookup("cleanLvl"))
	viper.SetDefault("cleanLvl", 1)

	runCmd.PersistentFlags().Int("copyLvl", 0, "copy level used for file output from a given (set of) runs")
	viper.BindPFlag("copyLvl", runCmd.PersistentFlags().Lookup("copyLvl"))
	viper.SetDefault("copyLvl", 0)

	runCmd.PersistentFlags().Int("gitignoreLvl", 0, "gitignore lvl for a given (set of) runs")
	viper.BindPFlag("gitignoreLvl", runCmd.PersistentFlags().Lookup("gitignoreLvl"))
	viper.SetDefault("gitignoreLvl", 1)

	//Bool Variables
	runCmd.PersistentFlags().Bool("git", false, "whether git is used")
	viper.BindPFlag("git", runCmd.PersistentFlags().Lookup("git"))
	viper.SetDefault("git", true)

	runCmd.PersistentFlags().Bool("overwrite", false, "Whether or not to remove existing output directories if they are present")
	viper.BindPFlag("overwrite", runCmd.PersistentFlags().Lookup("overwrite"))
	viper.SetDefault("overwrite", false)

	const saveconfig string = "saveConfig"
	runCmd.PersistentFlags().Bool(saveconfig, true, "Whether or not to save the existing configuration to a file with the model")
	viper.BindPFlag(saveconfig, runCmd.PersistentFlags().Lookup(saveconfig))

	nonmemCmd.AddCommand(runCmd)

}
