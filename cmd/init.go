package cmd

import (
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// RunCmd represents the run command
var initCmd = &cobra.Command{
	Use:   "init",
	Short: "Setups up configuration file with defaults",
	Long: `run bbi init to create a babylon.yaml configuration file in the current directory.
 `,
	Run: initializer,
}

func initializer(cmd *cobra.Command, args []string) {
	//Let's Manually set the known keys for nonmem:

	//nm73_gf
	viper.Set("nonmem.nm73_gf.default", false)
	viper.Set("nonmem.nm73_gf.executable", "nmfe73")
	viper.Set("nonmem.nm73_gf.home", "/opt/NONMEM/nm73gf")
	viper.Set("nonmem.nm73_gf.nmqual", true)

	//nm74_gf
	viper.Set("nonmem.nm74_gf.default", true)
	viper.Set("nonmem.nm74_gf.executable", "nmfe74")
	viper.Set("nonmem.nm74_gf.home", "/opt/NONMEM/nm74gf")
	viper.Set("nonmem.nm74_gf.nmqual", true)

	//nm73_nmfe
	viper.Set("nonmem.nm73_nmfe.default", false)
	viper.Set("nonmem.nm73_nmfe.executable", "nmfe73")
	viper.Set("nonmem.nm73_nmfe.home", "/opt/NONMEM/nm73gf_nmfe")
	viper.Set("nonmem.nm73_nmfe.nmqual", false)

	//nm74_nmfe
	viper.Set("nonmem.nm74_nmfe.default", false)
	viper.Set("nonmem.nm74_nmfe.executable", "nmfe74")
	viper.Set("nonmem.nm74_nmfe.home", "/opt/NONMEM/nm74gf_nmfe")
	viper.Set("nonmem.nm74_nmfe.nmqual", false)

	viper.WriteConfigAs("./babylon.yaml")

}

func init() {
	RootCmd.AddCommand(initCmd)
}
