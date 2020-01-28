package cmd

import (
	"errors"
	"log"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var startingDirectory string

// RunCmd represents the run command
var initCmd = &cobra.Command{
	Use:   "init",
	Short: "Create configuration file with defaults",
	Long: `Run bbi init to create a babylon.yaml configuration file in the current directory.
 `,
	Run: initializer,
}

func initializer(cmd *cobra.Command, args []string) {

	fs := afero.NewOsFs()

	locations := []string{}

	dir, err := cmd.Flags().GetStringSlice("dir")

	if err != nil {
		log.Fatalf("An error occurred trying to get the dir string: %s", err)
	}

	for _, l := range dir {
		//For each directory underneath the dir provided. Let's see if it's nonmemmy
		files, err := afero.ReadDir(fs, l)

		if err != nil {
			log.Fatalf("Unable to list contents of directory %s. Error is %s", dir, err)
		}

		for _, v := range files {
			//If it's a dir
			if ok, _ := afero.IsDir(fs, filepath.Join(l, v.Name())); ok {
				//And nonmem-ish
				if isPathNonMemmy(filepath.Join(l, v.Name())) {
					//Add it to the list
					locations = append(locations, filepath.Join(l, v.Name()))
				}
			}
		}

		//Let's iterate over the found locations and create the viper objects

		for _, v := range locations {
			nm, err := findNonMemBinary(v)
			if err != nil {
				log.Print(err)
				continue
			}

			identifier := filepath.Base(v)

			viper.Set("nonmem."+identifier+".default", len(locations) == 1) //If there's only one location, true
			viper.Set("nonmem."+identifier+".executable", nm)
			viper.Set("nonmem."+identifier+".home", v)
			viper.Set("nonmem."+identifier+".nmqual", hasNMQual(v))

		}
	}

	//Let's Manually set the known keys for nonmem:

	// //nm73_gf
	// viper.Set("nonmem.nm73_gf.default", false)
	// viper.Set("nonmem.nm73_gf.executable", "nmfe73")
	// viper.Set("nonmem.nm73_gf.home", "/opt/NONMEM/nm73gf")
	// viper.Set("nonmem.nm73_gf.nmqual", true)

	// //nm74_gf
	// viper.Set("nonmem.nm74_gf.default", true)
	// viper.Set("nonmem.nm74_gf.executable", "nmfe74")
	// viper.Set("nonmem.nm74_gf.home", "/opt/NONMEM/nm74gf")
	// viper.Set("nonmem.nm74_gf.nmqual", true)

	// //nm73_nmfe
	// viper.Set("nonmem.nm73_nmfe.default", false)
	// viper.Set("nonmem.nm73_nmfe.executable", "nmfe73")
	// viper.Set("nonmem.nm73_nmfe.home", "/opt/NONMEM/nm73gf_nmfe")
	// viper.Set("nonmem.nm73_nmfe.nmqual", true)

	// //nm74_nmfe
	// viper.Set("nonmem.nm74_nmfe.default", false)
	// viper.Set("nonmem.nm74_nmfe.executable", "nmfe74")
	// viper.Set("nonmem.nm74_nmfe.home", "/opt/NONMEM/nm74gf_nmfe")
	// viper.Set("nonmem.nm74_nmfe.nmqual", true)

	viper.WriteConfigAs("./babylon.yaml")

}

func init() {
	RootCmd.AddCommand(initCmd)

	const directory string = "dir"
	initCmd.Flags().StringSlice(directory, []string{}, "A directory in which to look for NonMem Installations")

}

//Evaluates if a specific directory path is nonmem-ish
func isPathNonMemmy(path string) bool {

	fs := afero.NewOsFs()

	//1 Does it contain the expected directories
	expectedDirs := []string{
		"source",
		"run",
		"license",
		"util",
	}

	for _, v := range expectedDirs {
		if ok, _ := afero.DirExists(fs, filepath.Join(path, v)); !ok {
			return false
		}
	}

	//2 Does it contain nonmem.lic?

	if ok, _ := afero.Exists(fs, filepath.Join(path, "license", "nonmem.lic")); !ok {
		return false
	}

	//3 Does it contain a nonmem executable
	located, err := afero.Glob(fs, filepath.Join(path, "run", "nmfe*"))

	if err != nil {
		return false
	}

	if len(located) == 0 {
		return false
	}

	//Are any of them executable?
	fails := 0

	for _, v := range located {
		info, err := fs.Stat(v)
		if err != nil {
			return false
		}

		executable := strings.Contains(info.Mode().String(), "x")
		if !executable {
			fails++
		}
	}

	//If none of the located files are executable, this isn't a nonmem folder
	if fails == len(located) {
		return false
	}

	return true
}

func findNonMemBinary(path string) (string, error) {
	//List all files in the path/run directory
	fs := afero.NewOsFs()
	files, err := afero.ReadDir(fs, filepath.Join(path, "run"))

	executables := []string{}

	if err != nil {
		return "", err
	}

	for _, v := range files {
		//Look for Executable files
		info, _ := fs.Stat(filepath.Join(path, "run", v.Name()))

		if err != nil {
			return "", err
		}

		if strings.Contains(info.Mode().String(), "x") {
			executables = append(executables, info.Name())
		}
	}

	//Iterate over executables to find one that matches regex
	r := regexp.MustCompile(`^nmfe[0-9]{2}$`)

	for _, v := range executables {
		if r.MatchString(v) {
			return v, nil
		}
	}

	return "", errors.New("No nonmem binary could be located in the given path. Please check again or try another directory")
}

func hasNMQual(path string) bool {
	fs := afero.NewOsFs()

	if ok, _ := afero.DirExists(fs, filepath.Join(path, "nmqual")); ok {
		return true
	}

	return false
}
