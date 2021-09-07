package cmd

import (
	"errors"
	"log"
	"path/filepath"
	"regexp"
	"strings"

	"bbi/configlib"

	"github.com/ghodss/yaml"

	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var startingDirectory string

// RunCmd represents the run command.
var initCmd = &cobra.Command{
	Use:   "init",
	Short: "Create configuration file with defaults",
	Long: `Run bbi init to create a bbi.yaml configuration file in the current directory.
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

	c := configlib.Config{}
	viper.Unmarshal(&c)

	yamlString, err := yaml.Marshal(c)

	if err != nil {
		log.Fatal(err)
	}

	//Write the byte array to file
	afero.WriteFile(fs, "./bbi.yaml", yamlString, 0755)
}

func init() {
	RootCmd.AddCommand(initCmd)

	const directory string = "dir"
	initCmd.Flags().StringSlice(directory, []string{}, "A directory in which to look for NonMem Installations")
}

//Evaluates if a specific directory path is nonmem-ish.
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
