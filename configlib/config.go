package configlib

import (
	"fmt"
	"github.com/metrumresearchgroup/babylon/utils"
	"path"
	"path/filepath"
	"runtime"
	"strings"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

type Config struct {
	NMVersion     string                  `yaml:"nmVersion" json:"nm_version,omitempty"`
	Overwrite     bool                    `yaml:"overwrite" json:"overwrite,omitempty"`
	CleanLvl      int                     `yaml:"cleanLvl" json:"clean_lvl,omitempty"`
	CopyLvl       int                     `yaml:"copyLvl" json:"copy_lvl,omitempty"`
	Git           bool                    `yaml:"git" json:"git,omitempty"`
	BabylonBinary string                  `yaml:"babylonbinary" json:"babylon_binary,omitempty"`
	SaveConfig    bool                    `yaml:"saveConfig" json:"save_config,omitempty"`
	OutputDir     string                  `yaml:"outputDir" json:"output_dir,omitempty"`
	Threads       int                     `yaml:"threads" json:"threads,omitempty"`
	Debug         bool                    `yaml:"debug" json:"debug,omitempty"`
	Nonmem        map[string]NonMemDetail `mapstructure:"nonmem" json:"nonmem,omitempty"`
	Parallel      ParallelConfig          `mapstructure:"parallel" json:"parallel"`
	Delay         int                     `yaml:"delay" json:"delay,omitempty"`
}

type NonMemDetail struct {
	Home       string `yaml:"home" json:"home,omitempty"`
	Executable string `yaml:"executable" json:"executable,omitempty"`
	Nmqual     bool   `yaml:"nmqual" json:"nmqual,omitempty"`
	Default    bool   `yaml:"default" json:"default,omitempty"`
}

type ParallelConfig struct {
	Parallel    bool   `yaml:"parallel" json:"parallel,omitempty"`
	Nodes       int    `yaml:"nodes" json:"nodes,omitempty"`
	MPIExecPath string `yaml:"mpiExecPath" json:"mpiExecPath,omitempty"`
	Timeout     int    `yaml:"timeout" json:"timeout,omitempty"`
	Parafile    string `yaml:"parafile" json:"parafile,omitempty"`
}

// LoadGlobalConfig loads nonmemutils configuration into the global Viper
func LoadGlobalConfig(configFilename string) error {
	viper.SetConfigName(configFilename)
	viper.SetConfigType("yaml")
	viper.AutomaticEnv()
	viper.SetEnvPrefix("babylon")
	err := viper.ReadInConfig()
	if err != nil {
		if _, ok := err.(viper.ConfigParseError); ok {
			return err
		}
		loadDefaultSettings() // still load default settings as don't need a config file
		return nil
	}

	loadDefaultSettings()
	return nil
}

func loadDefaultSettings() {
	viper.SetDefault("cacheDir", "mdlcache")
	viper.SetDefault("cacheExe", "")
	viper.SetDefault("gitignoreLvl", 1)
	viper.SetDefault("cleanLvl", 1)
	viper.SetDefault("git", true)
	viper.SetDefault("nmExecutable", "")
	viper.SetDefault("noBuild", false)
	viper.SetDefault("oneEst", false)
	viper.SetDefault("threads", runtime.NumCPU())
}

//LocateAndReadConfigFile will take a priority based approach to loading configs starting with those closest to the model all the way out to the home directory for the users
func LocateAndReadConfigFile(modelPath string) {

	if viper.ConfigFileUsed() != "" {
		//We've already read and loaded a config. Nothing to see here. Move along.
		return
	}

	locations := []string{
		modelPath,
	}

	for _, v := range locations {
		//Add the path and try to load the config
		viper.AddConfigPath(v)
		err := viper.ReadInConfig()

		if _, ok := err.(viper.ConfigFileNotFoundError); ok {
			//No config here
			continue
		}

		//Handle parse issues
		if err, ok := err.(viper.ConfigParseError); ok {
			log.Errorf("An error occurred trying to parse the config file located at %s. Error details are %s", v, err.Error())
			continue
		}

		//Let's print out the config we loaded
		if viper.GetBool("debug") {
			lines, _ := utils.ReadLines(filepath.Join(v, "babylon.yaml"))
			log.Debugf("Contents of loaded config file are: \n%s", strings.Join(lines, "\n"))
		}

		//If no errors we return to prevent further processing
		log.Infof("Configuration file successfully loaded from %s", path.Join(v, "babylon.yml"))
		return
	}
}

func ProcessSpecifiedConfigFile() {
	//Check to see if a config was provided.
	if len(viper.GetString("config")) > 0 {
		err := LoadFileToViper(viper.GetString("config"))
		if err != nil {
			//If we specified a config and we can't load it, stop processing to allow the user to decide
			//how best to proceed.
			log.Fatalf("User specified %s as the configuration to load, but an error "+
				"happened attempting to do so : %s", viper.GetString("config"), err)
		}
	}
}

func LoadFileToViper(filepath string) error {

	//Does the path contain babylon.yaml? If so we only need the directory
	if path.Base(filepath) == "babylon.yaml" {
		filepath = path.Dir(filepath)
	}
	//Or we could provide an IO reader for reading the config

	viper.AddConfigPath(filepath)
	err := viper.ReadInConfig()

	if _, ok := err.(viper.ConfigFileNotFoundError); ok {
		//No config here
		return fmt.Errorf("path %s was provided for a configuration file, but no configurations were "+
			"located here", filepath)
	}

	//Handle parse issues
	if err, ok := err.(viper.ConfigParseError); ok {
		return fmt.Errorf("an error occurred trying to parse the config file located at %s. Error details are %s", filepath, err.Error())
	}

	return nil
}

//SaveConfig takes the viper settings and writes them to a file in the original path
func SaveConfig(configpath string) {
	if viper.GetBool("saveConfig") {
		viper.WriteConfigAs(path.Join(configpath, "babylon.yaml"))
	}
}

//UnmarshalViper collects the viper details and inserts them into the class struct
func UnmarshalViper() Config {
	c := Config{}
	viper.Unmarshal(&c)
	return c
}
