package configlib

import (
	"fmt"
	"github.com/metrumresearchgroup/babylon/utils"
	"github.com/spf13/afero"
	"gopkg.in/yaml.v2"
	"os"
	"path"
	"path/filepath"
	"runtime"
	"strings"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

//Whenever
var AvailableConfiguration Config
var ConfigurationLoaded bool = false

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
	Local         LocalDetail             `mapstructure:"local" yaml:"local" json:"local,omitempty"`
	Nonmem        map[string]NonMemDetail `mapstructure:"nonmem" json:"nonmem,omitempty" yaml:"nonmem"`
	Parallel      ParallelConfig          `mapstructure:"parallel" json:"parallel" yaml:"parallel"`
	Delay         int                     `yaml:"delay" json:"delay,omitempty" yaml:"delay"`
}

type NonMemDetail struct {
	Home       string `yaml:"home" json:"home,omitempty"`
	Executable string `yaml:"executable" json:"executable,omitempty"`
	Nmqual     bool   `yaml:"nmqual" json:"nmqual,omitempty"`
	Default    bool   `yaml:"default" json:"default,omitempty"`
}

type LocalDetail struct {
	CreateChildDirs bool `yaml:"create_child_dirs" json:"create_child_dirs,omitempty"`
}

type ParallelConfig struct {
	Parallel    bool   `yaml:"parallel" json:"parallel,omitempty"`
	Nodes       int    `yaml:"nodes" json:"nodes,omitempty"`
	MPIExecPath string `yaml:"mpiExecPath" json:"mpiExecPath,omitempty"`
	Timeout     int    `yaml:"timeout" json:"timeout,omitempty"`
	Parafile    string `yaml:"parafile" json:"parafile,omitempty"`
}

func (c Config) RenderYamlToFile(path string) error {

	fs := afero.NewOsFs()
	yamlBytes, err := yaml.Marshal(c)

	if err != nil {
		log.Error("An error occurred serializing the config down to yaml")
		return err
	}

	targetFile := filepath.Join(path, "babylon.yaml")

	err = afero.WriteFile(fs, targetFile, yamlBytes, 0755)

	if err != nil {
		log.Error("An error occurred trying to write the serialized config yaml to file")
		return err
	}
	return nil
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

	//Should be the new output Directory only
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
		err := LoadViperFromPath(viper.GetString("config"))
		if err != nil {
			//If we specified a config and we can't load it, stop processing to allow the user to decide
			//how best to proceed.
			log.Fatalf("User specified %s as the configuration to load, but an error "+
				"happened attempting to do so : %s", viper.GetString("config"), err)
		}
	}
}

//SaveConfig takes the viper settings and writes them to a file in the original path
func SaveConfig(configpath string) {
	if viper.GetBool("saveConfig") {
		viper.WriteConfigAs(path.Join(configpath, "babylon.yaml"))
	}
}

//UnmarshalViper collects the viper details and inserts them into the class struct
func UnmarshalViper() *Config {
	if !ConfigurationLoaded {
		c := Config{}
		viper.Unmarshal(&c)
		ConfigurationLoaded = true
		AvailableConfiguration = c
		log.Debug("Loading configuration from viper into struct and memory")
		return &AvailableConfiguration
	} else {
		log.Debug("Returning preloaded configuration")
		return &AvailableConfiguration
	}
}

//LoadViperFromPath allows the read of viper from file reader
func LoadViperFromPath(path string) error {
	log.Debugf("Attempting to load configuration from %s", path)
	viper.SetConfigType("yaml")
	filename := viper.GetString("config")
	log.Debugf("Pulled %s as the filename value from viper", viper.GetString("config"))
	if len(filename) == 0 {
		log.Debug("Config value not accurately set. Using default")
		filename = "babylon.yaml"
	}

	log.Debugf("Designated filename is %s", filename)

	config, err := os.Open(filepath.Join(path, filename))

	if err != nil {
		log.Error(err)
		return fmt.Errorf("unable to load or access the configuration file (%s) located at %s", filename, path)
	}

	err = viper.ReadConfig(config)

	if err != nil {
		log.Error(err)
		return fmt.Errorf("viper had issues parsing the configuration file provided. Details are: %s", err)
	}

	log.Infof("Configuration successfully loaded from %s", filepath.Join(path, filename))
	return nil
}

func WriteViperConfig(path string, sge bool, config *Config) error {
	if sge {

		//Set the config to overwrite false and re-write config. This ensures that the local phase will not deal with io contention
		//around the SGE output streams
		log.Debug("Updating babylon config to overwrite=false. This avoids IO contention with the grid engine for the next execution round")
		config.Overwrite = false
		config.SaveConfig = false
		config.Local.CreateChildDirs = false
	}

	//TODO: How to process variable config names

	err := config.RenderYamlToFile(path)

	if err != nil {
		return err
	}

	return nil
}
