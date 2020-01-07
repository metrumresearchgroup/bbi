package configlib

import (
	"log"
	"path"
	"runtime"

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
		".",
		"$HOME",
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
			log.Printf("An error occurred trying to parse the config file located at %s. Error details are %s", v, err.Error())
			continue
		}

		//If no errors we return to prevent further processing
		log.Printf("Configuration file successfully loaded from %s", path.Join(v, "babylon.yml"))
		return
	}
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
