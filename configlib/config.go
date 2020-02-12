package configlib

import (
	log "github.com/sirupsen/logrus"
	"github.com/spf13/afero"
	"github.com/spf13/viper"
	"gopkg.in/yaml.v2"
	"os"
	"path"
	"path/filepath"
	"runtime"
)

//Whenever
var AvailableConfiguration Config
var ConfigurationLoaded bool = false

type Config struct {
	NMVersion       string                  `yaml:"nmVersion" json:"nm_version,omitempty"`
	Overwrite       bool                    `yaml:"overwrite" json:"overwrite,omitempty"`
	CleanLvl        int                     `yaml:"cleanLvl" json:"clean_lvl,omitempty"`
	CopyLvl         int                     `yaml:"copyLvl" json:"copy_lvl,omitempty"`
	Git             bool                    `yaml:"git" json:"git,omitempty"`
	BabylonBinary   string                  `yaml:"babylonbinary" json:"babylon_binary,omitempty"`
	SaveConfig      bool                    `yaml:"saveConfig" json:"save_config,omitempty"`
	OutputDir       string                  `yaml:"outputDir" json:"output_dir,omitempty"`
	Threads         int                     `yaml:"threads" json:"threads,omitempty"`
	Debug           bool                    `yaml:"debug" json:"debug,omitempty"`
	Local           LocalDetail             `mapstructure:"local" yaml:"local" json:"local,omitempty"`
	Nonmem          map[string]NonMemDetail `mapstructure:"nonmem" json:"nonmem,omitempty" yaml:"nonmem"`
	Parallel        bool                    `mapstructure:"parallel" json:"parallel" yaml:"parallel"`
	Delay           int                     `yaml:"delay" json:"delay,omitempty" yaml:"delay"`
	NMQual          bool                    `yaml:"nmqual" json:"nmqual,omitempty"`
	JSON            bool                    `yaml:"json_logging" json:"json_logging,omitempty"`
	Logfile         string                  `yaml:"log_file" json:"log_file,omitempty"`
	NMFEOptions     NMFEOptions             `yaml:"nmfe_optiions" json:"nmfe_options,omitempty" mapstructure:"nmfeoptions"`
	MPIExecPath     string                  `yaml:"mpiExecPath" json:"mpiExecPath,omitempty"`
	ParallelTimeout int                     `yaml:"parallel_timeout" json:"parallel_timeout,omitempty"`
	Parafile        string                  `yaml:"parafile" json:"parafile,omitempty"`
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

type NMFEOptions struct {
	LicenseFile string `yaml:"license_file" json:"license_file,omitempty" mapstructure:"licfile"`
	PRSame      bool   `yaml:"prsame" json:"prsame,omitempty"`
	Background  bool   `yaml:"background" json:"background,omitempty"`
	PRCompile   bool   `yaml:"prcompile" json:"prcompile,omitempty"`
	NoBuild     bool   `yaml:"nobuild" json:"nobuild,omitempty"`
	MaxLim      int    `yaml:"maxlim" jason:"maxlim,omitempty"` //Default (empty value) is 100
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

//SaveConfig takes the viper settings and writes them to a file in the original path
func SaveConfig(configpath string) {
	if viper.GetBool("saveConfig") {
		viper.WriteConfigAs(path.Join(configpath, "babylon.yaml"))
	}
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

	log.Debugf("Requested save of config file %s", path)

	err := config.RenderYamlToFile(path)

	if err != nil {
		return err
	}

	return nil
}

func ReadSpecifiedFileIntoConfigStruct(config string) (Config, error) {
	var returnConfig Config

	file, err := os.Open(config)
	if err != nil {
		return returnConfig, err
	}
	defer file.Close()

	err = viper.ReadConfig(file)

	if err != nil {
		log.Errorf("An error occurred trying to read the file %s into viper", config)
		return returnConfig, err
	}

	err = viper.Unmarshal(&returnConfig)

	if err != nil {
		log.Errorf("Unable to read file %s into viper!", config)
		return returnConfig, err
	}

	return returnConfig, nil
}

func LocateAndReadConfigFile() Config {

	var config Config

	if len(viper.GetString("config")) == 0 {
		log.Debug("No config has been specified. Attempting to load default")
		currentDir, err := os.Getwd()

		if err != nil {
			log.Fatalf("No specific config file provided, and we couldn't get the current working directory for some reason: %s", err)
		}

		config, err = ReadSpecifiedFileIntoConfigStruct(filepath.Join(currentDir, "babylon.yaml"))

		if err != nil {
			log.Fatalf("We couldn't open and read the details from the default configuration file location: %s", err)
		}

		log.Infof("Successfully loaded default configuration from %s", filepath.Join(currentDir, "babylon.yaml"))
	}

	//Config provided
	if len(viper.GetString("config")) > 0 {
		log.Debugf("A config file has been specified at %s", viper.GetString("config"))
		var err error
		config, err = ReadSpecifiedFileIntoConfigStruct(viper.GetString("config"))

		if err != nil {
			log.Fatalf("Configuration file provided at %s could not be loaded. Error is: %s ", viper.GetString("config"), err)
		}

		log.Infof("Successfully loaded specified configuration from %s", viper.GetString("config"))
	}

	return config
}
