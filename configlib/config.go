package configlib

import (
	"os"
	"path"
	"path/filepath"
	"runtime"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/afero"
	"github.com/spf13/viper"
	"gopkg.in/yaml.v2"
)

type Config struct {
	NMVersion       string                  `mapstructure:"nm_version" yaml:"nm_version" json:"nm_version,omitempty"`
	Overwrite       bool                    `mapstructure:"overwrite" yaml:"overwrite" json:"overwrite,omitempty"`
	CleanLvl        int                     `mapstructure:"clean_lvl" yaml:"clean_lvl" json:"clean_lvl,omitempty"`
	CopyLvl         int                     `mapstructure:"copy_lvl" yaml:"copy_lvl" json:"copy_lvl,omitempty"`
	Git             bool                    `mapstructure:"git" yaml:"git" json:"git,omitempty"`
	BabylonBinary   string                  `mapstructure:"babylon_binary" yaml:"babylon_binary" json:"babylon_binary,omitempty"`
	SaveConfig      bool                    `mapstructure:"save_config" yaml:"save_config" json:"save_config,omitempty"`
	OutputDir       string                  `mapstructure:"output_dir" yaml:"output_dir" json:"output_dir,omitempty"`
	Threads         int                     `mapstructure:"threads" yaml:"threads" json:"threads,omitempty"`
	Debug           bool                    `mapstructure:"debug" yaml:"debug" json:"debug,omitempty"`
	Local           LocalDetail             `mapstructure:"local" yaml:"local" json:"local,omitempty"`
	Nonmem          map[string]NonMemDetail `mapstructure:"nonmem" json:"nonmem,omitempty" yaml:"nonmem"`
	Parallel        bool                    `mapstructure:"parallel" json:"parallel" yaml:"parallel"`
	Delay           int                     `mapstructure:"delay" yaml:"delay" json:"delay,omitempty" yaml:"delay"`
	NMQual          bool                    `mapstructure:"nmqual" yaml:"nmqual" json:"nmqual,omitempty"`
	JSON            bool                    `mapstructure:"json" yaml:"json" json:"json,omitempty"`
	Logfile         string                  `mapstructure:"log_file" yaml:"log_file" json:"log_file,omitempty"`
	NMFEOptions     NMFEOptions             `mapstructure:"nmfe_options" yaml:"nmfe_options" json:"nmfe_options,omitempty"`
	MPIExecPath     string                  `mapstructure:"mpi_exec_path" yaml:"mpi_exec_path" json:"mpi_exec_path,omitempty"`
	ParallelTimeout int                     `mapstructure:"parallel_timeout" yaml:"parallel_timeout" json:"parallel_timeout,omitempty"`
	Parafile        string                  `mapstructure:"parafile" yaml:"parafile" json:"parafile,omitempty"`
	GridNamePrefix  string                  `mapstructure:"grid_name_prefix" yaml:"grid_name_prefix" json:"grid_name_prefix,omitempty"`
}

type NonMemDetail struct {
	Home       string `mapstructure:"home" yaml:"home" json:"home,omitempty"`
	Executable string `mapstructure:"executable" yaml:"executable" json:"executable,omitempty"`
	Nmqual     bool   `mapstructure:"nmqual" yaml:"nmqual" json:"nmqual,omitempty"`
	Default    bool   `mapstructure:"default" yaml:"default" json:"default,omitempty"`
}

type LocalDetail struct {
	CreateChildDirs bool `mapstructure:"create_child_dirs" yaml:"create_child_dirs" json:"create_child_dirs,omitempty"`
}

type NMFEOptions struct {
	LicenseFile string `mapstructure:"license_file" yaml:"license_file" json:"license_file,omitempty"`
	PRSame      bool   `mapstructure:"prsame" yaml:"prsame" json:"prsame,omitempty"`
	Background  bool   `mapstructure:"background" yaml:"background" json:"background,omitempty"`
	PRCompile   bool   `mapstructure:"prcompile" yaml:"prcompile" json:"prcompile,omitempty"`
	PRDefault   bool   `mapstructure:"prdefault" yaml:"prdefault" json:"prdefault,omitempty"`
	TPRDefault  bool   `mapstructure:"tprdefault" yaml:"tprdefault" json:"tprdefault,omitempty"`
	NoBuild     bool   `mapstructure:"nobuild" yaml:"nobuild" json:"nobuild,omitempty"`
	MaxLim      int    `mapstructure:"maxlim" yaml:"maxlim" json:"maxlim,omitempty"` //Default (empty value) is 3
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
	viper.SetDefault("clean_lvl", 1)
	viper.SetDefault("git", true)
	viper.SetDefault("one_est", false)
	viper.SetDefault("threads", runtime.NumCPU())
}

//SaveConfig takes the viper settings and writes them to a file in the original path
func SaveConfig(configpath string) {
	if viper.GetBool("saveConfig") {
		viper.WriteConfigAs(path.Join(configpath, "babylon.yaml"))
	}
}

func WriteViperConfig(path string, sge bool, config Config) error {
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
