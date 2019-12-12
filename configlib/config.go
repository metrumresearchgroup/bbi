package configlib

import (
	"runtime"

	"github.com/spf13/viper"
)

// LoadGlobalConfig loads nonmemutils configuration into the global Viper
func LoadGlobalConfig(configFilename string) error {
	viper.SetConfigName(configFilename)
	viper.SetConfigType("yaml")
	viper.AutomaticEnv()
	viper.SetEnvPrefix("babylon")
	viper.AddConfigPath(".")
	viper.AddConfigPath("$HOME")
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
	viper.SetDefault("git", true)
	viper.SetDefault("nmExecutable", "nmfe74")
	viper.SetDefault("noBuild", false)
	viper.SetDefault("oneEst", false)
	viper.SetDefault("threads", runtime.NumCPU())
}
