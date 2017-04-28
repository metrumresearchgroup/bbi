package configlib

import (
	"fmt"

	"runtime"

	"github.com/spf13/viper"
)

// LoadGlobalConfig loads nonmemutils configuration into the global Viper
func LoadGlobalConfig(configFilename string) error {
	viper.SetConfigName(configFilename)
	viper.SetConfigType("toml")
	viper.AutomaticEnv()
	viper.SetEnvPrefix("bab")
	viper.AddConfigPath(".")
	viper.AddConfigPath("$HOME")
	err := viper.ReadInConfig()
	if err != nil {
		if _, ok := err.(viper.ConfigParseError); ok {
			return err
		}
		loadDefaultSettings() // still load default settings as don't need a config file
		return fmt.Errorf("unable to locate Config file. (%s)", err)
	}

	loadDefaultSettings()
	return nil
}

func loadDefaultSettings() {
	viper.SetDefault("cacheDir", "nmcache")
	viper.SetDefault("cacheExe", "")
	viper.SetDefault("cleanLvl", 2)
	viper.SetDefault("copyLvl", 2)
	viper.SetDefault("gitignoreLvl", 1)
	viper.SetDefault("git", true)
	viper.SetDefault("nmExecutable", "nmfe74")
	viper.SetDefault("noBuild", false)
	viper.SetDefault("threads", runtime.NumCPU())
}
