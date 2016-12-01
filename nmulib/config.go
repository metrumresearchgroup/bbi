package nmulib

import (
	"fmt"

	"github.com/spf13/viper"
)

// LoadGlobalConfig loads nonmemutils configuration into the global Viper
func LoadGlobalConfig(configFilename string) error {
	// viper.SetConfigName("nmuconfig")
	// viper.SetConfigType("toml")
	viper.AutomaticEnv()
	viper.SetEnvPrefix("nmu")
	viper.SetConfigFile(configFilename)
	viper.AddConfigPath(".")
	err := viper.ReadInConfig()
	if err != nil {
		if _, ok := err.(viper.ConfigParseError); ok {
			return err
		}
		return fmt.Errorf("Unable to locate Config file. (%s)\n", err)
	}

	loadDefaultSettings()
	return nil
}

func loadDefaultSettings() {
	viper.SetDefault("cleanLvl", 2)
	viper.SetDefault("copyLvl", 2)
	viper.SetDefault("gitignoreLvl", 1)
	viper.SetDefault("useGit", true)
}
