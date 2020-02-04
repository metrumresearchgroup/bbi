package cmd

import (
	"github.com/google/uuid"
	"github.com/metrumresearchgroup/babylon/configlib"
	"os"
	"path"
	"reflect"
	"testing"
)

func Test_doesDirectoryContainOutputFiles(t *testing.T) {

	good := uuid.New().String()
	bad := uuid.New().String()

	os.Mkdir(good, 0755)
	os.Mkdir(bad, 0755)
	emptyFile(path.Join(bad, "meow.cov"))

	type args struct {
		path      string
		modelname string
	}
	tests := []struct {
		name string
		args args
		want bool
	}{
		{
			name: "Empty Directory",
			args: args{
				path:      good,
				modelname: "meow",
			},
			want: false,
		},
		{
			name: "Contains covariate file",
			args: args{
				path:      bad,
				modelname: "meow",
			},
			want: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := doesDirectoryContainOutputFiles(tt.args.path, tt.args.modelname); got != tt.want {
				t.Errorf("doesDirectoryContainOutputFiles() = %v, want %v", got, tt.want)
			}
		})
	}

	//Cleanup the directories
	os.RemoveAll(good)
	os.RemoveAll(bad)
}

func emptyFile(path string) {
	empty, _ := os.Create(path)
	empty.Close()
}

func Test_processNMFEOptions(t *testing.T) {

	type args struct {
		config *configlib.Config
	}
	tests := []struct {
		name string
		args args
		want []string
	}{
		{
			name: "Normal Operation",
			args: args{
				config: &configlib.Config{
					NMVersion:     "",
					Overwrite:     false,
					CleanLvl:      0,
					CopyLvl:       0,
					Git:           false,
					BabylonBinary: "",
					SaveConfig:    false,
					OutputDir:     "",
					Threads:       0,
					Debug:         false,
					Local:         configlib.LocalDetail{},
					Nonmem:        nil,
					Parallel:      configlib.ParallelConfig{},
					Delay:         0,
					NMQual:        false,
					JSON:          false,
					Logfile:       "",
					NMFEOptions: configlib.NMFEOptions{
						LicenseFile: "/tmp/nonmem.lic",
						PRSame:      false,
						Background:  true,
						PRCompile:   false,
						NoBuild:     true,
						MaxLim:      1,
					},
				},
			},
			want: []string{
				"-licfile=/tmp/nonmem.lic",
				"-background",
				"-nobuild",
				"maxlim=1",
			},
		},
		{
			name: "All booleans, no text",
			args: args{
				config: &configlib.Config{
					NMVersion:     "",
					Overwrite:     false,
					CleanLvl:      0,
					CopyLvl:       0,
					Git:           false,
					BabylonBinary: "",
					SaveConfig:    false,
					OutputDir:     "",
					Threads:       0,
					Debug:         false,
					Local:         configlib.LocalDetail{},
					Nonmem:        nil,
					Parallel:      configlib.ParallelConfig{},
					Delay:         0,
					NMQual:        false,
					JSON:          false,
					Logfile:       "",
					NMFEOptions: configlib.NMFEOptions{
						LicenseFile: "",
						PRSame:      true,
						Background:  true,
						PRCompile:   true,
						NoBuild:     false,
						MaxLim:      100,
					},
				},
			},
			want: []string{
				"-prsame",
				"-background",
				"-prcompile",
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := processNMFEOptions(tt.args.config); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("processNMFEOptions() = %v, want %v", got, tt.want)
			}
		})
	}
}
