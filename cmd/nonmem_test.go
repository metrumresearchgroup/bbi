package cmd

import (
	"os"
	"path"
	"path/filepath"
	"reflect"
	"testing"

	"github.com/google/uuid"
	"github.com/metrumresearchgroup/babylon/configlib"
	"github.com/spf13/afero"
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
				"-maxlim=1",
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

func Test_modelDataFile(t *testing.T) {
	type args struct {
		modelLines []string
	}
	tests := []struct {
		name    string
		args    args
		want    string
		wantErr bool
	}{
		{
			name: "Data File in 2nd line",
			args: args{
				modelLines: []string{
					"random first line",
					"$DATA /tmp/data.csv",
					"random third line",
				},
			},
			want:    "/tmp/data.csv",
			wantErr: false,
		},
		{
			name: "Data file present without target",
			args: args{
				modelLines: []string{
					"random first line",
					"$DATA",
					"random third line",
				},
			},
			want:    "",
			wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := modelDataFile(tt.args.modelLines)
			if (err != nil) != tt.wantErr {
				t.Errorf("modelDataFile() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if got != tt.want {
				t.Errorf("modelDataFile() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_dataFileIsPresent(t *testing.T) {
	const testingDir string = "/tmp/testing/babylon"
	const testingFile string = "test.txt"
	//Temporary working directory
	fs := afero.NewOsFs()
	fs.RemoveAll(testingDir)
	fs.MkdirAll(testingDir, 0755)
	file, _ := fs.Create(filepath.Join(testingDir, testingFile))
	file.Sync()
	file.Close()

	type args struct {
		datafile  string
		modelpath string
	}
	tests := []struct {
		name    string
		args    args
		wantErr bool
	}{
		{
			name: "File can be located",
			args: args{
				datafile:  filepath.Join(testingDir, testingFile),
				modelpath: testingDir,
			},
			wantErr: false,
		}, {
			name: "File cannot be located",
			args: args{
				datafile:  "/meow/pants.txt",
				modelpath: "don'texist",
			},
			wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if err := dataFileIsPresent(tt.args.datafile, tt.args.modelpath); (err != nil) != tt.wantErr {
				t.Errorf("dataFileIsPresent() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}
