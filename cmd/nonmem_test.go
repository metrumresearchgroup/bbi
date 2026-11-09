package cmd

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/metrumresearchgroup/bbi/utils"

	"github.com/metrumresearchgroup/bbi/configlib"

	"github.com/metrumresearchgroup/wrapt"
	"github.com/spf13/afero"
)

func Test_processNMFEOptions(tt *testing.T) {
	type args struct {
		config configlib.Config
	}
	tests := []struct {
		name string
		args args
		want []string
	}{
		{
			name: "Normal Operation",
			args: args{
				config: configlib.Config{
					NMVersion:  "",
					Overwrite:  false,
					CleanLvl:   0,
					CopyLvl:    0,
					Git:        false,
					BbiBinary:  "",
					SaveConfig: false,
					OutputDir:  "",
					Threads:    0,
					Debug:      false,
					Local:      configlib.LocalDetail{},
					Nonmem:     nil,
					Delay:      0,
					NMQual:     false,
					JSON:       false,
					Logfile:    "",
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
				config: configlib.Config{
					NMVersion:  "",
					Overwrite:  false,
					CleanLvl:   0,
					CopyLvl:    0,
					Git:        false,
					BbiBinary:  "",
					SaveConfig: false,
					OutputDir:  "",
					Threads:    0,
					Debug:      false,
					Local:      configlib.LocalDetail{},
					Nonmem:     nil,
					Delay:      0,
					NMQual:     false,
					JSON:       false,
					Logfile:    "",
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
		{
			name: "invalid maxlim",
			args: args{
				config: configlib.Config{
					NMFEOptions: configlib.NMFEOptions{
						MaxLim: -1,
					},
				},
			},
			want: nil,
		},
		{
			name: "maxlim=2",
			args: args{
				config: configlib.Config{
					NMFEOptions: configlib.NMFEOptions{
						MaxLim: 2,
					},
				},
			},
			want: []string{"-maxlim=2"},
		},
		{
			name: "maxlim=0",
			args: args{
				config: configlib.Config{
					NMFEOptions: configlib.NMFEOptions{
						MaxLim: 0,
					},
				},
			},
			want: nil,
		},
	}
	testId := "UNIT-CMD-002"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			got := processNMFEOptions(test.args.config)

			t.R.Equal(test.want, got)
		})
	}
}

func Test_modelDataFile(tt *testing.T) {
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
	testId := "UNIT-CMD-003"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			got, err := modelDataFile(test.args.modelLines)

			t.R.WantError(test.wantErr, err)
			t.R.Equal(test.want, got)
		})
	}
}

func Test_dataFileIsPresent(tt *testing.T) {
	const testingDir string = "/tmp/testing/bbi"
	const testingFile string = "test.txt"

	t := wrapt.WrapT(tt)
	// Temporary working directory
	fs := afero.NewOsFs()
	t.R.NoError(fs.RemoveAll(testingDir))
	t.R.NoError(fs.MkdirAll(testingDir, 0755))
	file, _ := fs.Create(filepath.Join(testingDir, testingFile))
	t.R.NoError(file.Sync())
	t.R.NoError(file.Close())

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
	testId := "UNIT-CMD-004"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			err := dataFileIsPresent(test.args.datafile, test.args.modelpath)

			t.R.WantError(test.wantErr, err)
		})
	}
}

func TestCreateChildDirectories(tt *testing.T) {
	t := wrapt.WrapT(tt)
	dir := t.TempDir()

	mname := "200"
	mpath := filepath.Join(dir, mname+".ctl")
	outdir := filepath.Join(dir, mname)

	t.Chdir(dir)

	err := os.WriteFile(mpath, []byte{}, 0o666)
	t.R.NoError(err)

	mod := &NonMemModel{
		Path:      mpath,
		Model:     filepath.Base(mpath),
		FileName:  mname,
		OutputDir: outdir,
		Configuration: configlib.Config{
			Overwrite: false,
		},
	}

	err = createChildDirectories(mod, false)
	t.R.NoError(err)
	t.A.DirExists(outdir)

	err = createChildDirectories(mod, false)
	t.R.ErrorContains(err, "overwrite")

	mod.Configuration.Overwrite = true

	err = createChildDirectories(mod, false)
	t.R.NoError(err)
	t.A.DirExists(outdir)
}
