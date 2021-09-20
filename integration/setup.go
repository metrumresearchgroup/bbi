package bbitest

import (
	"archive/tar"
	"compress/gzip"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/afero"
	"github.com/spf13/viper"
)

var ROOT_EXECUTION_DIR string
var EXECUTION_DIR string

// constants and variables used in summary tests.
const SUMMARY_TEST_DIR = "testdata/bbi_summary"
const SUMMARY_GOLD_DIR = "aa_golden_files"
const noSuchFileError = "no such file or directory"
const noFilePresentError = "no file present at"
const wrongExtensionError = "Must provide path to .lst"

type Scenario struct {
	Details    ScenarioDetails
	ctx        context.Context
	identifier string
	SourcePath string `json:"source_path"`
	Workpath   string `json:"workpath"`
	models     []Model
	archive    string // The name of the tar.gz file used
	DataFile   string `json:"data_file"` // The relative path to the data file in this scenario
	Datahash   string // Initially hashed value of the data file defined in scenario.json
}

type ScenarioDetails struct {
	DataFile string `json:"data_file"`
}

type Model struct {
	identifier string // acop or Executive_Mod
	filename   string // acop.mod or Executive_Mod.mod
	extension  string // .mod or .ctl
	path       string // Path at which model resides.
}

type Hashes struct {
	Model string `json:"model_md5"`
	Data  string `json:"data_md5"`
}

func (m Model) Execute(scenario *Scenario, args ...string) (string, error) {
	var cmdArguments []string

	cmdArguments = append(cmdArguments, args...)

	cmdArguments = append(cmdArguments, []string{
		filepath.Join(scenario.Workpath, m.filename),
	}...)

	return executeCommand(scenario.ctx, "bbi", cmdArguments...)
}

func newScenario(path string) (Scenario, error) {
	scenario := Scenario{
		identifier: filepath.Base(path),
		models:     []Model{},
	}

	scenario.SourcePath = path
	scenario.Workpath = filepath.Join(EXECUTION_DIR, scenario.identifier)

	scenario.models = modelsFromOriginalScenarioPath(path)
	scenario.archive = scenario.identifier + ".tar.gz"

	if len(scenario.models) == 0 {
		return scenario, errors.New("no model directories were located in the provided scenario")
	}

	return scenario, nil
}

func modelsFromOriginalScenarioPath(path string) []Model {
	models := []Model{}

	scenarioID := filepath.Base(path)
	newBaseDir := filepath.Join(EXECUTION_DIR, scenarioID)

	modelIdentifiers := []string{
		".ctl",
		".mod",
	}

	fs := afero.NewOsFs()

	for _, v := range modelIdentifiers {
		contents, _ := afero.Glob(fs, filepath.Join(path, "*"+v))
		for _, c := range contents {
			model := Model{
				filename: filepath.Base(c),
			}

			e := filepath.Ext(model.filename)
			model.identifier = strings.TrimSuffix(model.filename, e)
			model.extension = strings.TrimPrefix(e, ".")

			modelDir := filepath.Join(newBaseDir, model.identifier)
			model.path = modelDir

			models = append(models, model)
		}
	}

	return models
}

/*
func Initialize() []*Scenario {
	viper.SetEnvPrefix("bbi")
	viper.AutomaticEnv()

	if len(os.Getenv("NONMEMROOT")) == 0 {
		log.Fatal("Please provide the NONMEMROOT environment variable so that the bbi init command knows where" +
			"to look for Nonmem installations")
	}

	log.Infof("Beginning work with %s as the root", EXECUTION_DIR)

	fs := afero.NewOsFs()
	if ok, _ := afero.DirExists(fs, EXECUTION_DIR); !ok {
		fs.MkdirAll(EXECUTION_DIR, 0755)
	} else {
		fs.RemoveAll(EXECUTION_DIR)
		fs.MkdirAll(EXECUTION_DIR, 0755)
	}

	var scenarios []*Scenario

	dirs, _ := getScenarioDirs()
	whereami, _ := os.Getwd()

	// Let's navigate to each and try to tar it up
	// We'll use these later for execution layers by always starting with a clean slate from the tar content
	for _, v := range dirs {
		n := v
		scenario, _ := newScenario(n)
		scenarios = append(scenarios, &scenario)
		f, _ := os.Create(filepath.Join(whereami, "testdata", filepath.Base(n)+".tar.gz"))
		err := Tar(filepath.Join(n), f)
		if err != nil {
			log.Error(err)
		}
		f.Close()
	}

	// Now let's find all the tar gz files and move them to the EXECUTIONDIR
	tars, _ := afero.Glob(afero.NewOsFs(), filepath.Join(whereami, "testdata", "*.tar.gz"))

	for _, v := range tars {
		source, _ := os.Open(v)
		defer source.Close()

		dest, _ := os.Create(filepath.Join(EXECUTION_DIR, filepath.Base(v)))
		defer dest.Close()

		io.Copy(dest, source)
	}

	return scenarios
}
*/

// InitializeScenarios is used to set everything up for specific scenarios by name. These names will correlate to the directory
// names in the TestData directory. IE 240/acop/ctl_test/metrum_std.
func InitializeScenarios(selected []string) ([]*Scenario, error) {
	viper.SetEnvPrefix("bbi")
	viper.AutomaticEnv()

	if len(os.Getenv("NONMEMROOT")) == 0 {
		log.Fatal("Please provide the NONMEMROOT environment variable so that the bbi init command knows where" +
			"to look for Nonmem installations")
	}

	log.Infof("Beginning work with %s as the root", EXECUTION_DIR)

	fs := afero.NewOsFs()
	if ok, _ := afero.DirExists(fs, EXECUTION_DIR); !ok {
		if err := fs.MkdirAll(EXECUTION_DIR, 0755); err != nil {
			return nil, err
		}
	} else {
		if err := fs.RemoveAll(EXECUTION_DIR); err != nil {
			return nil, err
		}
		if err := fs.MkdirAll(EXECUTION_DIR, 0755); err != nil {
			return nil, err
		}
	}

	var scenarios []*Scenario

	dirs, _ := getScenarioDirs()
	whereami, _ := os.Getwd()

	// Let's navigate to each and try to tar it up
	// We'll use these later for execution layers by always starting with a clean slate from the tar content
	for _, v := range dirs {
		for _, s := range selected {
			if strings.EqualFold(s, filepath.Base(v)) {
				n := v
				scenario, _ := newScenario(n)
				scenarios = append(scenarios, &scenario)
				f, _ := os.Create(filepath.Join(whereami, "testdata", filepath.Base(n)+".tar.gz"))
				err := Tar(filepath.Join(n), f)
				if err != nil {
					log.Error(err)
				}
				f.Close()
			}
		}
	}

	// Now let's find all the tar gz files and move them to the EXECUTIONDIR
	tars, _ := afero.Glob(afero.NewOsFs(), filepath.Join(whereami, "testdata", "*.tar.gz"))

	for _, v := range tars {
		source, _ := os.Open(v)
		defer source.Close()

		dest, _ := os.Create(filepath.Join(EXECUTION_DIR, filepath.Base(v)))
		defer dest.Close()

		_, err := io.Copy(dest, source)
		if err != nil {
			return nil, err
		}
	}

	return scenarios, nil
}

func getScenarioDirs() ([]string, error) {
	whereami, _ := os.Getwd()
	fs := afero.NewOsFs()
	directories := []string{}

	contents, err := afero.ReadDir(fs, filepath.Join(whereami, "testdata"))

	if err != nil {
		log.Error("Unable to parse directory contents of 'testdata'")

		return directories, err
	}

	for _, v := range contents {
		if ok, _ := afero.IsDir(fs, filepath.Join(whereami, "testdata", v.Name())); ok {
			directories = append(directories, filepath.Join(whereami, "testdata", v.Name()))
		}
	}

	return directories, nil
}

// Tar takes a source and variable writers and walks 'source' writing each file
// found to the tar writer; the purpose for accepting multiple writers is to allow
// for multiple outputs (for example a file, or md5 hash).
func Tar(src string, writers ...io.Writer) error {
	// ensure the src actually exists before trying to tar it
	if _, err := os.Stat(src); err != nil {
		return fmt.Errorf("unable to tar files: %w", err)
	}

	mw := io.MultiWriter(writers...)

	gzw := gzip.NewWriter(mw)
	defer gzw.Close()

	tw := tar.NewWriter(gzw)
	defer tw.Close()

	// walk path
	return filepath.Walk(src, func(file string, fi os.FileInfo, err error) error {
		// return on any error
		if err != nil {
			return err
		}

		// return on non-regular files (thanks to [kumo](https://medium.com/@komuw/just-like-you-did-fbdd7df829d3) for this suggested update)
		if !fi.Mode().IsRegular() {
			return nil
		}

		// create a new dir/file header
		header, err := tar.FileInfoHeader(fi, fi.Name())
		if err != nil {
			return err
		}

		// update the name to correctly reflect the desired destination when untaring
		header.Name = strings.TrimPrefix(strings.Replace(file, src, "", -1), string(filepath.Separator))

		// write the header
		if err = tw.WriteHeader(header); err != nil {
			return err
		}

		// open files for taring
		f, err := os.Open(file)
		if err != nil {
			return err
		}

		// copy file data into tar writer
		if _, err = io.Copy(tw, f); err != nil {
			return err
		}

		// manually close here after each file operation; defering would cause each file close
		// to wait until all operations have completed.
		if err = f.Close(); err != nil {
			return err
		}

		return nil
	})
}

// Untar takes a destination path and a reader; a tar reader loops over the tarfile
// creating the file structure at 'dst' along the way, and writing any files.
func Untar(dst string, r io.Reader) error {
	gzr, err := gzip.NewReader(r)
	if err != nil {
		return err
	}
	defer gzr.Close()

	tr := tar.NewReader(gzr)

	for {
		var header *tar.Header
		header, err = tr.Next()

		switch {
		// if no more files are found return
		case errors.Is(err, io.EOF):
			return nil

		// return any other error
		case err != nil:
			return err

		// if the header is nil, just skip it (not sure how this happens)
		case header == nil:
			continue
		}

		// the target location where the dir/file should be created
		target := filepath.Join(dst, header.Name)

		// the following switch could also be done using fi.Mode(), not sure if there
		// a benefit of using one vs. the other.
		// fi := header.FileInfo()

		// check the file type
		switch header.Typeflag {
		// if its a dir and it doesn't exist create it
		case tar.TypeDir:
			if _, err = os.Stat(target); err != nil {
				if err = os.MkdirAll(target, 0755); err != nil {
					return err
				}
			}

		// if it's a file create it
		case tar.TypeReg:
			fs := afero.NewOsFs()
			parent := filepath.Dir(target)
			if ok, _ := afero.DirExists(fs, parent); !ok {
				if err = fs.MkdirAll(parent, 0755); err != nil {
					return err
				}
			}
			var f *os.File
			if f, err = os.OpenFile(target, os.O_CREATE|os.O_RDWR, os.FileMode(header.Mode)); err != nil {
				return err
			}

			// copy over contents
			if _, err = io.Copy(f, tr); err != nil {
				return err
			}

			// manually close here after each file operation; defering would cause each file close
			// to wait until all operations have completed.
			if err = f.Close(); err != nil {
				return err
			}
		}
	}
}

/*
func findModelFiles(path string) []string {
	knownModelTypes := []string{
		".mod",
		".ctl",
	}

	foundModels := []string{}

	fs := afero.NewOsFs()

	for _, v := range knownModelTypes {
		located, _ := afero.Glob(fs, filepath.Join(path, "*"+v))
		for _, l := range located {
			foundModels = append(foundModels, l)
		}
	}

	return foundModels
}
*/

func (scenario *Scenario) Prepare(ctx context.Context) error {
	if _, err := executeCommand(ctx, "bbi", "init", "--dir", os.Getenv("NONMEMROOT")); err != nil {
		return err
	}

	fs := afero.NewOsFs()
	scenario.ctx = ctx

	// create Target directory as this untar operation doesn't handle it for you
	if err := fs.MkdirAll(scenario.Workpath, 0755); err != nil {
		return err
	}

	reader, err := os.Open(filepath.Join(EXECUTION_DIR, scenario.archive))
	if err != nil {
		return err
	}

	if err = Untar(scenario.Workpath, reader); err != nil {
		return err
	}

	if err = reader.Close(); err != nil {
		return err
	}

	whereami, err := os.Getwd()
	if err != nil {
		return err
	}

	if err = os.Chdir(scenario.Workpath); err != nil {
		return err
	}

	if _, err = executeCommand(ctx, "bbi", "init", "--dir", os.Getenv("NONMEMROOT")); err != nil {
		return err
	}

	if err = os.Chdir(whereami); err != nil {
		return err
	}

	return err
}

func FeatureEnabled(key string) bool {
	value := os.Getenv(key)

	if value == "" {
		return false
	}

	b, err := strconv.ParseBool(value)

	if err != nil {
		return false
	}

	return b
}

func GetScenarioDetailsFromFile(r io.Reader) (ScenarioDetails, error) {
	var s ScenarioDetails
	contents, _ := ioutil.ReadAll(r)
	err := json.Unmarshal(contents, &s)

	return s, err
}

func GetBBIConfigJSONHashedValues(r io.Reader) (Hashes, error) {
	var h Hashes
	contents, _ := ioutil.ReadAll(r)
	err := json.Unmarshal(contents, &h)

	return h, err
}

func init() {
	if os.Getenv("ROOT_EXECUTION_DIR") == "" {
		log.Error("Please set the ROOT_EXECUTION_DIR environment variable")
		os.Exit(1)
	}

	ROOT_EXECUTION_DIR = os.Getenv("ROOT_EXECUTION_DIR")
	EXECUTION_DIR = filepath.Join(ROOT_EXECUTION_DIR, "working")
}
