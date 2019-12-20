package utils

import (
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"runtime"

	"github.com/spf13/afero"
)

// SetupCacheForRun copies over the cached nonmem executable from the cache to the run directory
func SetupCacheForRun(
	fs afero.Fs,
	baseDir string,
	modelDir string,
	cacheDir string,
	nmNameInCache string,
	debug bool,
) error {
	var fullCacheDirPath string
	var fullModelDirPath string
	if filepath.IsAbs(cacheDir) {
		fullCacheDirPath = cacheDir
	} else {
		fullCacheDirPath = filepath.Join(baseDir, cacheDir)
	}

	if filepath.IsAbs(modelDir) {
		fullModelDirPath = modelDir
	} else {
		fullModelDirPath = filepath.Join(baseDir, modelDir)
	}
	if debug {
		log.Printf("cache directory: %s", fullCacheDirPath)
		log.Printf("model directory: %s", fullModelDirPath)
	}
	// this should always be there how the package is being used currently as the time
	// this function is called is after the modelDir is successfully created, but given
	// there may be other uses, to be safe will also check again.
	ok, err := DirExists(fullCacheDirPath, fs)
	if !ok || err != nil {
		log.Printf("issue with cache directory at: %s, no precompiled model will be used. ERR: %s, ok: %v", cacheDir, err, ok)
		return err
	}
	//check that modelDir exists to copy nonmem executable into
	ok, err = DirExists(fullModelDirPath, fs)
	if !ok || err != nil {
		log.Printf("issue with model directory at: %s, no precompiled model will be used. ERR: %s, ok: %v", cacheDir, err, ok)
		return err
	}
	// check nmNameInCache is in in cache
	// copy file from cache to modelDir

	fileToCopyLocation := filepath.Join(
		fullCacheDirPath,
		nmNameInCache,
	)

	ok, err = Exists(fileToCopyLocation, fs)
	if !ok || err != nil {
		log.Printf("no cached model detected at: %s, skipping attempt to precompile", fileToCopyLocation)
		return err
	}

	fileToCopy, err := fs.Open(fileToCopyLocation)
	if err != nil {
		return fmt.Errorf("error copying file: (%s)", err)
	}
	defer fileToCopy.Close()

	var nonmemExecutableName string
	if runtime.GOOS == "windows" {
		nonmemExecutableName = "nonmem.exe"
	} else {
		nonmemExecutableName = "nonmem"
	}
	newFileLocation := filepath.Join(
		fullModelDirPath,
		nonmemExecutableName,
	)
	newFile, err := fs.Create(newFileLocation)
	if err != nil {
		return fmt.Errorf("error creating new file: (%s)", err)
	}
	defer newFile.Close()

	_, err = io.Copy(newFile, fileToCopy)
	if err != nil {
		return fmt.Errorf("error copying to new file: (%s)", err)
	}
	if debug {
		log.Println("changing executable privileges for nonmem executable")
	}
	if err := os.Chmod(newFileLocation, 0777); err != nil {
		log.Println("error changing permissions of executable after copying from cache")
		return (err)
	}
	return nil
}
