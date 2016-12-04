package utils

import (
	"fmt"
	"io"
	"log"
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
) error {

	fullCacheDirPath := filepath.Join(baseDir, cacheDir)
	fullModelDirPath := filepath.Join(baseDir, modelDir)

	ok, err := DirExists(fullCacheDirPath, fs)
	if !ok || err != nil {
		//TODO: change these exits to instead just return an error probably
		log.Printf("issue with cache directory at: %s, no precompiled model will be used. ERR: %s, ok: %v", cacheDir, err, ok)
		return err
	}
	//check that modelDir exists to copy nonmem executable into
	ok, err = DirExists(fullModelDirPath, fs)
	if !ok || err != nil {
		//TODO: change these exits to instead just return an error probably
		log.Printf("issue with model directory at: %s, no precompiled model will be used. ERR: %s, ok: %v", cacheDir, err, ok)
		return err
	}
	// check nmNameInCache is in in cache
	// copy file from cache to modelDir

	fileToCopyLocation := filepath.Join(
		fullCacheDirPath,
		nmNameInCache,
	)
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
	return nil
}
