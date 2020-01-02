package utils

import (
	"fmt"
	"io"
	"log"
	"path/filepath"
	"runtime"

	"github.com/spf13/afero"
)

// CopyExeToCache copies over the nonmem executable to the cache from the run directory
func CopyExeToCache(
	fs afero.Fs,
	modelDir string,
	cacheDir string,
	nmNameInCache string,
) error {

	fullCacheDirPath := filepath.Clean(cacheDir)
	fullModelDirPath := filepath.Clean(modelDir)

	ok, err := DirExists(fullCacheDirPath, fs)
	if !ok || err != nil {
		log.Printf("issue with cache directory at: %s, will not save executable to cache. ERR: %s, ok: %v", cacheDir, err, ok)
		return err
	}
	//check that modelDir exists to copy nonmem executable into
	ok, err = DirExists(fullModelDirPath, fs)
	if !ok || err != nil {
		log.Printf("issue with model directory at: %s, will not save executable to cache. ERR: %s, ok: %v", cacheDir, err, ok)
		return err
	}
	// check nmNameInCache is in in cache
	// copy file from cache to modelDir

	newCacheFileLocation := filepath.Join(
		fullCacheDirPath,
		nmNameInCache,
	)
	cacheFile, err := fs.Create(newCacheFileLocation)
	if err != nil {
		return fmt.Errorf("error copying file: (%s)", err)
	}
	defer cacheFile.Close()

	var nonmemExecutableName string
	if runtime.GOOS == "windows" {
		nonmemExecutableName = "nonmem.exe"
	} else {
		nonmemExecutableName = "nonmem"
	}
	exeLocation := filepath.Join(
		fullModelDirPath,
		nonmemExecutableName,
	)
	exeFile, err := fs.Open(exeLocation)
	if err != nil {
		return fmt.Errorf("error with nonmem exe file: (%s)", err)
	}
	defer exeFile.Close()

	_, err = io.Copy(cacheFile, exeFile)
	if err != nil {
		return fmt.Errorf("error copying to new file: (%s)", err)
	}
	return nil
}
