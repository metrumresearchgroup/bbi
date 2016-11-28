// a few of the below helpers originally copied from
// https://github.com/spf13/hugo/blob/master/helpers/path.go
// as of commit https://github.com/spf13/hugo/commit/65e5959bad36a6faa7ff8b9c1b21a2b99c4dacad

package utils

import (
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/spf13/afero"
)

// FilePathSeparator as defined by os.Separator.
const FilePathSeparator = string(filepath.Separator)

// ReplaceExtension takes a path and an extension, strips the old extension
// and returns the path with the new extension.
func ReplaceExtension(path string, newExt string) string {
	f, _ := FileAndExt(path)
	return f + "." + newExt
}

// Filename takes a path, strips out the extension,
// and returns the name of the file.
func Filename(in string) (name string) {
	name, _ = FileAndExt(in)
	return
}

// FileAndExt returns the filename and any extension of a file path as
// two separate strings.
//
// If the path, in, contains a directory name ending in a slash,
// then both name and ext will be empty strings.
//
// If the path, in, is either the current directory, the parent
// directory or the root directory, or an empty string,
// then both name and ext will be empty strings.
//
// If the path, in, represents the path of a file without an extension,
// then name will be the name of the file and ext will be an empty string.
//
// If the path, in, represents a filename with an extension,
// then name will be the filename minus any extension - including the dot
// and ext will contain the extension - minus the dot.
func FileAndExt(in string) (name string, ext string) {
	ext = filepath.Ext(in)
	base := filepath.Base(in)

	return extractFilename(in, ext, base, FilePathSeparator), ext
}

func extractFilename(in, ext, base, pathSeparator string) (name string) {

	// No file name cases. These are defined as:
	// 1. any "in" path that ends in a pathSeparator
	// 2. any "base" consisting of just an pathSeparator
	// 3. any "base" consisting of just an empty string
	// 4. any "base" consisting of just the current directory i.e. "."
	// 5. any "base" consisting of just the parent directory i.e. ".."
	if (strings.LastIndex(in, pathSeparator) == len(in)-1) || base == "" || base == "." || base == ".." || base == pathSeparator {
		name = "" // there is NO filename
	} else if ext != "" { // there was an Extension
		// return the filename minus the extension (and the ".")
		name = base[:strings.LastIndex(base, ".")]
	} else {
		// no extension case so just return base, which willi
		// be the filename
		name = base
	}
	return

}

// GetRelativePath returns the relative path of a given path.
func GetRelativePath(path, base string) (final string, err error) {
	if filepath.IsAbs(path) && base == "" {
		return "", errors.New("source: missing base directory")
	}
	name := filepath.Clean(path)
	base = filepath.Clean(base)

	name, err = filepath.Rel(base, name)
	if err != nil {
		return "", err
	}

	if strings.HasSuffix(filepath.FromSlash(path), FilePathSeparator) && !strings.HasSuffix(name, FilePathSeparator) {
		name += FilePathSeparator
	}
	return name, nil
}

// ExtractRootPaths extracts the root paths from the supplied list of paths.
// The resulting root path will not contain any file separators, but there
// may be duplicates.
// So "/content/section/" becomes "content"
func ExtractRootPaths(paths []string) []string {
	r := make([]string, len(paths))
	for i, p := range paths {
		root := filepath.ToSlash(p)
		sections := strings.Split(root, "/")
		for _, section := range sections {
			if section != "" {
				root = section
				break
			}
		}
		r[i] = root
	}
	return r

}

func getRealFileInfo(fs afero.Fs, path string) (os.FileInfo, string, error) {
	fileInfo, err := lstatIfOs(fs, path)
	realPath := path

	if err != nil {
		return nil, "", err
	}

	if fileInfo.Mode()&os.ModeSymlink == os.ModeSymlink {
		link, err := filepath.EvalSymlinks(path)
		if err != nil {
			return nil, "", fmt.Errorf("Cannot read symbolic link '%s', error was: %s", path, err)
		}
		fileInfo, err = lstatIfOs(fs, link)
		if err != nil {
			return nil, "", fmt.Errorf("Cannot stat '%s', error was: %s", link, err)
		}
		realPath = link
	}
	return fileInfo, realPath, nil
}

// GetRealPath returns the real file path for the given path, whether it is a
// symlink or not.
func GetRealPath(fs afero.Fs, path string) (string, error) {
	_, realPath, err := getRealFileInfo(fs, path)

	if err != nil {
		return "", err
	}

	return realPath, nil
}

// Code copied from Afero's path.go
// if the filesystem is OsFs use Lstat, else use fs.Stat
func lstatIfOs(fs afero.Fs, path string) (info os.FileInfo, err error) {
	_, ok := fs.(*afero.OsFs)
	if ok {
		info, err = os.Lstat(path)
	} else {
		info, err = fs.Stat(path)
	}
	return
}

// SafeWriteToDisk is the same as WriteToDisk
// but it also checks to see if file/directory already exists.
func SafeWriteToDisk(inpath string, r io.Reader, fs afero.Fs) (err error) {
	return afero.SafeWriteReader(fs, inpath, r)
}

// WriteToDisk writes content to disk.
func WriteToDisk(inpath string, r io.Reader, fs afero.Fs) (err error) {
	return afero.WriteReader(fs, inpath, r)
}

// GetTempDir returns a temporary directory with the given sub path.
func GetTempDir(subPath string, fs afero.Fs) string {
	return afero.GetTempDir(fs, subPath)
}

// DirExists checks if a path exists and is a directory.
func DirExists(path string, fs afero.Fs) (bool, error) {
	return afero.DirExists(fs, path)
}

// IsDir checks if a given path is a directory.
func IsDir(path string, fs afero.Fs) (bool, error) {
	return afero.IsDir(fs, path)
}

// IsEmpty checks if a given path is empty.
func IsEmpty(path string, fs afero.Fs) (bool, error) {
	return afero.IsEmpty(fs, path)
}

// FileContains checks if a file contains a specified string.
func FileContains(filename string, subslice []byte, fs afero.Fs) (bool, error) {
	return afero.FileContainsBytes(fs, filename, subslice)
}

// FileContainsAny checks if a file contains any of the specified strings.
func FileContainsAny(filename string, subslices [][]byte, fs afero.Fs) (bool, error) {
	return afero.FileContainsAnyBytes(fs, filename, subslices)
}

// Exists checks if a file or directory exists.
func Exists(path string, fs afero.Fs) (bool, error) {
	return afero.Exists(fs, path)
}

// ListDirNames returns an array of directory names from an array of fileinfo
// AppFs := afero.NewOsFs()
// dir := filepath.Dir(".")
// dirInfo, _ := afero.ReadDir(AppFs, dir)
// ListDirNames(dirInfo)
func ListDirNames(fd []os.FileInfo) []string {
	dirs := []string{}
	for _, pDir := range fd {
		if pDir.IsDir() && !strings.HasPrefix(pDir.Name(), ".") {
			//fmt.Printf("%v: %s\n", i, pDir.Name())
			dirs = append(dirs, pDir.Name())
		}
	}
	return dirs
}

// ListFileNames returns an array of directory names from an array of fileinfo
// AppFs := afero.NewOsFs()
// dir := filepath.Dir(".")
// dirInfo, _ := afero.ReadDir(AppFs, dir)
// ListDirNames(dirInfo)
func ListFiles(fd []os.FileInfo) []string {
	files := []string{}
	for _, pFile := range fd {
		if !pFile.IsDir() && !strings.HasPrefix(pFile.Name(), ".") {
			//fmt.Printf("%v: %s\n", i, pDir.Name())
			files = append(files, pFile.Name())
		}
	}
	return files
}
