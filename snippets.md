These are snippets used during testing to be converted to cli commands or other internal functions

## copy model to be executed in next run directory

```go
AppFs := afero.NewOsFs()
filePath := "fixtures/run001.mod"

// create a new dir for model estimation
fileName, fileExt := utils.FileAndExt(filePath)
dir := filepath.Dir(filePath)
dirInfo, _ := afero.ReadDir(AppFs, dir)
dirs := utils.ListDirNames(dirInfo)
newDirSuggestion := runner.FindNextEstDirNum(fileName, dirs, 2)
AppFs.MkdirAll(filepath.Join(
    dir,
    newDirSuggestion.NextDirName,
), 0755)

// prepare and copy the model to be run001
fileLines, _ := utils.ReadLinesFS(AppFs, filePath)
utils.WriteLinesFS(
    AppFs,
    runner.PrepareForExecution(fileLines),
    filepath.Join(
        dir,
        newDirSuggestion.NextDirName,
        fmt.Sprintf("%s%s", fileName, fileExt),
    ),
)
```

## seeing which files in dir

```go
AppFs := afero.NewOsFs()
	dirToClean := "fixtures/run001_est_2/"
	dirInfo, _ := afero.ReadDir(AppFs, dirToClean)
	fileList := utils.ListFiles(dirInfo)
	for i, file := range fileList {
		outputFiles := runner.EstOutputFileCleanLevels()
		lvl, ok := outputFiles[file]
		fmt.Println(fmt.Sprintf("%v: %s --> lvl:  %v ok: %v", i, file, lvl, ok))
	}
```

## Clean folder and move relevant files to parent directory

```go
func main() {
	AppFs := afero.NewOsFs()
	runNum := "run001"
	dir := "fixtures"
	dirToClean := "fixtures/run001_est_02/"
	cleanLvl := 2
	copyLvl := 2
	dirInfo, _ := afero.ReadDir(AppFs, dirToClean)
	fileList := utils.ListFiles(dirInfo)
	outputFiles := runner.EstOutputFileCleanLevels()
	keyOutputFiles := runner.EstOutputFilesByRun(runNum)
	for i, file := range fileList {

		// handle cleaning

		lvl, ok := outputFiles[file]
		fmt.Println(fmt.Sprintf("%v: %s --> lvl:  %v ok: %v", i, file, lvl, ok))
		if ok && cleanLvl >= lvl {
			err := AppFs.Remove(filepath.Join(
				dirToClean,
				file,
			))
			if err != nil {
				fmt.Println("ERROR: ", err)
			}
			fmt.Println("deleted file: ", file)
			continue
		}

		// Copy files to directory above
		lvl, ok = keyOutputFiles[file]
		if ok && lvl >= copyLvl {
			fileToCopyLocation := filepath.Join(
				dirToClean,
				file,
			)
			fileToCopy, err := AppFs.Open(fileToCopyLocation)
			if err != nil {
				fmt.Println("TERRIBLE ERROR opening FILE TO COPY")
				os.Exit(1)
			}
			defer fileToCopy.Close()

			newFileLocation := filepath.Join(
				dir,
				file,
			)
			newFile, err := AppFs.Create(newFileLocation)
			if err != nil {
				fmt.Println("TERRIBLE ERROR CREATING FILE TO COPY")
				os.Exit(1)
				continue
			}
			defer newFile.Close()

			_, err = io.Copy(newFile, fileToCopy)
			if err != nil {
				fmt.Println("TERRIBLE ERROR TRYING TO COPY: ", err)
				os.Exit(1)
			}
		}
	}
}


```


## shell out

```
func main() {
	cmdName := "Rscript.exe"
	cmdArgs := []string{"stdout_stream.R"}

	cmd := exec.Command(cmdName, cmdArgs...)
	cmdReader, err := cmd.StdoutPipe()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error creating StdoutPipe for Cmd", err)
		os.Exit(1)
	}

	scanner := bufio.NewScanner(cmdReader)
	go func() {
		for scanner.Scan() {
			fmt.Printf("Rscript out | %s\n", scanner.Text())
		}
	}()

	err = cmd.Start()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error starting Cmd", err)
		os.Exit(1)
	}

	err = cmd.Wait()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error waiting for Cmd", err)
		os.Exit(1)
	}
}
```

## full run

```

func main() {
	AppFs := afero.NewOsFs()

	filePath := "testdata/run001.mod"

	// create a new dir for model estimation
	runNum, fileExt := utils.FileAndExt(filePath)
	dir := filepath.Dir(filePath)
	dirInfo, _ := afero.ReadDir(AppFs, dir)
	dirs := utils.ListDirNames(dirInfo)
	newDirSuggestion := runner.FindNextEstDirNum(runNum, dirs, 2)
	AppFs.MkdirAll(filepath.Join(
		dir,
		newDirSuggestion.NextDirName,
	), 0755)

	// prepare and copy the model to be run001
	fileLines, _ := utils.ReadLinesFS(AppFs, filePath)
	utils.WriteLinesFS(
		AppFs,
		runner.PrepareForExecution(fileLines),
		filepath.Join(
			dir,
			newDirSuggestion.NextDirName,
			fmt.Sprintf("%s%s", runNum, fileExt),
		),
	)

	// run model
	origDir, _ := os.Getwd()
	fmt.Println("starting at dir: ", origDir)
	err := os.Chdir(filepath.Join(dir, newDirSuggestion.NextDirName))
	if err != nil {
		fmt.Println("could not change directory to: ", newDirSuggestion.NextDirName)
		os.Exit(1)
	}
	cmdName := "nmfe74"
	cmdArgs := []string{
		strings.Join([]string{runNum, fileExt}, ""),
		strings.Join([]string{runNum, ".lst"}, ""),
	}

	cmd := exec.Command(cmdName, cmdArgs...)
	cmdReader, err := cmd.StdoutPipe()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error creating StdoutPipe for Cmd", err)
		os.Exit(1)
	}

	scanner := bufio.NewScanner(cmdReader)
	go func() {
		for scanner.Scan() {
			fmt.Printf("nmfe74 out | %s\n", scanner.Text())
		}
	}()

	err = cmd.Start()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error starting Cmd", err)
		os.Exit(1)
	}

	err = cmd.Wait()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error waiting for Cmd", err)
		os.Exit(1)
	}

	err = os.Chdir(origDir)
	if err != nil {
		fmt.Println("could not change directory back to: ", origDir)
		os.Exit(1)
	}
	backToDir, _ := os.Getwd()
	fmt.Println("changed dir back to: ", backToDir)

	dirToClean := newDirSuggestion.NextDirName
	cleanLvl := 2
	copyLvl := 2
	edirInfo, _ := afero.ReadDir(AppFs, filepath.Join(dir, dirToClean))
	fileList := utils.ListFiles(edirInfo)
	runner.CleanEstFolderAndCopyToParent(AppFs, dir, runNum, dirToClean, fileList, cleanLvl, copyLvl)
}
```