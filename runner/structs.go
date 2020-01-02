package runner

// RunSettings is a struct that contains settings about run information
// passed in from config variables or flags
type RunSettings struct {
	Git                bool   `json:"git,omitempty"`
	SaveExe            string `json:"save_exe,omitempty"`
	Verbose            bool   `json:"verbose,omitempty"`
	Debug              bool   `json:"debug,omitempty"`
	CleanLvl           int    `json:"clean_lvl,omitempty"`
	CopyLvl            int    `json:"copy_lvl,omitempty"`
	CacheDir           string `json:"cache_dir,omitempty"`
	ExeNameInCache     string `json:"exe_name_in_cache,omitempty"`
	NmExecutableOrPath string `json:"nm_executable_or_path,omitempty"`
	OneEst             bool   `json:"one_est,omitempty"`
	OutputDir          string `json:"output_dir,omitempty"`
	Overwrite          bool   `json:"overwrite,omitempty"`
}

// ReturnStatus gives information about the result of a model run
type ReturnStatus struct {
	RunDir string `json:"run_dir,omitempty"`
	DidRun bool   `json:"did_run,omitempty"`
	Error  error  `json:"error,omitempty"`
}

//NextDirSuggestion provides a struct for the recommended next
// directory name, and whether there should be a project reorganization
// based on directory modifications, and whether this will be the first dir in the sequence
type NextDirSuggestion struct {
	NextDirName string `json:"next_dir_name,omitempty"`
	Reorg       bool   `json:"reorg,omitempty"`
	FirstRun    bool   `json:"first_run,omitempty"`
}
