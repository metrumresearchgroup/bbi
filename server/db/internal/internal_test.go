package internal

import (
	"testing"
	"time"

	"reflect"

	"github.com/metrumresearchgroup/bbi/runner"
	"github.com/metrumresearchgroup/bbi/server"
)

func TestMarshalModel(t *testing.T) {
	sampleDuration := float64(time.Now().AddDate(0, 0, -1).Add(10*time.Minute).Unix() - time.Now().AddDate(0, 0, -1).Unix())
	testModel := server.Model{
		ID:     0,
		Status: "COMPLETED",
		ModelInfo: server.ModelInfo{
			ModelPath: "C://temphello",
			RunSettings: runner.RunSettings{
				Git:            true,
				SaveExe:        "cache.exe",
				Verbose:        true,
				Debug:          true,
				CleanLvl:       1,
				CopyLvl:        2,
				CacheDir:       "cache_dir",
				ExeNameInCache: "cache.exe",
				OneEst:         true,
			},
		},
		RunInfo: server.RunInfo{
			QueueTime: time.Now().AddDate(0, 0, -1).Unix(),
			StartTime: time.Now().AddDate(0, 0, -1).Unix(),
			Duration:  sampleDuration,
			RunDir:    "run_est_001",
			Error:     "an error",
		},
	}

	var result server.Model
	if buf, err := MarshalModel(&testModel); err != nil {
		t.Fatal(err)
	} else if err := UnmarshalModel(buf, &result); err != nil {
		t.Fatal(err)
	} else if !reflect.DeepEqual(testModel, result) {
		t.Fatalf("unexpected copy: %#v", result)
	}
}
