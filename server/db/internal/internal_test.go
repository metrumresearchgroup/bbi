package internal

import (
	"testing"
	"time"

	"reflect"

	"github.com/dpastoor/babylon/runner"
	"github.com/dpastoor/babylon/server"
)

func TestMarshalModel(t *testing.T) {
	sampleDuration := time.Now().AddDate(0, 0, -1).Add(10*time.Minute).Unix() - time.Now().AddDate(0, 0, -1).Unix()
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
			},
		},
		RunInfo: server.RunInfo{
			QueueTime: time.Now().AddDate(0, 0, -1).Unix(),
			StartTime: time.Now().AddDate(0, 0, -1).Unix(),
			Duration:  sampleDuration,
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
