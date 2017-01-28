package internal

import (
	"fmt"
	"testing"
	"time"

	"reflect"

	"github.com/dpastoor/nonmemutils/runner"
	"github.com/dpastoor/nonmemutils/server"
)

func TestMarshalModel(t *testing.T) {
	sampleDuration := time.Now().AddDate(0, 0, -1).Add(10*time.Minute).Unix() - time.Now().AddDate(0, 0, -1).Unix()
	testModel := server.Model{
		ID:     0,
		Status: "COMPLETED",
		ModelInfo: server.ModelInfo{
			ModelPath:   "C://temphello",
			RunSettings: runner.RunSettings{Git: true, SaveExe: "cache.exe"},
			CacheDir:    "cache_dir",
			CacheExe:    "cache.exe",
		},
		RunInfo: server.RunInfo{
			QueueTime: time.Now().AddDate(0, 0, -1).Unix(),
			StartTime: time.Now().AddDate(0, 0, -1).Unix(),
			Duration:  sampleDuration,
		},
	}
	fmt.Println(testModel)

	var result server.Model
	if buf, err := MarshalModel(&testModel); err != nil {
		t.Fatal(err)
	} else if err := UnmarshalModel(buf, &result); err != nil {
		t.Fatal(err)
	} else if !reflect.DeepEqual(testModel, result) {
		t.Fatalf("unexpected copy: %#v", result)
	}
}
