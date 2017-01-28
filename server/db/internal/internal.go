package internal

import (
	"fmt"

	"github.com/dpastoor/nonmemutils/runner"
	"github.com/dpastoor/nonmemutils/server"
	"github.com/gogo/protobuf/proto"
)

//go:generate protoc --gofast_out=. internal.proto

// MarshalModel encodes a model to binary format.
func MarshalModel(m *server.Model) ([]byte, error) {
	var status Model_StatusType
	modelInfo := &m.ModelInfo
	runInfo := &m.RunInfo
	switch m.Status {
	case "QUEUED":
		status = Model_QUEUED
	case "RUNNING":
		status = Model_RUNNING
	case "COMPLETED":
		status = Model_COMPLETED
	default:
		return nil, fmt.Errorf("unrecognized model status: %v", m.Status)
	}
	return proto.Marshal(&Model{
		Id:     int64(m.ID),
		Status: status,
		ModelInfo: &ModelInfo{
			ModelPath: modelInfo.ModelPath,
			RunSettings: &ModelInfo_RunSettings{
				Git:     modelInfo.RunSettings.Git,
				SaveExe: modelInfo.RunSettings.SaveExe,
			},
			CacheDir: modelInfo.CacheDir,
			CacheExe: modelInfo.CacheExe,
		},
		RunInfo: &RunInfo{
			QueueTime: runInfo.QueueTime,
			StartTime: runInfo.StartTime,
			Duration:  runInfo.Duration,
		},
	})
}

// UnmarshalModel decodes a model from a binary data.
func UnmarshalModel(data []byte, m *server.Model) error {
	var pb Model
	if err := proto.Unmarshal(data, &pb); err != nil {
		return err
	}

	modelInfo := pb.GetModelInfo()
	runInfo := pb.GetRunInfo()

	m.ID = int(pb.Id)
	status := pb.GetStatus()

	switch status {
	case Model_COMPLETED:
		m.Status = "COMPLETED"
	case Model_RUNNING:
		m.Status = "RUNNING"
	case Model_QUEUED:
		m.Status = "QUEUED"
	default:
		return fmt.Errorf("unrecognized model status: %v", status)
	}
	m.ModelInfo = server.ModelInfo{
		ModelPath: modelInfo.ModelPath,
		RunSettings: runner.RunSettings{
			Git:     modelInfo.RunSettings.Git,
			SaveExe: modelInfo.RunSettings.SaveExe,
		},
		CacheDir: modelInfo.CacheDir,
		CacheExe: modelInfo.CacheExe,
	}
	m.RunInfo = server.RunInfo{
		QueueTime: runInfo.QueueTime,
		StartTime: runInfo.StartTime,
		Duration:  runInfo.Duration,
	}

	return nil
}
