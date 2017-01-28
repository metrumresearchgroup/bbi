package server

import "github.com/dpastoor/nonmemutils/runner"

// Model information about a model to be executed
type Model struct {
	ID        int
	Status    string
	ModelInfo ModelInfo
	RunInfo   RunInfo
}

// RunInfo stores details about a model runner
// Queue time represents the time a model was added to the Queue
// StartTime is the time the worker starts execution of the model code for processing steps
// Duration is the time, in milliseconds from StartTime to The model run completing
// as a unix timestamp
type RunInfo struct {
	QueueTime int64
	StartTime int64
	Duration  int64
}

// ModelInfo contains the information passed in to execute the model
type ModelInfo struct {
	ModelPath   string
	RunSettings runner.RunSettings
	CacheDir    string
	CacheExe    string
}

// Client creates a connection to services
type Client interface {
	ModelService() ModelService
}

// ModelService describes the interface to interact with models
type ModelService interface {
	GetModels() ([]Model, error)
	GetModelsByStatus(status string) ([]Model, error)
	GetModelByID(modelID int) (Model, error)
	CreateModel(m *Model) error
	CreateModels(model []Model) error
	AcquireNextQueuedModel() (Model, error)
	UpdateModel(m *Model) error
}
