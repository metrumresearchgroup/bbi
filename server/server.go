package server

import "github.com/dpastoor/babylon/runner"

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
// RunDir is the (sub)-directory where the model is executed
// Error is the string representation of the error that stopped the run if an error was present
// as a unix timestamp
type RunInfo struct {
	QueueTime int64
	StartTime int64
	Duration  int64
	RunDir    string
	Error     string
}

// ModelInfo contains the information about the model execution
type ModelInfo struct {
	ModelPath   string
	RunSettings runner.RunSettings
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
	CreateModels(model []Model) ([]Model, error)
	AcquireNextQueuedModel() (Model, error)
	UpdateModel(m *Model) error
}
