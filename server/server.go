package server

import "github.com/metrumresearchgroup/bbi/runner"

// Model information about a model to be executed
type Model struct {
	ID        int       `json:"id,omitempty"`
	Status    string    `json:"status,omitempty"`
	ModelInfo ModelInfo `json:"model_info,omitempty"`
	RunInfo   RunInfo   `json:"run_info,omitempty"`
}

// RunInfo stores details about a model runner
// Queue time represents the time a model was added to the Queue
// StartTime is the time the worker starts execution of the model code for processing steps
// Duration is the time, in milliseconds from StartTime to The model run completing
// RunDir is the (sub)-directory where the model is executed
// Error is the string representation of the error that stopped the run if an error was present
// as a unix timestamp
type RunInfo struct {
	QueueTime int64   `json:"queue_time,omitempty"`
	StartTime int64   `json:"start_time,omitempty"`
	Duration  float64 `json:"duration,omitempty"`
	RunDir    string  `json:"run_dir,omitempty"`
	Error     string  `json:"error,omitempty"`
}

// ModelInfo contains the information about the model execution
type ModelInfo struct {
	ModelPath   string             `json:"model_path,omitempty"`
	RunSettings runner.RunSettings `json:"run_settings,omitempty"`
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
