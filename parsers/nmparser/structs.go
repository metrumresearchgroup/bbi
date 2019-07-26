package parser

// RunDetails contains key information about logistics of the model run
type RunDetails struct {
	NMversion           string
	RunStart            string
	RunEnd              string
	EstimationTime      float64
	CovarianceTime      float64
	FunctionEvaluations int64
	SignificantDigits   float64
	ProblemText         string
	ModFile             string
	EstimationMethod    []string
	DataSet             string
	NumberOfPatients    int32
	NumberOfObs         int32
	NumberOfDataRecords int32
	OutputTable         string
}

// CompletionDetails ...
type CompletionDetails struct {
	Shrinkage                 ShrinkageDetails
	Ofv                       OfvDetails
	ZeroGradientDetected      bool
	FinalZeroGradientDetected bool
	CovStepComplete           bool
	Messages                  []string
}

// ShrinkageDetails ...
type ShrinkageDetails struct {
	Eta Shrinkage
	Ebv Shrinkage
	Eps Shrinkage
}

// GradientDetails ...
type GradientDetails struct {
	Zero  bool
	Start string
	End   float64
}

// CovarianceStep ...
type CovarianceStep struct {
	Attempted  bool
	MatrixType string
	OK         bool
	Errors     []string
}

// Shrinkage ...
type Shrinkage struct {
	SD []float64
	VR []float64
}

// OfvDetails ...
type OfvDetails struct {
	OFV             float64
	OFVNoConstant   float64
	OFVWithConstant float64
}
