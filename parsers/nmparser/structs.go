package parser

// RunDetails contains key information about logistics of the model run
type RunDetails struct {
	Version             string   `json:"version,omitempty"`
	RunStart            string   `json:"run_start,omitempty"`
	RunEnd              string   `json:"run_end,omitempty"`
	EstimationTime      float64  `json:"estimation_time,omitempty"`
	CovarianceTime      float64  `json:"covariance_time,omitempty"`
	FunctionEvaluations int64    `json:"function_evaluations,omitempty"`
	SignificantDigits   float64  `json:"significant_digits,omitempty"`
	ProblemText         string   `json:"problem_text,omitempty"`
	ModFile             string   `json:"mod_file,omitempty"`
	EstimationMethod    []string `json:"estimation_method,omitempty"`
	DataSet             string   `json:"data_set,omitempty"`
	NumberOfPatients    int64    `json:"number_of_patients,omitempty"`
	NumberOfObs         int64    `json:"number_of_obs,omitempty"`
	NumberOfDataRecords int64    `json:"number_of_data_records,omitempty"`
	OutputTable         string   `json:"output_table,omitempty"`
}

// CompletionDetails ...
type CompletionDetails struct {
	Shrinkage                 ShrinkageDetails `json:"shrinkage,omitempty"`
	Ofv                       OfvDetails       `json:"ofv,omitempty"`
	ZeroGradientDetected      bool             `json:"zero_gradient_detected,omitempty"`
	FinalZeroGradientDetected bool             `json:"final_zero_gradient_detected,omitempty"`
	CovStepComplete           bool             `json:"cov_step_complete,omitempty"`
	Messages                  []string         `json:"messages,omitempty"`
}

// ShrinkageDetails ...
type ShrinkageDetails struct {
	Eta Shrinkage `json:"eta,omitempty"`
	Ebv Shrinkage `json:"ebv,omitempty"`
	Eps Shrinkage `json:"eps,omitempty"`
}

// GradientDetails ...
type GradientDetails struct {
	Zero  bool    `json:"zero,omitempty"`
	Start string  `json:"start,omitempty"`
	End   float64 `json:"end,omitempty"`
}

// CovarianceStep ...
type CovarianceStep struct {
	Attempted  bool     `json:"attempted,omitempty"`
	MatrixType string   `json:"matrix_type,omitempty"`
	OK         bool     `json:"ok,omitempty"`
	Errors     []string `json:"errors,omitempty"`
}

// Shrinkage ...
type Shrinkage struct {
	SD []float64 `json:"sd,omitempty"`
	VR []float64 `json:"vr,omitempty"`
}

// OfvDetails ...
type OfvDetails struct {
	OFV             float64 `json:"ofv,omitempty"`
	OFVNoConstant   float64 `json:"ofv_no_constant,omitempty"`
	OFVWithConstant float64 `json:"ofv_with_constant,omitempty"`
}

// ModelOutput is the output struct from a lst file
type ModelOutput struct {
	RunDetails              RunDetails          `json:"run_details,omitempty"`
	FinalParameterEstimates ParameterEstimates  `json:"final_parameter_estimates,omitempty"`
	FinalParameterStdErr    ParameterEstimates  `json:"final_parameter_std_err,omitempty"`
	ParameterStructures     ParameterStructures `json:"parameter_structures,omitempty"`
	ParameterNames          ParameterNames      `json:"parameter_names,omitempty"`
	OFV                     OfvDetails          `json:"ofv,omitempty"`
	ShrinkageDetails        ShrinkageDetails    `json:"shrinkage_details,omitempty"`
}
