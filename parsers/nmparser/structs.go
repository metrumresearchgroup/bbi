package parser

// ParameterNames containst the names of model parameters
type ParameterNames struct {
	Theta []string `json:"theta,omitempty"`
	Omega []string `json:"omega,omitempty"`
	Sigma []string `json:"sigma,omitempty"`
}

// ParametersResult contains data about the parameter values for a model
type ParametersResult struct {
	Theta []float64 `json:"theta,omitempty"`
	Omega []float64 `json:"omega,omitempty"`
	Sigma []float64 `json:"sigma,omitempty"`
}

// RandomEffectResult ...
type RandomEffectResult struct {
	Omega []float64 `json:"omega,omitempty"`
	Sigma []float64 `json:"sigma,omitempty"`
}

// ParametersData contains data for each method
type ParametersData struct {
	Method    string           `json:"method,omitempty"`
	Estimates ParametersResult `json:"estimates,omitempty"`
	StdErr    ParametersResult `json:"std_err,omitempty"`
	// indicates this line contains the OMEGA and SIGMA elements in
	// standard deviation/correlation format
	RandomEffectSD RandomEffectResult `json:"random_effect_sd,omitempty"`
	// indicates this line contains the standard errors to the OMEGA and
	// SIGMA elements in standard deviation/correlation format
	RandomEffectSDSE RandomEffectResult `json:"random_effect_sdse,omitempty"`
	Fixed            ParametersResult   `json:"fixed,omitempty"`
}

// RunHeuristics ...
// some values are defined as pointers to support tri-state: true, false, nil
type RunHeuristics struct {
	CovarianceStepOmitted  HeuristicStatus `json:"covariance_step_omitted,omitempty"`
	LargeConditionNumber   HeuristicStatus `json:"large_condition_number,omitempty"`
	CorrelationsOk         HeuristicStatus `json:"correlations_ok,omitempty"`
	ParameterNearBoundary  HeuristicStatus `json:"parameter_near_boundary,omitempty"`
	HessianReset           HeuristicStatus `json:"hessian_reset,omitempty"`
	HasFinalZeroGradient   HeuristicStatus `json:"has_final_zero_gradient,omitempty"`
	MinimizationSuccessful HeuristicStatus `json:"minimization_successful,omitempty"`
}

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
	OutputFilesUsed     []string `json:"output_files_used,omitempty"`
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
	RunDetails          RunDetails          `json:"run_details,omitempty"`
	RunHeuristics       RunHeuristics       `json:"run_heuristics,omitempty"`
	ParametersData      []ParametersData    `json:"parameters_data,omitempty"`
	ParameterStructures ParameterStructures `json:"parameter_structures,omitempty"`
	ParameterNames      ParameterNames      `json:"parameter_names,omitempty"`
	OFV                 OfvDetails          `json:"ofv,omitempty"`
	ShrinkageDetails    ShrinkageDetails    `json:"shrinkage_details,omitempty"`
	CovariateTheta      []FlatArray         `json:"covariate_theta,omitempty"`
}

// ExtData provides an intermediate representation of the ExtData after iterations have been stripped out
// and the various tables broken out
type ExtData struct {
	EstimationMethods []string
	ParameterNames    []string
	EstimationLines   [][]string
}

// MatrixData ...
type MatrixData struct {
	Values     [][]float64
	ThetaCount int
	OmageCount int
	SigmaCount int
}

// FlatArray ... to be consumed by R as matrix(Values, nrow=Dim)
type FlatArray struct {
	Values []float64
	Dim    int
}

// MakeFlatArray ...
func MakeFlatArray(matrix [][]float64, length int, dim int) FlatArray {
	if length > len(matrix) {
		panic("error making FlatArray")
	}

	values := make([]float64, length*length)
	k := 0
	for i := 0; i < length; i++ {
		for j := 0; j < length; j++ {
			values[k] = matrix[i][j]
			k++
		}
	}
	return FlatArray{
		Values: values,
		Dim:    dim,
	}
}
