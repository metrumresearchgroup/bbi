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
	Shrinkage                 []ShrinkageDetails `json:"shrinkage,omitempty"`
	Ofv                       OfvDetails         `json:"ofv,omitempty"`
	ZeroGradientDetected      bool               `json:"zero_gradient_detected,omitempty"`
	FinalZeroGradientDetected bool               `json:"final_zero_gradient_detected,omitempty"`
	CovStepComplete           bool               `json:"cov_step_complete,omitempty"`
	Messages                  []string           `json:"messages,omitempty"`
}

// Shrinkage Types
// Type 1=etabar
// Type 2=Etabar SE
// Type 3=P val
// Type 4=%Eta shrinkage SD version
// Type 5=%EPS shrinkage SD version
// Type 6=%Eta shrinkage based on empirical Bayes Variance (SD version)
// Type 7=number of subjects used.
// Type 8=%Eta shrinkage variance version
// Type 9=%EPS shrinkage variance version
// Type 10=%Eta shrinkage based on empirical Bayes Variance (variance version)

// Types 1-7 are supported in NOMMEM 73
// All types are supported in NONMEM 74

// ShrinkageDetails ...
type ShrinkageDetails struct {
	EtaBar      []float64 `json:"eta_bar,omitempty"`
	EtaBarSE    []float64 `json:"ebv_bar_se,omitempty"`
	Pval        []float64 `json:"pval,omitempty"`
	EtaSD       []float64 `json:"eta_sd,omitempty"`
	EpsSD       []float64 `json:"eps_sd,omitempty"`
	EbvSD       []float64 `json:"ebv_sd,omitempty"`
	NumSubjects []float64 `json:"num_subjects,omitempty"`
	EtaVR       []float64 `json:"eta_vr,omitempty"`
	EpsVR       []float64 `json:"eps_vr,omitempty"`
	EbvVR       []float64 `json:"ebv_vr,omitempty"`
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
	ShrinkageDetails    []ShrinkageDetails  `json:"shrinkage_details,omitempty"`
	CovarianceTheta     []FlatArray         `json:"covariance_theta,omitempty"`
	CorrelationTheta    []FlatArray         `json:"correlation_theta,omitempty"`
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

// DefaultInt ...
const DefaultInt = int(-999999999)

// DefaultInt32 ...
const DefaultInt32 = int32(-999999999)

// DefaultInt64 ...
const DefaultInt64 = int64(-999999999)

// DefaultFloat64 ...
const DefaultFloat64 = float64(-999999999)

// DefaultString ...
const DefaultString = "-999999999"

// NewRunDetails create RunDetails struct with default values
func NewRunDetails() RunDetails {
	runDetails := RunDetails{
		Version:             DefaultString,
		RunStart:            DefaultString,
		RunEnd:              DefaultString,
		EstimationTime:      DefaultFloat64,
		CovarianceTime:      DefaultFloat64,
		FunctionEvaluations: DefaultInt64,
		SignificantDigits:   DefaultFloat64,
		ProblemText:         DefaultString,
		ModFile:             DefaultString,
		EstimationMethod:    []string{},
		DataSet:             DefaultString,
		NumberOfPatients:    DefaultInt64,
		NumberOfObs:         DefaultInt64,
		NumberOfDataRecords: DefaultInt64,
		OutputTable:         DefaultString,
		OutputFilesUsed:     []string{},
	}
	return runDetails
}
