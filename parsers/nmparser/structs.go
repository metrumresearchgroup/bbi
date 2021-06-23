package parser

// FlatArray provides a slice of values meant to be coerced to a matrix
// of the dimensions Dim
// This allows easy coercion to matrices in languages like R
// using matrix(Values, nrow = Dim).
type FlatArray struct {
	Values []float64 `json:"values,omitempty"`
	Dim    int       `json:"dim,omitempty"`
}

// ParameterNames contains the names of model parameters.
type ParameterNames struct {
	Theta []string `json:"theta,omitempty"`
	Omega []string `json:"omega,omitempty"`
	Sigma []string `json:"sigma,omitempty"`
}

// ParametersResult contains data about the parameter values for a model.
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

// ParametersData contains data for each method.
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
// some values are defined as pointers to support tri-state: true, false, nil.
type RunHeuristics struct {
	CovarianceStepAborted  bool `json:"covariance_step_aborted"`
	LargeConditionNumber   bool `json:"large_condition_number"`
	EigenvalueIssues       bool `json:"eigenvalue_issues"`
	CorrelationsNotOK      bool `json:"correlations_not_ok"`
	ParameterNearBoundary  bool `json:"parameter_near_boundary"`
	HessianReset           bool `json:"hessian_reset"`
	HasFinalZeroGradient   bool `json:"has_final_zero_gradient"`
	MinimizationTerminated bool `json:"minimization_terminated"`
	EtaPvalSignificant     bool `json:"eta_pval_significant"`
	PRDERR                 bool `json:"prderr"`
}

// RunDetails contains key information about logistics of the model run.
type RunDetails struct {
	Version             string   `json:"version,omitempty"`
	RunStart            string   `json:"run_start,omitempty"`
	RunEnd              string   `json:"run_end,omitempty"`
	EstimationTime      float64  `json:"estimation_time,omitempty"`
	CovarianceTime      float64  `json:"covariance_time,omitempty"`
	CpuTime             float64  `json:"cpu_time,omitempty"`
	FunctionEvaluations int64    `json:"function_evaluations,omitempty"`
	SignificantDigits   float64  `json:"significant_digits,omitempty"`
	ProblemText         string   `json:"problem_text,omitempty"`
	ModFile             string   `json:"mod_file,omitempty"`
	EstimationMethods   []string `json:"estimation_method,omitempty"`
	DataSet             string   `json:"data_set,omitempty"`
	NumberOfSubjects    int64    `json:"number_of_subjects,omitempty"`
	NumberOfObs         int64    `json:"number_of_obs,omitempty"`
	NumberOfDataRecords int64    `json:"number_of_data_records,omitempty"`
	OutputTables        []string `json:"output_tables,omitempty"`
	OutputFilesUsed     []string `json:"output_files_used,omitempty"`
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
// Type 11=%Relative information

// Types 1-7 are supported in NOMMEM 73
// Types 8-10 added in NONMEM 74
// Type 11 added in NONMEM 75

// ShrinkageDetails ...
type ShrinkageDetails struct {
	SubPop              int64     `json:"sub_pop,omitempty"`
	EtaBar              []float64 `json:"eta_bar,omitempty"`
	EtaBarSE            []float64 `json:"ebv_bar_se,omitempty"`
	Pval                []float64 `json:"pval,omitempty"`
	EtaSD               []float64 `json:"eta_sd,omitempty"`
	EpsSD               []float64 `json:"eps_sd,omitempty"`
	EbvSD               []float64 `json:"ebv_sd,omitempty"`
	NumSubjects         []float64 `json:"num_subjects,omitempty"`
	EtaVR               []float64 `json:"eta_vr,omitempty"`
	EpsVR               []float64 `json:"eps_vr,omitempty"`
	EbvVR               []float64 `json:"ebv_vr,omitempty"`
	RelativeInformation []float64 `json:"relative_information,omitempty"`
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
	EstMethod       string  `json:"method,omitempty"`
	OFVNoConstant   float64 `json:"ofv_no_constant,omitempty"`
	ConstantToOFV   float64 `json:"constant_to_ofv,omitempty"`
	OFVWithConstant float64 `json:"ofv_with_constant,omitempty"`
}

// OfvDetails ...
type ConditionNumDetails struct {
	EstMethod       string  `json:"method,omitempty"`
	ConditionNumber float64 `json:"condition_number,omitempty"`
}

// SummaryOutput is the output struct from a lst file.
type SummaryOutput struct {
	RunDetails       RunDetails            `json:"run_details,omitempty"`
	RunHeuristics    RunHeuristics         `json:"run_heuristics,omitempty"`
	ParametersData   []ParametersData      `json:"parameters_data,omitempty"`
	ParameterNames   ParameterNames        `json:"parameter_names,omitempty"`
	OFV              []OfvDetails          `json:"ofv,omitempty"`
	ConditionNumber  []ConditionNumDetails `json:"condition_number,omitempty"`
	ShrinkageDetails [][]ShrinkageDetails  `json:"shrinkage_details,omitempty"`
}

// CovCorOutput is the output from parsing the .cov and .cor file.
type CovCorOutput struct {
	CovarianceTheta  []FlatArray `json:"covariance_theta,omitempty"`
	CorrelationTheta []FlatArray `json:"correlation_theta,omitempty"`
}

// ExtData provides an intermediate representation of the ExtData after iterations have been stripped out
// and the various tables broken out.
type ExtData struct {
	EstimationMethods []string
	ParameterNames    []string
	EstimationLines   [][]string
}

type ExtFastData struct {
	EstimationMethods []string
	ParameterNames    []string
	EstimationLines   [][]string
	TerminationCodes   [][]string
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

// NewRunDetails create RunDetails struct with default values.
func NewRunDetails() RunDetails {
	runDetails := RunDetails{
		Version:             DefaultString,
		RunStart:            DefaultString,
		RunEnd:              DefaultString,
		EstimationTime:      DefaultFloat64,
		CovarianceTime:      DefaultFloat64,
		CpuTime:             DefaultFloat64,
		FunctionEvaluations: DefaultInt64,
		SignificantDigits:   DefaultFloat64,
		ProblemText:         DefaultString,
		ModFile:             DefaultString,
		EstimationMethods:   []string{},
		DataSet:             DefaultString,
		NumberOfSubjects:    DefaultInt64,
		NumberOfObs:         DefaultInt64,
		NumberOfDataRecords: DefaultInt64,
		OutputTables:        []string{},
		OutputFilesUsed:     []string{},
	}

	return runDetails
}

// NewOfvDetails ...
func NewOfvDetails(method string) OfvDetails {
	ofvDetails := OfvDetails{
		EstMethod:       method,
		OFVNoConstant:   DefaultFloat64,
		ConstantToOFV:   DefaultFloat64,
		OFVWithConstant: DefaultFloat64,
	}

	return ofvDetails
}

func NewConditionNumDetails(method string, condNum float64) ConditionNumDetails {
	conditionNumDetails := ConditionNumDetails{
		EstMethod:       method,
		ConditionNumber: condNum,
	}

	return conditionNumDetails
}

// NewRunHeuristics provides a new run heuristics struct
// at the moment it just returns all defaults, however
// this abstraction will allow more flexible refactoring
// to control additional default logic if needed.
// For now, the idea is to define the heuristic such that
// given a true result, it is a negative outcome.
// For example, rather than using a Heuristic CorrelationOK
// in which a default false would cause significant logic
// to test to make sure they were OK, where the implication
// is that unless otherwise warned, they are.
// Hence, using the Heuristic CorrelationNotOK allows
// a reasonable default false, that can be detected true.
func NewRunHeuristics() RunHeuristics {
	return RunHeuristics{}
}
