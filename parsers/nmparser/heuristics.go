package parser

// AnyTrue returns whether any heuristic has a true
// value.
func (hs RunHeuristics) AnyTrue() bool {
	if hs.CovarianceStepAborted {
		return true
	}
	if hs.LargeConditionNumber {
		return true
	}
	if hs.CorrelationsNotOK {
		return true
	}
	if hs.ParameterNearBoundary {
		return true
	}
	if hs.HessianReset {
		return true
	}
	if hs.HasFinalZeroGradient {
		return true
	}
	if hs.MinimizationTerminated {
		return true
	}

	return false
}

func (hs RunHeuristics) ErrorStrings() []string {
	var errors []string
	if hs.CovarianceStepAborted {
		errors = append(errors, "Covariance Step Aborted")
	}
	if hs.LargeConditionNumber {
		errors = append(errors, "Large Condition Number")
	}
	if hs.CorrelationsNotOK {
		errors = append(errors, "High Correlation")
	}
	if hs.ParameterNearBoundary {
		errors = append(errors, "Parameter Near Boundary")
	}
	if hs.HessianReset {
		errors = append(errors, "Hessian Reset")
	}
	if hs.HasFinalZeroGradient {
		errors = append(errors, "Final Zero Gradient")
	}
	if hs.MinimizationTerminated {
		errors = append(errors, "Minimization Terminated")
	}

	return errors
}
