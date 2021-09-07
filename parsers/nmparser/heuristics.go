package parser

// AnyTrue returns whether any heuristic has a true
// value
func (hs RunHeuristics) AnyTrue() bool {
	if hs.CovarianceStepAborted == true {
		return true
	}
	if hs.LargeConditionNumber == true {
		return true
	}
	if hs.CorrelationsNotOK == true {
		return true
	}
	if hs.ParameterNearBoundary == true {
		return true
	}
	if hs.HessianReset == true {
		return true
	}
	if hs.HasFinalZeroGradient == true {
		return true
	}
	if hs.MinimizationTerminated == true {
		return true
	}

	return false
}

func (hs RunHeuristics) ErrorStrings() []string {
	var errors []string
	if hs.CovarianceStepAborted == true {
		errors = append(errors, "Covariance Step Aborted")
	}
	if hs.LargeConditionNumber == true {
		errors = append(errors, "Large Condition Number")
	}
	if hs.CorrelationsNotOK == true {
		errors = append(errors, "High Correlation")
	}
	if hs.ParameterNearBoundary == true {
		errors = append(errors, "Parameter Near Boundary")
	}
	if hs.HessianReset == true {
		errors = append(errors, "Hessian Reset")
	}
	if hs.HasFinalZeroGradient == true {
		errors = append(errors, "Final Zero Gradient")
	}
	if hs.MinimizationTerminated == true {
		errors = append(errors, "Minimization Terminated")
	}

	return errors
}
