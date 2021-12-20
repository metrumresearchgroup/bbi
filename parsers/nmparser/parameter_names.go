package parser

import "fmt"

// NewDefaultParameterNames creates a new Parameter Names
// structure Such that the labels are:
// THETA1, THETA2, ...
// OMEGA(1,1), OMEGA(1,2), OMEGA(2,2),...
// SIGMA(1,1), SIGMA(1,2), SIGMA(2,2),...
func NewDefaultParameterNames(nTheta int, nOmega int, nSigma int) ParameterNames {
	var thetaNames, omegaNames, sigmaNames []string
	for i := 1; i <= nTheta; i++ {
		thetaNames = append(thetaNames, fmt.Sprintf("THETA%v", i))
	}
	for i := 0; i < nOmega; i++ {
		omegaNames = append(omegaNames, fmt.Sprintf("OMEGA%s", omegaIndices[i]))
	}
	for i := 0; i < nSigma; i++ {
		sigmaNames = append(sigmaNames, fmt.Sprintf("SIGMA%s", omegaIndices[i]))
	}

	return ParameterNames{
		Theta: thetaNames,
		Omega: omegaNames,
		Sigma: sigmaNames,
	}
}
