package parser

import (
	"fmt"
)

func setMissingValuesToDefault(results *SummaryOutput) {
	// method name
	for i := range results.ParametersData {
		if len(results.ParametersData[i].Method) == 0 {
			results.ParametersData[i].Method = "METHOD NOT DETECTED"
		}
	}

	// stderr
	for i := range results.ParametersData {
		var nameCount int

		thetaCount := len(results.ParametersData[i].Estimates.Theta)
		omegaCount := len(results.ParametersData[i].Estimates.Omega)
		sigmaCount := len(results.ParametersData[i].Estimates.Sigma)

		// stderr
		// if the "stderr" slice has less values than the "estimates" slice,
		// then pad the stderr slice with default values to match the dimesion
		nameCount = len(results.ParametersData[i].StdErr.Theta)
		for n := nameCount; n < thetaCount; n++ {
			results.ParametersData[i].StdErr.Theta = append(results.ParametersData[i].StdErr.Theta, DefaultFloat64)
		}
		nameCount = len(results.ParametersData[i].StdErr.Omega)
		for n := nameCount; n < omegaCount; n++ {
			results.ParametersData[i].StdErr.Omega = append(results.ParametersData[i].StdErr.Omega, DefaultFloat64)
		}
		nameCount = len(results.ParametersData[i].StdErr.Sigma)
		for n := nameCount; n < sigmaCount; n++ {
			results.ParametersData[i].StdErr.Sigma = append(results.ParametersData[i].StdErr.Sigma, DefaultFloat64)
		}

		// RandomEffectSD
		// if the "RandomEffectSD" slice has less values than the "estimates" slice,
		// then pad the RandomEffectSD slice with default values to match the dimesion
		nameCount = len(results.ParametersData[i].RandomEffectSD.Omega)
		for n := nameCount; n < omegaCount; n++ {
			results.ParametersData[i].RandomEffectSD.Omega = append(results.ParametersData[i].RandomEffectSD.Omega, DefaultFloat64)
		}
		nameCount = len(results.ParametersData[i].RandomEffectSD.Sigma)
		for n := nameCount; n < sigmaCount; n++ {
			results.ParametersData[i].RandomEffectSD.Sigma = append(results.ParametersData[i].RandomEffectSD.Sigma, DefaultFloat64)
		}

		// RandomEffectSDSE
		// if the "RandomEffectSDSE" slice has less values than the "estimates" slice,
		// then pad the RandomEffectSDSE slice with default values to match the dimesion
		nameCount = len(results.ParametersData[i].RandomEffectSDSE.Omega)
		for n := nameCount; n < omegaCount; n++ {
			results.ParametersData[i].RandomEffectSDSE.Omega = append(results.ParametersData[i].RandomEffectSDSE.Omega, DefaultFloat64)
		}
		nameCount = len(results.ParametersData[i].RandomEffectSDSE.Sigma)
		for n := nameCount; n < sigmaCount; n++ {
			results.ParametersData[i].RandomEffectSDSE.Sigma = append(results.ParametersData[i].RandomEffectSDSE.Sigma, DefaultFloat64)
		}

		// parameter names
		if len(results.ParameterNames.Theta) == 0 {
			for n := 0; n < thetaCount; n++ {
				results.ParameterNames.Theta = append(results.ParameterNames.Theta, fmt.Sprintf("THETA%v", n+1))
			}
		}
		if len(results.ParameterNames.Omega) == 0 {
			results.ParameterNames.Omega = GetBlockParameterNames("OMEGA", omegaCount)
		}
		if len(results.ParameterNames.Sigma) == 0 {
			results.ParameterNames.Sigma = GetBlockParameterNames("SIGMA", sigmaCount)
		}
	}

}
