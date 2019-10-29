package parser

import (
	"fmt"
)

func setMissingValuesToDefault(results *ModelOutput, etaCount, epsCount int) {
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
				results.ParameterNames.Theta = append(results.ParameterNames.Theta, fmt.Sprintf("Theta%v", n+1))
			}
		}
		if len(results.ParameterNames.Omega) == 0 {
			results.ParameterNames.Omega = GetBlockParameterNames("OMEGA", omegaCount)
		}
		if len(results.ParameterNames.Sigma) == 0 {
			results.ParameterNames.Sigma = GetBlockParameterNames("SIGMA", omegaCount)
		}
	}

	// shrinkage eta's
	if etaCount > 0 {
		for n := range results.ShrinkageDetails {
			if len(results.ShrinkageDetails[n].EtaSD) == 0 {
				results.ShrinkageDetails[n].EtaSD = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[n].EtaSD {
					results.ShrinkageDetails[n].EtaSD[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[n].EtaVR) == 0 {
				results.ShrinkageDetails[n].EtaVR = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[n].EtaVR {
					results.ShrinkageDetails[n].EtaVR[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[n].EbvSD) == 0 {
				results.ShrinkageDetails[n].EbvSD = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[n].EbvSD {
					results.ShrinkageDetails[n].EbvSD[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[n].EbvVR) == 0 {
				results.ShrinkageDetails[n].EbvVR = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[n].EbvVR {
					results.ShrinkageDetails[n].EbvVR[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[n].EtaBar) == 0 {
				results.ShrinkageDetails[n].EtaBar = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[n].EtaBar {
					results.ShrinkageDetails[n].EtaBar[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[n].EtaBarSE) == 0 {
				results.ShrinkageDetails[n].EtaBarSE = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[n].EtaBarSE {
					results.ShrinkageDetails[n].EtaBarSE[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[n].Pval) == 0 {
				results.ShrinkageDetails[n].Pval = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[n].Pval {
					results.ShrinkageDetails[n].Pval[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[n].NumSubjects) == 0 {
				results.ShrinkageDetails[n].NumSubjects = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[n].NumSubjects {
					results.ShrinkageDetails[n].NumSubjects[i] = DefaultFloat64
				}
			}
		}
	}

	// shrinkage eps's
	if epsCount > 0 {
		for n := range results.ShrinkageDetails {
			if len(results.ShrinkageDetails[n].EpsSD) == 0 {
				results.ShrinkageDetails[n].EpsSD = make([]float64, epsCount)
				for i := range results.ShrinkageDetails[n].EpsSD {
					results.ShrinkageDetails[n].EpsSD[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[n].EpsVR) == 0 {
				results.ShrinkageDetails[n].EpsVR = make([]float64, epsCount)
				for i := range results.ShrinkageDetails[n].EpsVR {
					results.ShrinkageDetails[n].EpsVR[i] = DefaultFloat64
				}
			}
		}
	}
}
