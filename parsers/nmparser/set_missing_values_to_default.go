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

	// shrinkage eta's
	for methodIndex := range results.ShrinkageDetails {
		for subGroupIndex := range results.ShrinkageDetails[methodIndex] {
			if len(results.ShrinkageDetails[methodIndex][subGroupIndex].EtaSD) == 0 {
				results.ShrinkageDetails[methodIndex][subGroupIndex].EtaSD = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[methodIndex][subGroupIndex].EtaSD {
					results.ShrinkageDetails[methodIndex][subGroupIndex].EtaSD[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[methodIndex][subGroupIndex].EtaVR) == 0 {
				results.ShrinkageDetails[methodIndex][subGroupIndex].EtaVR = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[methodIndex][subGroupIndex].EtaVR {
					results.ShrinkageDetails[methodIndex][subGroupIndex].EtaVR[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[methodIndex][subGroupIndex].EbvSD) == 0 {
				results.ShrinkageDetails[methodIndex][subGroupIndex].EbvSD = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[methodIndex][subGroupIndex].EbvSD {
					results.ShrinkageDetails[methodIndex][subGroupIndex].EbvSD[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[methodIndex][subGroupIndex].EbvVR) == 0 {
				results.ShrinkageDetails[methodIndex][subGroupIndex].EbvVR = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[methodIndex][subGroupIndex].EbvVR {
					results.ShrinkageDetails[methodIndex][subGroupIndex].EbvVR[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[methodIndex][subGroupIndex].EtaBar) == 0 {
				results.ShrinkageDetails[methodIndex][subGroupIndex].EtaBar = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[methodIndex][subGroupIndex].EtaBar {
					results.ShrinkageDetails[methodIndex][subGroupIndex].EtaBar[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[methodIndex][subGroupIndex].EtaBarSE) == 0 {
				results.ShrinkageDetails[methodIndex][subGroupIndex].EtaBarSE = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[methodIndex][subGroupIndex].EtaBarSE {
					results.ShrinkageDetails[methodIndex][subGroupIndex].EtaBarSE[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[methodIndex][subGroupIndex].Pval) == 0 {
				results.ShrinkageDetails[methodIndex][subGroupIndex].Pval = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[methodIndex][subGroupIndex].Pval {
					results.ShrinkageDetails[methodIndex][subGroupIndex].Pval[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[methodIndex][subGroupIndex].NumSubjects) == 0 {
				results.ShrinkageDetails[methodIndex][subGroupIndex].NumSubjects = make([]float64, etaCount)
				for i := range results.ShrinkageDetails[methodIndex][subGroupIndex].NumSubjects {
					results.ShrinkageDetails[methodIndex][subGroupIndex].NumSubjects[i] = DefaultFloat64
				}
			}
		}
	}

	// shrinkage eps's
	for methodIndex := range results.ShrinkageDetails {
		for subGroupIndex := range results.ShrinkageDetails[methodIndex] {
			if len(results.ShrinkageDetails[methodIndex][subGroupIndex].EpsSD) == 0 {
				results.ShrinkageDetails[methodIndex][subGroupIndex].EpsSD = make([]float64, epsCount)
				for i := range results.ShrinkageDetails[methodIndex][subGroupIndex].EpsSD {
					results.ShrinkageDetails[methodIndex][subGroupIndex].EpsSD[i] = DefaultFloat64
				}
			}
			if len(results.ShrinkageDetails[methodIndex][subGroupIndex].EpsVR) == 0 {
				results.ShrinkageDetails[methodIndex][subGroupIndex].EpsVR = make([]float64, epsCount)
				for i := range results.ShrinkageDetails[methodIndex][subGroupIndex].EpsVR {
					results.ShrinkageDetails[methodIndex][subGroupIndex].EpsVR[i] = DefaultFloat64
				}
			}
		}
	}
}
