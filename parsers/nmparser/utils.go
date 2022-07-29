package parser

import (
	"math"
	"strconv"
	"strings"
)

// createDiagonalBlock creates a slice of ints that would form a diagonal matrix with value 1 for diagonal and 0 for off diagonal elements.
func createDiagonalBlock(num int) []int {
	var iArr []int
	for i := 1; i <= num; i++ {
		for j := 1; j <= i; j++ {
			if j == i {
				iArr = append(iArr, 1)
			} else {
				iArr = append(iArr, 0)
			}
		}
	}

	return iArr
}

// Checks the final estimation method against a list of non-gradient based methods.
// Primarily used for deciding whether to look for a .grd file to parse.
func CheckIfNotGradientBased(results SummaryOutput) bool {
	nmethods := len(results.RunDetails.EstimationMethods)
	if nmethods == 0 {
		return false
	}

	finalMethod := results.RunDetails.EstimationMethods[nmethods-1]

	notGradientMethods := []string{
		"Bayesian Analysis",
		"Stochastic Approximation Expectation-Maximization",
		"Importance Sampling",
	}

	for _, m := range notGradientMethods {
		if strings.Contains(finalMethod, m) {
			return true
		}
	}

	return false
}

// Checks the final estimation method for the string "Bayesian Analysis"
// which is present in all Bayesian methods (MCMC, NUTS, etc.)
func CheckIfBayesian(results SummaryOutput) bool {
	nmethods := len(results.RunDetails.EstimationMethods)
	if nmethods == 0 {
		return false
	}

	isBayesian := strings.Contains(
		results.RunDetails.EstimationMethods[nmethods-1],
		"Bayesian Analysis",
	)

	return isBayesian
}

func strToFloat(s string) float64 {
	f, err := strconv.ParseFloat(s, 64)
	if err != nil || math.IsNaN(f) {
		f = DefaultFloat64
	}

	return f
}
