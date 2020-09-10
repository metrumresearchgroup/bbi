package parser

import "github.com/thoas/go-funk"

// createDiagonalBlock creates a slice of ints that would form a diagonal matrix with value 1 for diagonal and 0 for off diagonal elements
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
// Primarily used for deciding whether to look for a .grd file to parse
func CheckIfNotGradientBased(results SummaryOutput) bool {
	isNotGradientBased := funk.Contains([]string{
		"MCMC Bayesian Analysis",
		"Stochastic Approximation Expectation-Maximization",
		"Importance Sampling assisted by MAP Estimation",
		"Importance Sampling",
		"Importance Sampling (No Prior)",
		"NUTS Bayesian Analysis",
		"Objective Function Evaluation by Importance Sampling",
	}, results.RunDetails.EstimationMethods[len(results.RunDetails.EstimationMethods)-1])

	return isNotGradientBased
}

// Checks the final estimation method against a list of Bayesian methods.
func CheckIfBayesian(results SummaryOutput) bool {
	isBayesian := funk.Contains([]string{
		"MCMC Bayesian Analysis",
		"NUTS Bayesian Analysis",
	}, results.RunDetails.EstimationMethods[len(results.RunDetails.EstimationMethods)-1])

	return isBayesian
}
