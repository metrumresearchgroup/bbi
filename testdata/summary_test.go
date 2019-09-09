package cmd

import (
	"encoding/json"
	"fmt"
	"os/exec"
	"testing"

	parser "github.com/babylon/parsers/nmparser"
	"github.com/stretchr/testify/assert"
)

func TestSummary(t *testing.T) {
	var tests = []struct {
		bbi                          string
		mod                          string
		context                      string
		expectedParameters           string
		expectedParametersStructures string
		expectedParameterNames       string
		expectedOfv                  string
		expectedShrinkage            string
	}{
		{
			bbi:                          "/usr/local/bin/bbi",
			mod:                          "./example-models/nonmem/BQL/2.mod",
			context:                      "BQL",
			expectedParameters:           "[{\"method\":\"TABLE NO.     1: First Order Conditional Estimation with Interaction: Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0\",\"estimates\":{\"theta\":[26.4905,282.616,297.043,58.749,1.5095,0.75,1,1],\"omega\":[0.100611,0,0.035999,0,0,0.0111726]},\"std_err\":{\"theta\":[0.60908,4.40586,2.32933,1.01296,0.0212776,10000000000,10000000000,10000000000],\"omega\":[0.00959481,10000000000,0.00353287,10000000000,10000000000,0.00186447]},\"random_effect_sd\":{\"omega\":[0.317192,0,0.189734,0,0,0.1057]},\"random_effect_sdse\":{\"omega\":[0.0151246,10000000000,0.00931006,10000000000,10000000000,0.00881961]},\"fixed\":{\"theta\":[0,0,0,0,0,1,1,1],\"omega\":[0,1,0,1,1,0]}}]",
			expectedParametersStructures: "{\"Theta\":9,\"Omega\":[1,0,1,0,0,1],\"Sigma\":[1]}",
			expectedParameterNames:       "{\"theta\":[\"1    CLF\",\"2    V2F\",\"3    V3F\",\"4    QF\",\"5    KA\",\"6 POW_CL\",\"7 POW_V2\",\"8 POW_V3\",\"9 POW_Q\"]}",
			expectedOfv:                  "{\"ofv\":4965.943833438051,\"ofv_no_constant\":-14346.006,\"ofv_with_constant\":-9380.062196247563}",
			expectedShrinkage:            "{\"eta\":{\"sd\":[0.40632,2.0606,18.484],\"vr\":[0.81099,4.0787,33.551]},\"ebv\":{\"sd\":[0.49256,2.1487,18.703],\"vr\":[0.98269,4.2512,33.908]},\"eps\":{\"sd\":[9.7026],\"vr\":[18.464]}}",
		},
		{
			bbi:                          "/usr/local/bin/bbi",
			mod:                          "./example-models/nonmem/IOVMM/10.mod",
			context:                      "IOVMM",
			expectedParameters:           "[{\"method\":\"TABLE NO.     1: First Order: Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0\",\"estimates\":{\"theta\":[14.2204,100.996,0.824135,0.988974,1.70925],\"omega\":[0.0490352,0,0.0482149,0,0,0.0359252,0,0,0,0.0359252,0,0,0,0,0.0359252],\"sigma\":[0.0458045,0]},\"std_err\":{},\"random_effect_sd\":{\"omega\":[0.221439,0,0.219579,0,0,0.189539,0,0,0,0.189539,0,0,0,0,0.189539],\"sigma\":[0.21402,0]},\"random_effect_sdse\":{},\"fixed\":{\"theta\":[0,0,0,0,0],\"omega\":[0,1,0,1,1,0,1,1,1,1,1,1,1,1,1],\"sigma\":[0,1]}}]",
			expectedParametersStructures: "{\"Theta\":6,\"Omega\":[1,0,2,0,0,3,0,0,0,3,0,0,0,0,3],\"Sigma\":[1,0,1]}",
			expectedParameterNames:       "{\"theta\":[\"1. CL POP 1\",\"2. V\",\"3. WT ON CL\",\"4. WT ON V\",\"5. CL POP 2\",\"6. PROB POP 1\"]}",
			expectedOfv:                  "{\"ofv\":23157.251036757752,\"ofv_no_constant\":17485.751,\"ofv_with_constant\":40643.001566747465}",
			expectedShrinkage:            "{\"eta\":{},\"ebv\":{},\"eps\":{}}",
		},
		// {
		// 	bbi:                          "/usr/local/bin/bbi",
		// 	mod:                          "./example-models/nonmem/IOVMM/10.mod",
		// 	context:                      "IOVMM",
		// 	expectedParameters:           "expectedParameters",
		// 	expectedParametersStructures: "expectedParametersStructures",
		// 	expectedParameterNames:       "expectedParameterNames",
		// 	expectedOfv:                  "expectedOfv",
		//  expectedShrinkage:            "expectedShrinkage",
		// },
	}

	for _, tt := range tests {
		stdout, err := exec.Command(tt.bbi, "summary", "--tree", tt.mod).Output()
		assert.Equal(t, nil, err, fmt.Sprintf("Fail exec: %s", err))

		var bbiSummary parser.ModelOutput //BayblonSummaryTree
		err = json.Unmarshal(stdout, &bbiSummary)
		assert.Equal(t, nil, err, fmt.Sprintf("Fail unmarshal: %s", tt.context))

		// assert.Equal(t, "7.4.3", bbiSummary.RunDetails.Version, fmt.Sprintf("Failed: %s", tt.context))
		// assert.Equal(t, int64(189), bbiSummary.RunDetails.FunctionEvaluations, fmt.Sprintf("Failed: %s", tt.context))

		parametersData, err := json.Marshal(bbiSummary.ParametersData)
		assert.Equal(t, nil, err, fmt.Sprintf("Fail marshal: %s", tt.context))
		assert.Equal(t, tt.expectedParameters, string(parametersData), fmt.Sprintf("Failed marshal: %s", tt.context))

		parametersStructures, err := json.Marshal(bbiSummary.ParameterStructures)
		assert.Equal(t, nil, err, fmt.Sprintf("Fail marshal: %s", tt.context))
		assert.Equal(t, tt.expectedParametersStructures, string(parametersStructures), fmt.Sprintf("Failed marshal: %s", tt.context))

		parametersNames, err := json.Marshal(bbiSummary.ParameterNames)
		assert.Equal(t, nil, err, fmt.Sprintf("Fail marshal: %s", tt.context))
		assert.Equal(t, tt.expectedParameterNames, string(parametersNames), fmt.Sprintf("Failed marshal: %s", tt.context))

		ofv, err := json.Marshal(bbiSummary.OFV)
		assert.Equal(t, nil, err, fmt.Sprintf("Fail marshal: %s", tt.context))
		assert.Equal(t, tt.expectedOfv, string(ofv), fmt.Sprintf("Failed marshal: %s", tt.context))

		shrinkageDetails, err := json.Marshal(bbiSummary.ShrinkageDetails)
		assert.Equal(t, nil, err, fmt.Sprintf("Fail marshal: %s", tt.context))
		assert.Equal(t, tt.expectedShrinkage, string(shrinkageDetails), fmt.Sprintf("Failed marshal: %s", tt.context))
	}
}
