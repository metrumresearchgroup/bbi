[![Go Report Card](https://goreportcard.com/badge/github.com/metrumresearchgroup/bbi)](https://goreportcard.com/report/github.com/metrumresearchgroup/bbi)

# bbi

bbi is (will be) a complete solution for managing projects involving modeling and simulation with a number 
of software solutions used in pharmaceutical sciences. 

Initial support encompasses NONMEM  however the api is designed in a way to be flexible to handle other software.

Most components are written in Go, a language championed by google. By using go, virtually all components are nicely cross platform,
 and can be distributed as a single binary with no (required) dependencies for the user.

### bbi CLI

command line interface for executing and managing models and projects. The nomenclature for the command line is relatively simple, following a similar pattern regardless of the modeling software or execution mode. 

`bbi <modeling software> <execution mode> path_to_model`

For Example:

`bbi nonmem run sge path/to/file.mod`

 * `nonmem` : The modeling software we should be targeting for this run
 * `sge` : The mode of execution. For nonmem this can either be local or sge, with sge indicating submission of jobs to the grid
 * `path/to/file.mod` : The location of the file to submit for execution. Can be relative or absolute. 

 #### Configuration

 While the `bbi` CLI is extremely configurable via flags on execution, such as: 

 `bbi nonmem run --cleanLvl 2 --copyLvl 1 --overwrite=true --git=true ... /path/to/file.mod`

 For automation and reproducability purposes, that can be painful, leading to scripting purely for the purpose of re-executing the job the same was it was originally run. This can be remedied via a `yaml` config file in one of two ways:

 * The `--saveConfig` flag will take all the flags you have passed and write it to `bbi.yaml` in the same directory as the model file you provide as an argument
 * You may also use the `bbi init` command in the directory with your models to create a default configuration file. you may alter this as necessary to meet your needs.

 Configurations for execution are located (in order of priority)

 * In the Model Directory
 * In the directory from which `bbi` is executed
 * In the executing user's home (`~`) directory


 #### Configuration and SGE

 You may notice that if you issue a job with nonmem targeting SGE that a `bbi.yaml` file is created for you automatically. This is because SGE execution wraps the `bbi` CLI into an executable for the grid to execute. This ensures the following:

 * Execution via SGE is done **the exact same way** that local execution for nonmem occurs. 
 * This includes cleanup, copy-up, and git operations
 * Also ensures a single execution path 

 To do this sanely, we make sure that the parameters provided on the initial SGE run are captured and stored with the model. This way, each subsequent `bbi` call made from the grid is made with the *exact same parameters*.

### Summary output

Example output of NONMEM run summary as of Jan 2021 

By controlling the CLI flags, it can either be an object that can be consumed by other programs such as R:

```
bbi nonmem summary 103/103 --json
```

```
{
	"run_details": {
		"version": "7.4.4",
		"run_start": "-999999999",
		"run_end": "Tue Jan 19 11:47:45 EST 2021",
		"estimation_time": 48.01,
		"covariance_time": 1.47,
		"cpu_time": 57.706,
		"function_evaluations": 1048,
		"significant_digits": 3.3,
		"problem_text": "LEM From bbr: see 103.yaml for details",
		"mod_file": "-999999999",
		"estimation_method": [
			"First Order Conditional Estimation with Interaction"
		],
		"data_set": "../../../data/derived/analysis3.csv",
		"number_of_patients": 160,
		"number_of_obs": 3142,
		"number_of_data_records": 4292,
		"output_files_used": [
			"103.lst",
			"103.cpu",
			"103.ext",
			"103.grd",
			"103.shk"
		]
	},
	"run_heuristics": {
		"covariance_step_aborted": false,
		"large_condition_number": false,
		"correlations_not_ok": false,
		"parameter_near_boundary": false,
		"hessian_reset": false,
		"has_final_zero_gradient": false,
		"minimization_terminated": false,
		"eta_pval_significant": false,
		"prderr": false
	},
	"parameters_data": [
		{
			"method": "TABLE NO.     1: First Order Conditional Estimation with Interaction: Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
			"estimates": {
				"theta": [
					-1.94009,
					1.76696,
					1.08739,
					4.57113,
					1.64519
				],
				"omega": [
					0.0819738,
					0.0980853,
					0.268494,
					-0.00359228,
					-0.0192407,
					0.146635
				],
				"sigma": [
					33063.6
				]
			},
			"std_err": {
				"theta": [
					0.0727892,
					0.114614,
					0.0373194,
					0.0494494,
					0.0744255
				],
				"omega": [
					0.0181149,
					0.0321424,
					0.067229,
					0.0188361,
					0.0392745,
					0.0226792
				],
				"sigma": [
					4892.95
				]
			},
			"random_effect_sd": {
				"omega": [
					0.286311,
					0.661149,
					0.518164,
					-0.0327652,
					-0.0969695,
					0.38293
				],
				"sigma": [
					181.834
				]
			},
			"random_effect_sdse": {
				"omega": [
					0.031635,
					0.112879,
					0.0648724,
					0.173561,
					0.199903,
					0.0296127
				],
				"sigma": [
					13.4544
				]
			},
			"fixed": {
				"theta": [
					0,
					0,
					0,
					0,
					0
				],
				"omega": [
					0,
					0,
					0,
					0,
					0,
					0
				],
				"sigma": [
					0
				]
			}
		}
	],
	"parameter_names": {
		"theta": [
			"THETA1",
			"THETA2",
			"THETA3",
			"THETA4",
			"THETA5"
		],
		"omega": [
			"OMEGA(1,1)",
			"OMEGA(2,1)",
			"OMEGA(2,2)",
			"OMEGA(3,1)",
			"OMEGA(3,2)",
			"OMEGA(3,3)"
		],
		"sigma": [
			"SIGMA(1,1)"
		]
	},
	"ofv": [
		{
			"method": "First Order Conditional Estimation with Interaction",
			"ofv_no_constant": 36411.268,
			"constant_to_ofv": 5774.609742658163,
			"ofv_with_constant": 42185.877707146574
		}
	],
	"condition_number": [
		{
			"method": "First Order Conditional Estimation with Interaction",
			"condition_number": 1
		}
	],
	"shrinkage_details": [
		[
			{
				"sub_pop": 1,
				"eta_bar": [
					0.00549081,
					-0.00128784,
					-0.00450789
				],
				"ebv_bar_se": [
					0.0142059,
					0.0208238,
					0.0250342
				],
				"pval": [
					0.699114,
					0.950686,
					0.857098
				],
				"eta_sd": [
					37.0419,
					49.0066,
					17.0463
				],
				"eps_sd": [
					3.49867
				],
				"ebv_sd": [
					36.8599,
					48.6304,
					16.9266
				],
				"num_subjects": [
					160,
					160,
					160
				],
				"eta_vr": [
					60.3627,
					73.9967,
					31.1869
				],
				"eps_vr": [
					6.87493
				],
				"ebv_vr": [
					60.1333,
					73.6116,
					30.9881
				]
			}
		]
	]
}
```

or human readable tabular formats such as:

```
bbi nonmem summary 103/103 
```

```
PROBLEM From bbr: see 103.yaml for details
Dataset: ../../../data/derived/analysis3.csv
Records: 4292   Observations: 3142  Patients: 160
Estimation Method(s):
 - First Order Conditional Estimation with Interaction
No Heuristic Problems Detected
+-------+--------+----------+------------------+
| THETA |  NAME  | ESTIMATE |   STDERR (RSE)   |
+-------+--------+----------+------------------+
| TH 1  | THETA1 | -1.94009 | 0.0727892 (3.8%) |
| TH 2  | THETA2 | 1.76696  | 0.114614 (6.5%)  |
| TH 3  | THETA3 | 1.08739  | 0.0373194 (3.4%) |
| TH 4  | THETA4 | 4.57113  | 0.0494494 (1.1%) |
| TH 5  | THETA5 | 1.64519  | 0.0744255 (4.5%) |
+-------+--------+----------+------------------+
+------------+------+----------+---------------+
|   OMEGA    | ETA  | ESTIMATE | SHRINKAGE (%) |
+------------+------+----------+---------------+
| OMEGA(1,1) | ETA1 | 0.081974 | 37.041900     |
| OMEGA(2,2) | ETA2 | 0.268494 | 49.006600     |
| OMEGA(3,3) | ETA3 | 0.146635 | 17.046300     |
+------------+------+----------+---------------+
```


### Development

Test-driven development encouraged, facilitated by goconvey to monitor coverage
and re-run tests on change(s)

During development, while installing new versions for testing, running "make install" will auto-append a timestamp to the build version.

```
$ go get github.com/smartystreets/goconvey
goconvey 
```

a web UI will be started at localhost:8080 with file watchers to rerun tests on 
file change(s)
