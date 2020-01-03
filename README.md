[![Go Report Card](https://goreportcard.com/badge/github.com/metrumresearchgroup/babylon)](https://goreportcard.com/report/github.com/metrumresearchgroup/babylon)

# babylon

Babylon is (will be) a complete solution for managing projects involving modeling and simulation with a number 
of software solutions used in pharmaceutical sciences. 
This is a fork of the [nonmemutils project](https://github.com/dpastoor/nonmemutils) that is broader in scope.

Initial support encompasses NONMEM  however the api is designed in a way to be flexible to handle other software.

Most components are written in Go, a language championed by google. By using go, virtually all components are nicely cross platform,
 and can be distributed as a single binary with no (required) dependencies for the user.

** Currently the software is considered pre-alpha and documentation is non-existant **. However, go, 
being a statically typed language and simple in it's structures, 
makes it _reasonably_ easy to look at the raw source to get a feel for what is going on.

Babylon is broken up into a couple core components:

### babylon-cli

command line interface for executing and managing models and projects. The nomenclature for the command line is relatively simple, following a similar pattern regardless of the modeling software or execution mode. 

`bbi <modeling software> <execution mode> path_to_model`

For Example:

`bbi nonmem run sge /abs/path/to/file.mod`

 * `nonmem` : The modeling software we should be targeting for this run
 * `sge` : The mode of execution. For nonmem this can either be local or sge, with sge indicating submission of jobs to the grid
 * `/abs/path/to/file.mod` : The location of the file to submit for execution. Can be relative or absolute. 

 #### Viper Configuration

 While the Babylon cli is extremely configurable via flags on execution, such as: 

 `bbi nonmem run --cleanLvl 2 --copyLvl 1 --overwrite=true --git=true ... /path/to/file.mod`

 For automation and reproducability purposes, that can be painful, leading to scripting purely for the purpose of re-executing the job the same was it was originally run. This can be remedied via a `yaml` config file in one of two ways:

 * The `--saveConfig` flag will take all the flags you have passed and write it to `babylon.yaml` in the same directory as the model file you provide as an argument
 * You may also use the `bbi init` command in the directory with your models to create a default configuration file. you may alter this as necessary to meet your needs.

 Configurations for execution are located (in order of priority)

 * In the Model Directory
 * In the directory from which Babylon is executed
 * In the executing user's home (`~`) directory


 #### Viper and SGE
 You may notice that if you issue a job with nonmen targeting SGE that a babylon.yaml file is created for you automatically. This is because SGE execution wraps the Babylon CLI into an executable for the grid to execute. This ensures the following:

 * Execution via SGE is done **the exact same way** that local execution for nonmem occurs. 
 * This includes cleanup, copy-up, and git operations
 * Also ensures a single execution path 

 To do this sanely, we make sure that the parameters provided on the initial SGE run are captured and stored with the model. This way, each subsequent Babylon call made from the grid is made with the *exact same parameters*.

### babylon-server

a server component that can help provide an entry-point for non-cli applications, or hook into the 
cli to allow for more managed and/or asyncronous execution patterns. For example, queueing models
to run under specific resource constraints or in a cluster environment.

### internal components

Internal components, such as the parsers, runners, and other utility data expose their
api in such a way that at minimum can be used by both the cli and server

### Example output

Example output of NONMEM run summary as of November 2016 (with new fields continually being added)

By controlling the CLI flags, it can either be an object that can be consumed by other programs such as R:

```
{                                                                     
        "RunDetails": {                                               
                "NMversion": "7.2.0",                                 
                "RunStart": "Tue Dec 17 18:10:55 2013",               
                "RunEnd": "Tue Dec 17 18:11:32 2013",                 
                "EstimationTime": 6.84,                               
                "CovarianceTime": 3.34,                               
                "FunctionEvaluations": 352,                           
                "SignificantDigits": 3.4                              
        },                                                            
        "FinalParameterEstimates": {                                  
                "Theta": [                                            
                        "2.31E+00",                                   
                        "4.25E+01",                                   
                        "4.64E+02",                                   
                        "-8.09E-02",                                  
                        "4.14E+00"                                    
                ],                                                    
                "Omega": [                                            
                        "1.23E-01",                                   
                        "0.00E+00",                                   
                        "1.54E-01"                                    
                ],                                                    
                "Sigma": [                                            
                        "1.00E+00"                                    
                ],                                                    
                "OmegaCorr": [                                        
                        "3.51E-01",                                   
                        "0.00E+00",                                   
                        "3.92E-01"                                    
                ],                                                    
                "SigmaCorr": [                                        
                        "1.00E+00"                                    
                ]                                                     
        },                                                            
        "FinalParameterStdErr": {                                     
                "Theta": [                                            
                        "8.53E-02",                                   
                        "2.90E+00",                                   
                        "2.96E+01",                                   
                        "5.55E-02",                                   
                        "1.36E+00"                                    
                ],                                                    
                "Omega": [                                            
                        "2.23E-02",                                   
                        ".........",                                  
                        "2.67E-02"                                    
                ],                                                    
                "Sigma": [                                            
                        "........."                                   
                ],                                                    
                "OmegaCorr": [                                        
                        "3.18E-02",                                   
                        ".........",                                  
                        "3.40E-02"                                    
                ],                                                    
                "SigmaCorr": [                                        
                        "........."                                   
                ]                                                     
        },                                                            
        "ParameterStructures": {                                      
                "Theta": 5,                                           
                "Omega": [                                            
                        1,                                            
                        0,                                            
                        1                                             
                ],                                                    
                "Sigma": [                                            
                        1                                             
                ]                                                     
        },                                                            
        "ParameterNames": {                                           
                "Theta": [                                            
                        "KA",                                         
                        "CL",                                         
                        "V2",                                         
                        "RUVp",                                       
                        "RUVa"                                        
                ],                                                    
                "Omega": [],                                          
                "Sigma": []                                           
        },                                                            
        "OFV": 2643.561                                               
}    
```

or human readable tabular formats such as:

```
+-----------+----------+-------------------+--------------+---------------------+
|   Theta   |   Name   |   Estimate (SN)   |   Estimate   |      StdErr (RSE)   |
+-----------+----------+-------------------+--------------+---------------------+
|   TH 1    |     KA   |        2.31E+00   |       2.31   |    0.0862 (3.7 %)   |
|   TH 2    |     CL   |        5.50E+01   |         55   |      3.33 (6.1 %)   |
|   TH 3    |     V2   |        4.65E+02   |        465   |      29.7 (6.4 %)   |
|   TH 4    |   RUVp   |       -8.06E-02   |    -0.0806   |   0.0555 (68.9 %)   |
|   TH 5    |   RUVa   |        4.13E+00   |       4.13   |     1.36 (32.9 %)   |
+-----------+----------+-------------------+--------------+---------------------+
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
