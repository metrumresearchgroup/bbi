[![Circle CI](https://circleci.com/gh/metrumresearchgroup/babylon.svg?style=svg)](https://circleci.com/gh/metrumresearchgroup/babylon)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/metrumresearchgroup/babylon?branch=master&svg=true)](https://ci.appveyor.com/project/metrumresearchgroup/babylon)
[![Go Report Card](https://goreportcard.com/badge/github.com/metrumresearchgroup/babylon)](https://goreportcard.com/report/github.com/metrumresearchgroup/babylon)

# babylon

Babylon is (will be) a complete solution for managing projects involving modeling and simulation with a number 
of software solutions used in pharmaceutical sciences. 
This is a fork of the [nonmemutils project](https://github.com/dpastoor/nonmemutils) that is broader in scope.

Initial support encompasses NONMEM and STAN, however the api is designed in a way to be flexible to handle other software.

Most components are written in Go, a language championed by google. By using go, virtually all components are nicely cross platform,
 and can be distributed as a single binary with no (required) dependencies for the user.

** Currently the software is considered pre-alpha and documentation is non-existant **. However, go, 
being a statically typed language and simple in it's structures, 
makes it _reasonably_ easy to look at the raw source to get a feel for what is going on.

Babylon is broken up into a couple core components:

### babylon-cli

command line interface for executing and managing models and projects

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
