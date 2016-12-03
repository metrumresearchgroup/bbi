[![Circle CI](https://circleci.com/gh/dpastoor/nonmemutils.svg?style=svg)](https://circleci.com/gh/dpastoor/nonmemutils)
[![Go Report Card](https://goreportcard.com/badge/github.com/dpastoor/nonmemutils)](https://goreportcard.com/report/github.com/dpastoor/nonmemutils)

# nonmemutils
nonmem utility functions written in Go with an emphasis on full test coverage

This project has two main goals

1) to support my use cases in nonmem, as well as those requested by collegues
* handle parsing of lst files for command line summaries and as JSON output to be pulled into other programs like R
* execute jobs and support nonmem 7.4's pre-compiled binary features
* manage run submissions 'globally' in that a single run submission should not lock out the shell if you don't want, and all runs should be aware of one-another as to not oversaturate resources, namely CPU.
* clean up and provide some helpers for managing output from PsN submitted jobs, namely more first class integration with git.
* provide simulation and re-estimation templating 
* first class hooks to R for pre- and post- processing

2) to learn and experiment with go.

By using go, this utility is nicely cross platform, and is distributed as a single binary.

** Currently the software is considered pre-alpha and documentation is non-existant **. However, go, being a statically typed language and simple in it's structures, makes it _reasonably_ easy to look at the raw source to get a feel for what is going on.


Example output as of september 2016 (with new fields continually being added)
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

With tabular outputs that look like:

```
+-----------+----------+-------------------+--------------+------------+--------------+
|   Theta   |   Name   |   Estimate (SN)   |   Estimate   |   StdErr   |   SE (RSE)   |
+-----------+----------+-------------------+--------------+------------+--------------+
|   TH 1    |     KA   |        2.31E+00   |       2.31   |   0.0862   |      3.7 %   |
|   TH 2    |     CL   |        5.50E+01   |         55   |     3.33   |      6.1 %   |
|   TH 3    |     V2   |        4.65E+02   |        465   |     29.7   |      6.4 %   |
|   TH 4    |   RUVp   |       -8.06E-02   |    -0.0806   |   0.0555   |     68.9 %   |
|   TH 5    |   RUVa   |        4.13E+00   |       4.13   |     1.36   |     32.9 %   |
+-----------+----------+-------------------+--------------+------------+--------------+
```
