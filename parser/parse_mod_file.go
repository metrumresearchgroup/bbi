package parser

import "strings"

// Est represents information about an estimation step
type Est struct {
	Method string
}

// Cov respresents information about the presence and settings for covariance step
type Cov struct {
	Ok       bool
	Settings string
}

// Sim represents information about the presence and settings for a simulation record
type Sim struct {
	Ok       bool
	Settings string
}

// Table represents information about an output table
type Table struct {
	File     string
	Settings string
}

// ModelInfo is the information about a model
type ModelInfo struct {
	Prob    string
	Routine string
	Est     []Est
	Cov     Cov
	Sim     Sim
	Tables  []Table
}

// ParseModInfo parses the model file
func ParseModInfo(lines []string) ModelInfo {

	var probLine string
	var routineLine string
	var estLines []int
	var cov Cov
	var sim Sim
	var tables []Table
	for i, line := range lines {
		switch {
		// these will currently be a false positive(s) for commented out lines until changed to a regex
		// checking for uncommented lines or commented lines are stripped before being passed to this function
		case strings.Contains(line, "$PROB"):
			probLine = line
		case strings.Contains(line, "$SUB"):
			routineLine = line
		case strings.Contains(line, "$EST"):
			estLines = append(estLines, i)
		case strings.Contains(line, "$COV"):
			cov.Ok = true
			cov.Settings = line
		case strings.Contains(line, "$SIM"):
			sim.Ok = true
			sim.Settings = line
		case strings.Contains(line, "$TABLE"):
			tables = append(tables, Table{Settings: line})
		default:
			continue
		}
	}

	var estRecords []Est
	if len(estLines) > 0 {
		// for now just give the whole bit of information, can parse later
		for _, i := range estLines {
			estRecords = append(estRecords, Est{Method: lines[i]})
		}
	}

	result := ModelInfo{
		Prob:    probLine,
		Routine: routineLine,
		Est:     estRecords,
		Cov:     cov,
		Sim:     sim,
		Tables:  tables,
	}
	return result
}
