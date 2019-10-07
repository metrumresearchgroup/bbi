package parser

import (
	"bytes"
	"encoding/json"
)

// HeuristicStatus supports extended states beyond true and false
type HeuristicStatus int

const (
	// HeuristicUndefined default value, not set to true or false
	HeuristicUndefined HeuristicStatus = iota
	// HeuristicTrue explicitly set to true
	HeuristicTrue
	// HeuristicFalse explicitly set to true
	HeuristicFalse
)

func (s HeuristicStatus) String() string {
	return toString[s]
}

var toString = map[HeuristicStatus]string{
	HeuristicUndefined: "HeuristicUndefined",
	HeuristicTrue:      "HeuristicTrue",
	HeuristicFalse:     "HeuristicFalse",
}

var toID = map[string]HeuristicStatus{
	"HeuristicUndefined": HeuristicUndefined,
	"HeuristicTrue":      HeuristicTrue,
	"HeuristicFalse":     HeuristicFalse,
}

// MarshalJSON marshals the enum as a quoted json string
func (s HeuristicStatus) MarshalJSON() ([]byte, error) {
	buffer := bytes.NewBufferString(`"`)
	buffer.WriteString(toString[s])
	buffer.WriteString(`"`)
	return buffer.Bytes(), nil
}

// UnmarshalJSON unmashals a quoted json string to the enum value
func (s *HeuristicStatus) UnmarshalJSON(b []byte) error {
	var j string
	err := json.Unmarshal(b, &j)
	if err != nil {
		return err
	}
	// Note that if the string cannot be found then it will be set to the zero value, 'Created' in this case.
	*s = toID[j]
	return nil
}
