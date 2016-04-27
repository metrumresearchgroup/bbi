package parser

import (
	"testing"
)

var parameterDiagonal01Results = []int{1}
var parameterDiagonal02Results = []int{1, 0, 1}
var parameterDiagonal03Results = []int{1, 0, 1, 0, 0, 1}

func TestcreateDiagonalBlock(t *testing.T) {
	result1 := createDiagonalBlock(1)
	result2 := createDiagonalBlock(2)
	result3 := createDiagonalBlock(3)
	for i, val := range result1 {
		if val != parameterDiagonal01Results[i] {
			t.Log("GOT: ", val, " EXPECTED: ", parameterDiagonal01Results[i], " at index: ", i)
			t.Fail()
		}
	}
	for i, val := range result2 {
		if val != parameterDiagonal02Results[i] {
			t.Log("GOT: ", val, " EXPECTED: ", parameterDiagonal02Results[i], " at index: ", i)
			t.Fail()
		}
	}
	for i, val := range result3 {
		if val != parameterDiagonal03Results[i] {
			t.Log("GOT: ", val, " EXPECTED: ", parameterDiagonal03Results[i], " at index: ", i)
			t.Fail()
		}
	}
}
