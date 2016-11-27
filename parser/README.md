README
=================================

Run test coverage with

```
go test -coverprofile=nonmemutils.coverage
```

For  a test coverage report

```
go tool cover -html=nonmemutils.coverage
```


```go
 func main() {
 	data, _ := readLines("parser/fixtures/lstfiles/simple-onecmpt-ex1.lst")
 	results := parser.ParseLstEstimationFile(data)
 	bs, _ := json.MarshalIndent(results, "", "\t")
 	fmt.Println(string(bs))
 	results.Summary()
 }
 ```