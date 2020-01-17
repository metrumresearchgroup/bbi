package cmd

import (
	"github.com/google/uuid"
	"os"
	"path"
	"testing"
)

func Test_doesDirectoryContainOutputFiles(t *testing.T) {

	good := uuid.New().String()
	bad := uuid.New().String()

	os.Mkdir(good, 0755)
	os.Mkdir(bad, 0755)
	emptyFile(path.Join(bad, "meow.cov"))

	type args struct {
		path      string
		modelname string
	}
	tests := []struct {
		name string
		args args
		want bool
	}{
		{
			name: "Empty Directory",
			args: args{
				path:      good,
				modelname: "meow",
			},
			want: false,
		},
		{
			name: "Contains covariate file",
			args: args{
				path:      bad,
				modelname: "meow",
			},
			want: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := doesDirectoryContainOutputFiles(tt.args.path, tt.args.modelname); got != tt.want {
				t.Errorf("doesDirectoryContainOutputFiles() = %v, want %v", got, tt.want)
			}
		})
	}

	//Cleanup the directories
	os.RemoveAll(good)
	os.RemoveAll(bad)
}

func emptyFile(path string) {
	empty, _ := os.Create(path)
	empty.Close()
}
