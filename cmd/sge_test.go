package cmd

import (
	"github.com/metrumresearchgroup/bbi/configlib"
	"testing"
)

func Test_gridengineJobName(t *testing.T) {
	type args struct {
		model *NonMemModel
	}
	tests := []struct {
		name    string
		args    args
		want    string
		wantErr bool
	}{
		{
			name: "No prefix present",
			args: args{
				model: &NonMemModel{
					FileName: "meow",
				},
			},
			want:    "Run_meow",
			wantErr: false,
		},
		{
			name: "Prefix Present",
			args: args{
				model: &NonMemModel{
					FileName: "meow",
					Configuration: configlib.Config{
						GridNamePrefix: "test",
					},
				},
			},
			want:    "test_Run_meow",
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := gridengineJobName(tt.args.model)
			if (err != nil) != tt.wantErr {
				t.Errorf("gridengineJobName() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if got != tt.want {
				t.Errorf("gridengineJobName() got = %v, want %v", got, tt.want)
			}
		})
	}
}
