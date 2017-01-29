package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"runtime"
	"time"

	"path/filepath"

	"github.com/dpastoor/nonmemutils/runner"
	"github.com/dpastoor/nonmemutils/server"
	"github.com/dpastoor/nonmemutils/server/db"
	"github.com/dpastoor/nonmemutils/server/httpserver"
	"github.com/pressly/chi"
	"github.com/pressly/chi/middleware"
	"github.com/spf13/afero"
)

func main() {

	// Open the my.db data file in your current directory.
	// It will be created if it doesn't exist.
	client := db.NewClient()
	client.Path = "models.db"
	os.Remove("models.db")
	os.Remove("models.db.lock")
	// _, existsErr := os.Stat(client.Path)

	err := client.Open() // connect to the boltDB instance
	if err != nil {
		log.Fatalf("could not open boltdb instance")
	}

	// extract the model service and initalize http handlers
	ms := client.ModelService()
	httpClient := httpserver.NewModelHandler()

	// provide the model service to the httpClient so has access to the boltdb
	httpClient.ModelService = ms

	// create a model bucket to store models
	// if os.IsNotExist(existsErr) {
	// 	fmt.Println("no database was detected on initializiation, populating a sample one now...")
	// 	populateDB(ms)
	// }

	// launch the worker(s)
	aferofs := afero.NewOsFs()

	numWorkers := runtime.NumCPU()
	if numWorkers == 1 {
		// don't want to block the http manager with the worker routine
		runtime.GOMAXPROCS(2)
	}

	// for hyperthreaded machines -2 should still basically saturate the CPU completely, while still leaving
	// two threads goroutines to deal with requests and db management
	// eg 4 core machine will show up as having 8 cpus, therefore 6 workers
	// running 6 models on a 4 core machine will definitely saturate the CPU well.
	for i := 0; i < numWorkers-2; i++ {
		go launchWorker(aferofs, ms, true, i)
	}

	r := chi.NewRouter()

	// A good base middleware stack
	r.Use(middleware.RequestID)
	r.Use(middleware.RealIP)
	r.Use(middleware.Logger)
	r.Use(middleware.Recoverer)

	// When a client closes their connection midway through a request, the
	// http.CloseNotifier will cancel the request context (ctx).
	// r.Use(middleware.CloseNotify)

	// Set a timeout value on the request context (ctx), that will signal
	// through ctx.Done() that the request has timed out and further
	// processing should be stopped.
	// r.Use(middleware.Timeout(60 * time.Second))

	r.Get("/", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("hi"))
	})

	r.Route("/models", func(r chi.Router) {
		r.Get("/", httpClient.HandleGetAllModels)
		r.Post("/", httpClient.HandleSubmitModels)
		// r.With(paginate).Get("/", listArticles) // GET /articles
		// r.Post("/", createArticle)              // POST /articles
		// r.Get("/search", searchArticles)        // GET /articles/search
		r.Route("/:modelID", func(r chi.Router) {
			r.Use(httpClient.ModelCtx)
			r.Get("/", httpClient.HandleGetModelByID) // GET /models/123
		})
	})

	fmt.Println("serving now on 3333")
	http.ListenAndServe(":3333", r)

}

//runWorker polls the DB and acquires queue'd models to run via EstimateModel
// fs is the file system abstraction that the runner code will use, should use an afero.OsFS
// ms is the model service that interacts with the db/queue
// verbose is whether to log information
// workerNumber is the worker number for use in logs
func launchWorker(
	fs afero.Fs,
	ms server.ModelService,
	verbose bool,
	workerNum int,
) {
	fmt.Printf("launching worker number %v\n", workerNum)
	for {
		model, err := ms.AcquireNextQueuedModel()
		if model.ID == 0 {
			// no queued models
			fmt.Println("no queued models, going to sleep...")
			time.Sleep(5 * time.Second)
			continue
		}
		if err != nil {
			fmt.Println("error acquiring new model, skipping...")
			continue
		}
		filePath := model.ModelInfo.ModelPath
		startTime := time.Now()
		if verbose {
			log.Printf("run %s running on worker %v!", filepath.Base(filePath), workerNum)
		}
		runner.EstimateModel(
			fs,
			filePath,
			model.ModelInfo.RunSettings,
		)
		if verbose {
			log.Printf("completed run %s releasing worker back to queue \n", filePath)
		}
		duration := time.Since(startTime)
		model.Status = "COMPLETED"
		model.RunInfo.StartTime = startTime.Unix()
		model.RunInfo.Duration = int64(duration.Seconds())
		ms.UpdateModel(&model)

		fmt.Println("duration: ", duration)
	}
}

func populateDB(ms server.ModelService) error {
	var newModels []server.Model
	sampleDuration := time.Now().AddDate(0, 0, -1).Add(10*time.Minute).Unix() - time.Now().AddDate(0, 0, -1).Unix()
	startInsert := time.Now()
	for i := 0; i < 10; i++ {
		newModel := server.Model{
			ID:     0,
			Status: "COMPLETED",
			ModelInfo: server.ModelInfo{
				ModelPath: "C://temphello",
				RunSettings: runner.RunSettings{
					Git:                true,
					SaveExe:            "cache.exe",
					Verbose:            false,
					Debug:              false,
					CleanLvl:           1,
					CopyLvl:            1,
					CacheDir:           "cache_dir",
					ExeNameInCache:     "cache.exe",
					NmExecutableOrPath: "nmfe74",
				},
			},
			RunInfo: server.RunInfo{
				QueueTime: time.Now().AddDate(0, 0, -1).Unix(),
				StartTime: time.Now().AddDate(0, 0, -1).Unix(),
				Duration:  sampleDuration,
			},
		}
		newModels = append(newModels, newModel)
	}
	for i := 0; i < 4; i++ {
		newModel := server.Model{
			ID:     0,
			Status: "RUNNING",
			ModelInfo: server.ModelInfo{
				ModelPath: "C://temphello",
				RunSettings: runner.RunSettings{
					Git:                true,
					SaveExe:            "cache.exe",
					Verbose:            false,
					Debug:              false,
					CleanLvl:           1,
					CopyLvl:            1,
					CacheDir:           "cache_dir",
					ExeNameInCache:     "cache.exe",
					NmExecutableOrPath: "nmfe74",
				},
			},
			RunInfo: server.RunInfo{
				QueueTime: time.Now().AddDate(0, 0, -1).Unix(),
				StartTime: time.Now().Unix(),
				Duration:  int64(0),
			},
		}
		newModels = append(newModels, newModel)
	}
	for i := 0; i < 40000; i++ {
		newModel := server.Model{
			ID:     0,
			Status: "QUEUED",
			ModelInfo: server.ModelInfo{
				ModelPath: "C://temphello",
				RunSettings: runner.RunSettings{
					Git:                true,
					SaveExe:            "cache.exe",
					Verbose:            false,
					Debug:              false,
					CleanLvl:           1,
					CopyLvl:            1,
					CacheDir:           "cache_dir",
					ExeNameInCache:     "cache.exe",
					NmExecutableOrPath: "nmfe74",
				},
			},
			RunInfo: server.RunInfo{
				QueueTime: time.Now().AddDate(0, 0, -1).Unix(),
				StartTime: int64(0),
				Duration:  int64(0),
			},
		}
		newModels = append(newModels, newModel)
	}

	_, err := ms.CreateModels(newModels)
	fmt.Println("inserted sample model output in: ", time.Since(startInsert))
	return err
}

// func getModelStatus(w http.ResponseWriter, r *http.Request) {
// 	ctx := r.Context()
// 	model, ok := ctx.Value("model").(*Model)
// 	if !ok {
// 		http.Error(w, http.StatusText(422), 422)
// 		return
// 	}
// 	w.Write([]byte(fmt.Sprintf("modelID:%v", model.ModelID)))
// }

// func submitModel(w http.ResponseWriter, r *http.Request) {
// 	model := new(Model)
// 	fmt.Println("new model submitted to queue")
// 	w.Write([]byte(fmt.Sprintf("modelID:%v", model.ModelID)))
// }

// func dbGetModel(aid string) (*Model, error) {
// 	return new(Model), nil
// }
