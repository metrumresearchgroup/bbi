package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"time"

	flag "github.com/spf13/pflag"

	"github.com/dpastoor/babylon/runner"
	"github.com/dpastoor/babylon/server"
	"github.com/dpastoor/babylon/server/db"
	"github.com/dpastoor/babylon/server/httpserver"
	"github.com/pressly/chi"
	"github.com/pressly/chi/middleware"
	"github.com/spf13/afero"
)

var (
	port     int
	database string
	reset    bool
	workers  int
)

func init() {
	flag.IntVarP(&port, "port", "p", 3333, "port number to serve")
	flag.BoolVarP(&reset, "reset", "r", false, "wipe and reset the database")
	flag.StringVarP(&database, "database", "d", "models.db", "path and name of database to store model results")
	flag.IntVarP(&workers, "workers", "w", 0, "number of workers, set to negative number for no workers to be activated on initialization")
}

func main() {

	if reset {
		_, existsErr := os.Stat(database)

		if os.IsNotExist(existsErr) {
			fmt.Println("no prior database occurence detected")
		} else {
			err := os.Remove(database)
			if err != nil {
				log.Fatalf("error removing previous database instance: %s", err)
			}
			err = os.Remove(fmt.Sprintf("%s%s", database, ".lock"))
			if err != nil {
				log.Fatalf("error removing previous database lockfile instance: %s", err)
			}

		}
	}
	// Open the my.db data file in your current directory.
	// It will be created if it doesn't exist.
	client := db.NewClient()
	client.Path = database

	err := client.Open() // connect to the boltDB instance
	if err != nil {
		log.Fatalf("could not open boltdb instance")
	}

	// extract the model service and initalize http handlers
	ms := client.ModelService()
	httpClient := httpserver.NewModelHandler()

	// provide the model service to the httpClient so has access to the boltdb
	httpClient.ModelService = ms

	// launch the worker(s)
	aferofs := afero.NewOsFs()

	numCPU := runtime.NumCPU()
	numWorkers := workers

	if numCPU == 1 {
		// don't want to block the http manager with the worker routine
		if numWorkers == 0 {
			// default
			runtime.GOMAXPROCS(2)
			numWorkers = 1
		} else {
			runtime.GOMAXPROCS(max(numWorkers, 1) + 1) // hopefully people don't abuse this
		}
	} else {
		// multithread processor
		if numWorkers == 0 {
			// two threads for the server/boltdb instance
			// given a hyperthreaded computer, this should be fine
			numWorkers = max(numCPU-2, 1)
			fmt.Println("default number of workers set to: ", numWorkers)
		} else {
			// make sure they don't oversaturate with workers
			if numWorkers >= numCPU {
				fmt.Println("Your worker count is high, be aware there may be performance implications")
				fmt.Println("Suggest setting your worker count to at, or below: ", numCPU-1)
				runtime.GOMAXPROCS(numWorkers + 1)
			}

		}
	}

	// for hyperthreaded machines -2 should still basically saturate the CPU completely, while still leaving
	// two threads goroutines to deal with requests and db management
	// eg 4 core machine will show up as having 8 cpus, therefore 6 workers
	// running 6 models on a 4 core machine will definitely saturate the CPU well.
	for i := 0; i < numWorkers; i++ {
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

	fmt.Println(fmt.Sprintf("serving now on %v", port))
	http.ListenAndServe(fmt.Sprintf(":%v", port), r)

}

func min(x, y int) int {
	if x < y {
		return x
	}
	return y
}
func max(x, y int) int {
	if x > y {
		return x
	}
	return y
}

//LaunchWorker polls the DB and acquires queue'd models to run via EstimateModel
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

// populateDB populates the database with some fake objects with various statuses
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
