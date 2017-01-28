package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/dpastoor/nonmemutils/runner"
	"github.com/dpastoor/nonmemutils/server"
	"github.com/dpastoor/nonmemutils/server/db"
	"github.com/dpastoor/nonmemutils/server/httpserver"
	"github.com/pressly/chi"
	"github.com/pressly/chi/middleware"
)

func main() {

	// Open the my.db data file in your current directory.
	// It will be created if it doesn't exist.
	client := db.NewClient()
	client.Path = "models.db"

	_, existsErr := os.Stat(client.Path)

	err := client.Open() // connect to the boltDB instance
	if err != nil {
		log.Fatalf("could not open boltdb instance")
	}

	ms := client.ModelService()
	httpClient := httpserver.NewModelHandler()
	httpClient.ModelService = ms

	// create a model bucket to store models
	if os.IsNotExist(existsErr) {
		fmt.Println("no database was detected on initializiation, populating a sample one now...")
		populateDB(ms)
	}

	r := chi.NewRouter()

	// A good base middleware stack
	r.Use(middleware.RequestID)
	r.Use(middleware.RealIP)
	r.Use(middleware.Logger)
	r.Use(middleware.Recoverer)

	// When a client closes their connection midway through a request, the
	// http.CloseNotifier will cancel the request context (ctx).
	r.Use(middleware.CloseNotify)

	// Set a timeout value on the request context (ctx), that will signal
	// through ctx.Done() that the request has timed out and further
	// processing should be stopped.
	r.Use(middleware.Timeout(60 * time.Second))

	r.Get("/", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("hi"))
	})

	r.Route("/models", func(r chi.Router) {
		r.Get("/", httpClient.HandleGetAllModels)
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

func populateDB(ms server.ModelService) error {
	var newModels []server.Model
	sampleDuration := time.Now().AddDate(0, 0, -1).Add(10*time.Minute).Unix() - time.Now().AddDate(0, 0, -1).Unix()
	startInsert := time.Now()
	for i := 0; i < 4000; i++ {
		newModel := server.Model{
			ID:     0,
			Status: "COMPLETED",
			ModelInfo: server.ModelInfo{
				ModelPath:   "C://temphello",
				RunSettings: runner.RunSettings{Git: true, SaveExe: "cache.exe"},
				CacheDir:    "cache_dir",
				CacheExe:    "cache.exe",
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
				ModelPath:   "C://temphello",
				RunSettings: runner.RunSettings{Git: true, SaveExe: "cache.exe"},
				CacheDir:    "cache_dir",
				CacheExe:    "cache.exe",
			},
			RunInfo: server.RunInfo{
				QueueTime: time.Now().AddDate(0, 0, -1).Unix(),
				StartTime: time.Now().Unix(),
				Duration:  int64(0),
			},
		}
		newModels = append(newModels, newModel)
	}
	for i := 0; i < 10; i++ {
		newModel := server.Model{
			ID:     0,
			Status: "QUEUED",
			ModelInfo: server.ModelInfo{
				ModelPath:   "C://temphello",
				RunSettings: runner.RunSettings{Git: true, SaveExe: "cache.exe"},
				CacheDir:    "cache_dir",
				CacheExe:    "cache.exe",
			},
			RunInfo: server.RunInfo{
				QueueTime: time.Now().AddDate(0, 0, -1).Unix(),
				StartTime: int64(0),
				Duration:  int64(0),
			},
		}
		newModels = append(newModels, newModel)
	}

	err := ms.CreateModels(newModels)
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
