package main

import (
	"context"
	"fmt"
	"net/http"
	"time"

	"github.com/dpastoor/nonmemutils/runner"
	"github.com/pressly/chi"
	"github.com/pressly/chi/middleware"
)

func main() {
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

	// RESTy routes for "articles" resource
	r.Route("/models", func(r chi.Router) {
		// r.With(paginate).Get("/", listArticles) // GET /articles
		// r.Post("/", createArticle)              // POST /articles
		// r.Get("/search", searchArticles)        // GET /articles/search

		r.Route("/:modelID", func(r chi.Router) {
			r.Use(ModelCtx)
			r.Get("/", getModelStatus) // GET /models/123
			r.Post("/", submitModel)
		})
	})
	fmt.Println("serving now on 3333")
	http.ListenAndServe(":3333", r)

}

// ModelCtx is the context
func ModelCtx(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		modelID := chi.URLParam(r, "modelID")
		model, err := dbGetModel(modelID)
		if err != nil {
			http.Error(w, http.StatusText(404), 404)
			return
		}
		ctx := context.WithValue(r.Context(), "model", model)
		next.ServeHTTP(w, r.WithContext(ctx))
	})
}

func getModelStatus(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()
	model, ok := ctx.Value("model").(*Model)
	if !ok {
		http.Error(w, http.StatusText(422), 422)
		return
	}
	w.Write([]byte(fmt.Sprintf("modelID:%v", model.ModelID)))
}

func submitModel(w http.ResponseWriter, r *http.Request) {
	model := new(Model)
	fmt.Println("new model submitted to queue")
	w.Write([]byte(fmt.Sprintf("modelID:%v", model.ModelID)))
}

// Model information about a model to be executed
type Model struct {
	ModelID     int64
	ModelPath   string
	RunSettings runner.RunSettings
	CacheDir    string
	CacheExe    string
}

func dbGetModel(aid string) (*Model, error) {
	return new(Model), nil
}
