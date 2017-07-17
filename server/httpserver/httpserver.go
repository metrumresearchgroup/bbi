package httpserver

import (
	"context"
	"fmt"
	"net/http"
	"strconv"

	"github.com/dpastoor/babylon/server"
	"github.com/go-chi/chi"
	"github.com/go-chi/render"
)

// ModelHandler represents the HTTP API handler for ModelService
type ModelHandler struct {
	ModelService server.ModelService
}

// NewModelHandler provides a pointer to a new httpClient
func NewModelHandler(ms server.ModelService) *ModelHandler {
	return &ModelHandler{
		ModelService: ms,
	}
}

// HandleGetModelsByStatus provides all models
// accepts query param status with values COMPLETED, QUEUED, RUNNING
func (c *ModelHandler) HandleGetModelsByStatus(w http.ResponseWriter, r *http.Request) {
	var models []server.Model
	status := r.URL.Query().Get("status")
	fmt.Println("status: ", status)
	if status != "" {
		models, _ = c.ModelService.GetModelsByStatus(status)
	} else {
		models, _ = c.ModelService.GetModels()
	}

	render.JSON(w, r, models)
}

// HandleGetModelByID handles providing model by ID taken from context
func (c *ModelHandler) HandleGetModelByID(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()
	model, ok := ctx.Value("model").(*server.Model)

	if !ok {
		http.Error(w, http.StatusText(422), 422)
		return
	}
	render.JSON(w, r, model)
}

// ModelCtx is the context
func (c *ModelHandler) ModelCtx(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		modelID := chi.URLParam(r, "modelID")
		mid, _ := strconv.ParseInt(modelID, 10, 64)
		model, err := c.ModelService.GetModelByID(int(mid))
		if err != nil {
			http.Error(w, http.StatusText(404), 404)
			return
		}
		ctx := context.WithValue(r.Context(), "model", &model)
		next.ServeHTTP(w, r.WithContext(ctx))
	})
}

// HandleSubmitModels adds models to the database for workers to execute
func (c *ModelHandler) HandleSubmitModels(w http.ResponseWriter, r *http.Request) {
	var models []server.Model
	if err := render.DecodeJSON(r.Body, &models); err != nil {
		render.JSON(w, r, err.Error())
		return
	}
	models, err := c.ModelService.CreateModels(models)
	if err != nil {
		fmt.Printf("Insertion of models failed with err: %v", err)
	}
	render.JSON(w, r, models)
}
