package httpserver

import (
	"context"
	"net/http"
	"strconv"

	"github.com/dpastoor/nonmemutils/server"
	"github.com/pressly/chi"
	"github.com/pressly/chi/render"
)

// ModelHandler represents the HTTP API handler for ModelService
type ModelHandler struct {
	ModelService server.ModelService
}

// NewModelHandler provides a pointer to a new httpClient
func NewModelHandler() *ModelHandler {
	c := &ModelHandler{}
	return c
}

// HandleGetAllModels provides all models
func (c *ModelHandler) HandleGetAllModels(w http.ResponseWriter, r *http.Request) {
	models, _ := c.ModelService.GetModels()
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
		ctx := context.WithValue(r.Context(), "model", model)
		next.ServeHTTP(w, r.WithContext(ctx))
	})
}
