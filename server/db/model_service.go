package db

import (
	"encoding/binary"

	"github.com/coreos/bbolt"
	"github.com/metrumresearchgroup/bbi/server"
	"github.com/metrumresearchgroup/bbi/server/db/internal"
)

// make sure ModelService implements server.ModelService
var _ server.ModelService = &ModelService{}

// ModelService represents a service for managing models
type ModelService struct {
	client *Client
}

// GetModels returns all models in the boltdb
func (m *ModelService) GetModels() ([]server.Model, error) {
	var models []server.Model
	m.client.db.View(func(tx *bolt.Tx) error {
		// models bucket created when db initialized
		b := tx.Bucket([]byte("models"))
		b.ForEach(func(key []byte, value []byte) error {
			var model server.Model
			internal.UnmarshalModel(value, &model)
			models = append(models, model)
			return nil
		})
		return nil
	})
	return models, nil
}

// GetModelsByStatus returns all models in the boltdb
func (m *ModelService) GetModelsByStatus(status string) ([]server.Model, error) {
	var models []server.Model
	m.client.db.View(func(tx *bolt.Tx) error {
		// models bucket created when db initialized
		b := tx.Bucket([]byte("models"))
		b.ForEach(func(key []byte, value []byte) error {
			var model server.Model
			internal.UnmarshalModel(value, &model)
			if model.Status == status {
				models = append(models, model)
			}
			return nil
		})
		return nil
	})
	return models, nil
}

// GetModelByID returns details about a specific Model
func (m *ModelService) GetModelByID(modelID int) (server.Model, error) {
	var model server.Model
	m.client.db.View(func(tx *bolt.Tx) error {
		// models bucket created when db initialized
		b := tx.Bucket([]byte("models"))
		internal.UnmarshalModel(b.Get(itob(modelID)), &model)

		return nil
	})
	return model, nil
}

// CreateModel adds a model to the boltdb and updates the modelID to reflect the stored key in boltdb
func (m *ModelService) CreateModel(model *server.Model) error {
	err := m.client.db.Update(func(tx *bolt.Tx) error {
		// models bucket created when db initialized
		b := tx.Bucket([]byte("models"))
		id, _ := b.NextSequence()
		model.ID = int(id)

		buf, err := internal.MarshalModel(model)
		if err != nil {
			return err
		}
		return b.Put(itob(model.ID), buf)
	})
	return err
}

// CreateModels adds an array of models to the boltdb in a single batch transaction
func (m *ModelService) CreateModels(models []server.Model) ([]server.Model, error) {
	err := m.client.db.Batch(func(tx *bolt.Tx) error {
		// models bucket created when db initialized
		b := tx.Bucket([]byte("models"))
		n := 0
		for i, model := range models {
			id, err := b.NextSequence()
			if err != nil {
				return err
			}
			model.ID = int(id)
			buf, err := internal.MarshalModel(&model)
			if err != nil {
				return err
			}
			err = b.Put(itob(model.ID), buf)
			if err != nil {
				return err
			}
			// update the model stored in the slice so contains the new ID when returned
			models[i] = model
			n++
		}
		return nil
	})
	return models, err
}

// AcquireNextQueuedModel returns the next model with status QUEUED while also changing the value to RUNNING
func (m *ModelService) AcquireNextQueuedModel() (server.Model, error) {
	var nextModel server.Model
	err := m.client.db.Update(func(tx *bolt.Tx) error {
		// models bucket created when db initialized
		b := tx.Bucket([]byte("models"))
		c := b.Cursor()
		for key, value := c.First(); key != nil; key, value = c.Next() {
			var model server.Model
			internal.UnmarshalModel(value, &model)
			if model.Status == "QUEUED" {
				model.Status = "RUNNING"
				nextModel = model
				break
			}
		}
		if nextModel.ID == 0 {
			// no queued models found
			return nil
		}
		buf, err := internal.MarshalModel(&nextModel)
		if err != nil {
			return err
		}
		return b.Put(itob(nextModel.ID), buf)
	})
	return nextModel, err
}

// UpdateModel updates the model status
func (m *ModelService) UpdateModel(model *server.Model) error {
	err := m.client.db.Update(func(tx *bolt.Tx) error {
		// models bucket created when db initialized
		b := tx.Bucket([]byte("models"))
		buf, err := internal.MarshalModel(model)
		if err != nil {
			return err
		}
		return b.Put(itob(model.ID), buf)
	})
	return err
}

// itob returns an 8-byte big endian representation of v.
func itob(v int) []byte {
	b := make([]byte, 8)
	binary.BigEndian.PutUint64(b, uint64(v))
	return b
}
