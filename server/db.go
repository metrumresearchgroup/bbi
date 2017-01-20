package main

import (
	"encoding/binary"
	"encoding/json"

	"github.com/boltdb/bolt"
	"github.com/dpastoor/nonmemutils/runner"
)

// Model information about a model to be executed
type Model struct {
	ID      int
	Status  string
	Details ModelDetails
}

// ModelDetails contains the information passed in to execute the model
type ModelDetails struct {
	ModelPath   string
	RunSettings runner.RunSettings
	CacheDir    string
	CacheExe    string
}

// ModelStore contains information for managing models in a boltdb
type ModelStore struct {
	// Filename to the BoltDB database
	Path string
	db   *bolt.DB
}

// ModelService provides an interface for getting models
// type ModelService interface {
// 	GetModels() ([]*Model, error)
// 	GetModel(mID int) (*Model, error)
// 	CreateModel(md Model) error
// }

// GetModels returns all models in the boltdb
func (m *ModelStore) GetModels() ([]Model, error) {
	var models []Model
	m.db.View(func(tx *bolt.Tx) error {
		// models bucket created when db initialized
		b := tx.Bucket([]byte("models"))
		b.ForEach(func(key []byte, value []byte) error {
			var model Model
			json.Unmarshal(value, &model)
			models = append(models, model)
			return nil
		})
		return nil
	})
	return models, nil
}

// GetModel returns details about a specific Model
func (m *ModelStore) GetModel(modelID int) (*Model, error) {
	var model *Model
	m.db.View(func(tx *bolt.Tx) error {
		// models bucket created when db initialized
		b := tx.Bucket([]byte("models"))
		json.Unmarshal(b.Get(itob(modelID)), &model)

		return nil
	})
	return model, nil
}

// CreateModel adds a model to the boltdb and updates the modelID to reflect the stored key in boltdb
func (m *ModelStore) CreateModel(model *Model) error {
	err := m.db.Update(func(tx *bolt.Tx) error {
		// models bucket created when db initialized
		b := tx.Bucket([]byte("models"))
		id, _ := b.NextSequence()
		model.ID = int(id)

		buf, err := json.Marshal(model)
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
