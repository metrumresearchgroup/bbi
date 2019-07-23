package db

import (
	"time"

	"os"

	"github.com/boltdb/bolt"
	"github.com/dpastoor/babylon/server"
)

// Client represents a client to the underlying BoltDB instance
type Client struct {
	// Filepath to the BoltDB database
	Path string

	// Services
	modelService ModelService

	db *bolt.DB
}

// ModelService provides an interface for getting models
// type ModelService interface {
// 	GetModels() ([]*Model, error)
// 	GetModel(mID int) (*Model, error)
// 	CreateModel(md Model) error
// }

// NewClient creates a new client bound to the modelService
func NewClient(path string) *Client {
	c := &Client{Path: path}
	c.modelService.client = c
	return c
}

// Open opens and initializes the BoltDB database
func (c *Client) Open() error {
	// Open database file.
	db, err := bolt.Open(c.Path, 0666, &bolt.Options{Timeout: 1 * time.Second})
	if err != nil {
		return err
	}
	c.db = db

	// Initialize top-level buckets.
	tx, err := c.db.Begin(true)
	if err != nil {
		return err
	}
	defer tx.Rollback()

	if _, err := tx.CreateBucketIfNotExists([]byte("models")); err != nil {
		return err
	}

	return tx.Commit()
}

// Close closes then underlying BoltDB database.
func (c *Client) Close() error {
	if c.db != nil {
		return c.db.Close()
	}
	return nil
}

// ResetDb will delete and reinitialize the DB
func (c *Client) ResetDb() error {
	// if file exists delete
	if _, err := os.Stat(c.Path); err == nil {
		err = os.Remove(c.Path)
		if err != nil {
			return err
		}
	}
	c.Open()
	return nil
}

//ModelService returns the modelService associated with the client
func (c *Client) ModelService() server.ModelService {
	return &c.modelService
}
