-include local.mk

HTTP_PORT ?= 1313

version := $(shell git describe --tags --always HEAD)
export HUGO_PARAMS_BBIVERSION ?= $(version)

.PHONY: help
help:
	$(info Primary targets:)
	$(info * build: build site under public/)
	$(info * serve: build and serve the site locally with 'hugo server')
	@:

.PHONY: build
build: prep
	hugo --gc --minify

.PHONY: serve
serve: prep
	hugo server -p '$(HTTP_PORT)'

copied_files  = content/_index.md
copied_files += content/news.md

.PHONY: prep
prep: commands
prep: $(copied_files)
	rm -rf public resources

command_dir = content/docs/commands

.PHONY: commands
commands:
	rm -rf '$(command_dir)'
	./scripts/ingest-command-docs '$(command_dir)'

content/_index.md: ../../README.md
	grep -vF 'https://goreportcard.com/badge/' '$<' >'$@'

content/news.md: ../../NEWS.md
	printf -- '---\ntitle: "News"\n---\n\n' >'$@'
	sed 's/^#/##/' '$<' | \
	  sed 's/^## bbi/##/' | \
	  sed 's/^## babylon/##/' | \
	  >>'$@'

.PHONY: clean
clean:
	rm -rf public resources $(command_dir)
	rm -f $(copied_files)
