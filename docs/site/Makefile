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
	rm -rf public resources static/latest.json
	$(MAKE) static/latest.json

static/latest.json:
	v=$$(git for-each-ref --sort='-v:refname' --format='%(refname:short)' 'refs/tags/v*' | \
	     grep -E '^v[0-9]+\.[0-9]+\.[0-9]+$$' | \
	     head -n1); \
	  test -n "$$v" && printf '{"version": "%s"}\n' "$$v" >'$@'

command_dir = content/docs/commands

.PHONY: commands
commands:
	rm -rf '$(command_dir)'
	./scripts/ingest-command-docs '$(command_dir)'

content/_index.md: ../../README.md
	grep -vF 'https://goreportcard.com/badge/' '$<' >'$@'

content/news.md: ./scripts/add-gh-links
content/news.md: ../../NEWS.md
	printf -- '---\ntitle: "News"\n---\n\n' >'$@'
	sed 's/^#/##/' '$<' | \
	  sed 's/^## bbi/##/' | \
	  sed 's/^## babylon/##/' | \
	  ./scripts/add-gh-links https://github.com/metrumresearchgroup/bbi \
	  >>'$@'

.PHONY: clean
clean:
	rm -rf public resources $(command_dir)
	rm -f $(copied_files)
