#!/bin/bash

pandoc --wrap=preserve --standalone --citeproc --bibliography=references.bib --csl=ieee.csl make_references.md -o references.html

pandoc --wrap=preserve --standalone --citeproc --bibliography=references.bib --csl=ieee.csl --to=markdown-citations make_references.md -o references.md

