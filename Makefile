SHELL := /bin/bash

SOURCES := thesis.tex preamble.tex commands.tex acronyms.tex

.PHONY: all print open data commit_yesterday clean

all: thesis.pdf thesis_print.pdf

print: thesis_print.pdf

thesis.pdf: $(SOURCES)
	latexmk -pdf thesis.tex

thesis_print.pdf: thesis_print.tex $(SOURCES)
	latexmk -pdf thesis_print.tex

open:
	zathura -x "code-insiders --no-sandbox -r -g %{input}:%{line}" thesis.pdf

data:
	cd code && quarto render data_commuting.qmd --to pdf --output $$(mktemp --suffix ".pdf")
	cd code && quarto render data_rki.qmd --to pdf --output $$(mktemp --suffix ".pdf")

commit_yesterday:
	git commit --amend --date="yesterday 8PM" --no-edit

clean:
	rm -f thesis.{acn,aux,auxlock,bbl,bcf,fdb_latexmk,fls,glo,ist,log,out,run.xml,synctex.gz,toc,bbl-SAVE-ERROR,bcf-SAVE-ERROR}
	rm -f thesis_print.{acn,aux,auxlock,bbl,bcf,fdb_latexmk,fls,glo,ist,log,out,run.xml,synctex.gz,toc,bbl-SAVE-ERROR,bcf-SAVE-ERROR}
	rm -rf $$(biber --cache)
	find . -type f -name "*.aux" -exec rm -f {} +
