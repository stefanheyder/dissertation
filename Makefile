SHELL := /bin/bash

SOURCES := thesis.tex preamble.tex commands.tex acronyms.tex 

# extra memory for figures
TEX_MEM := extra_mem_top=2000000 extra_mem_bot=8000000

.PHONY: all print open data figures r-kernel commit_yesterday clean thesis.pdf thesis_print.pdf

all: thesis.pdf thesis_print.pdf

print: thesis_print.pdf

thesis.pdf: $(SOURCES)
	$(TEX_MEM) latexmk -pdf thesis.tex

thesis_print.pdf: thesis_print.tex $(SOURCES)
	$(TEX_MEM) latexmk -pdf thesis_print.tex

open:
	zathura -x "code-insiders --no-sandbox -r -g %{input}:%{line}" thesis.pdf

data:
	cd code && quarto render data_commuting.qmd --to pdf --output $$(mktemp --suffix ".pdf")
	cd code && quarto render data_rki.qmd --to pdf --output $$(mktemp --suffix ".pdf")

r-kernel:
	PATH="$(CURDIR)/.venv/bin:$$PATH" Rscript -e 'IRkernel::installspec(name = "ir", displayname = "R")'

figures: r-kernel
	@find nbs -iname '*figures*.ipynb' -not -path '*/.ipynb_checkpoints/*' -print0 | \
	while IFS= read -r -d '' nb; do \
		echo "==> executing $$nb"; \
		uv run jupyter nbconvert --to notebook --execute --inplace \
			--ExecutePreprocessor.kernel_name=ir \
			--ExecutePreprocessor.timeout=-1 "$$nb" || exit 1; \
	done
	@echo "figures: done"

commit_yesterday:
	git commit --amend --date="yesterday 8PM" --no-edit

clean:
	rm -f thesis.{acn,aux,auxlock,bbl,bcf,fdb_latexmk,fls,glo,ist,log,out,run.xml,synctex.gz,toc,bbl-SAVE-ERROR,bcf-SAVE-ERROR}
	rm -f thesis_print.{acn,aux,auxlock,bbl,bcf,fdb_latexmk,fls,glo,ist,log,out,run.xml,synctex.gz,toc,bbl-SAVE-ERROR,bcf-SAVE-ERROR}
	rm -rf $$(biber --cache)
	find . -type f -name "*.aux" -exec rm -f {} +
