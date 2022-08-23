.PHONY: clean all

LATEX := docker run --platform=linux/amd64 --rm -v $(PWD):/workdir:z ivanpondal/thesis-latex pdflatex
BIBTEX := docker run --platform=linux/amd64 --rm -v $(PWD):/workdir:z ivanpondal/thesis-latex bibtex
LATEXFLAGS := -interaction=nonstopmode

all: tesis.pdf

clean:
	rm -rf ./*.aux ./*.log ./*.tex~ ./*.toc ./*.tex.backup ./*.snm ./*.vrb \
	./*.nav ./*.out ./*.fdb_latexmk ./*.maf ./*.mtc ./*.mtc0 */.fls \
	*/.synctex.gz

thesis-latex-docker: Dockerfile
	docker build --platform=linux/amd64 . -t ivanpondal/thesis-latex

%.pdf: %.tex thesis-latex-docker
	$(LATEX) $(LATEXFLAGS) tesis
	$(BIBTEX) tesis
	$(LATEX) $(LATEXFLAGS) tesis
	$(LATEX) $(LATEXFLAGS) tesis
