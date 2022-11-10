.PHONY: clean all

LATEX := docker run --rm -v $(PWD):/workdir:z ivanpondal/paper-latex pdflatex
BIBTEX := docker run --rm -v $(PWD):/workdir:z ivanpondal/paper-latex bibtex
LATEXFLAGS := -interaction=errorstopmode

all: paper.pdf

clean:
	rm -rf ./*.aux ./*.log ./*.tex~ ./*.toc ./*.tex.backup ./*.snm ./*.vrb \
	./*.nav ./*.out ./*.fdb_latexmk ./*.maf ./*.mtc ./*.mtc0 */.fls \
	*/.synctex.gz

thesis-latex-docker: Dockerfile
	docker build -t ivanpondal/paper-latex:latest .

%.pdf: %.tex thesis-latex-docker
	$(LATEX) $(LATEXFLAGS) paper
