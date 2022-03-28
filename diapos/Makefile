.PHONY: clean all

LATEX := docker run --rm -v $(PWD):/workdir:z ivanpondal/thesis-slides-latex pdflatex
LATEXFLAGS := -interaction=nonstopmode

all: tesis-diapos.pdf

clean:
	rm -rf ./*.aux ./*.log ./*.tex~ ./*.toc ./*.tex.backup ./*.snm ./*.vrb \
	./*.nav ./*.out ./*.fdb_latexmk ./*.maf ./*.mtc ./*.mtc0 */.fls \
	*/.synctex.gz

thesis-slides-latex-docker: Dockerfile
	docker build . -t ivanpondal/thesis-slides-latex

%.pdf: %.tex thesis-slides-latex-docker
	$(LATEX) $(LATEXFLAGS) tesis-diapos
	$(LATEX) $(LATEXFLAGS) tesis-diapos
