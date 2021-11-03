.PHONY: clean all thesis-latex-docker

LATEX := docker run --rm -v $(PWD):/workdir:z ivanpondal/thesis-latex pdflatex
LATEXFLAGS := -interaction=nonstopmode

all: main.pdf

clean:
	rm -rf ./*.aux ./*.log ./*.tex~ ./*.toc ./*.pdf ./*.tex.backup ./*.snm \
	./*.vrb ./*.nav ./*.out ./*.fdb_latexmk ./*.maf ./*.mtc ./*.mtc0 */.fls \
	*/.synctex.gz

thesis-latex-docker: Dockerfile
	docker build . -t ivanpondal/thesis-latex

%.pdf: %.tex thesis-latex-docker
	$(LATEX) $(LATEXFLAGS) $<
	$(LATEX) $(LATEXFLAGS) $<
