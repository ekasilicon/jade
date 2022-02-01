design.pdf: design.tex
	pdflatex -shell-escape design.tex
	bibtex design
	pdflatex -shell-escape design.tex
	pdflatex -shell-escape design.tex

%.tex: %.scrbl
	racket $< > $@ || (rm $@ && false)

clean:
	-rm *.tex
	-rm *.pdf
	-rm *.out
	-rm *.log
	-rm *.aux
