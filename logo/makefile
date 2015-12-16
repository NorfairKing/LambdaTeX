all: logo.png

logo.svg: logo.tex
	latexmk -pdf logo.tex -pdflatex='pdflatex -shell-escape -halt-on-error'
	latexmk -c logo.tex

logo.png: logo.svg
	inkscape  -w 600 logo.svg -e logo.png

