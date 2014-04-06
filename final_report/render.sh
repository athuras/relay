pdflatex $1
pdflatex $1
bibtex $1
pdflatex $1

rm *.toc *.aux *.tic *.synctex.gz *.lof *.glo
