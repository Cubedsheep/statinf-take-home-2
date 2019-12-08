# default main tex file
@default_files = ('take-home-2.tex');

# compile directly to a pdf
$pdf_mode = 1;        # tex -> pdf

# enable shell-escape to save graphics externally
$latex = 'latex -interaction=nonstopmode -shell-escape';
$pdflatex = 'pdflatex -interaction=nonstopmode -shell-escape';

# remove the crap latex generates after compiling the document
# when using latexmk -c
$clean_ext = "bbl nav out snm aux auxlock blg fdb_latexmk fls";
