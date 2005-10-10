TEX = latex
BIB = bibtex

all: vignette.pdf 
	cp vignette.pdf MANOR.pdf
	rm -f vignette*

%.pdf: %.dvi
	dvipdfm $<

html: index.dvi
	latex2html -split +1 -show_section_numbers -local_icons -no_auto_link -no_math -reuse 0 index.tex
	cp MANOR.css index/index.css
	cp *.html *.png index/
	cp ../../.data/edge.txt index/
	cp ../../.data/gradient.gpr index/

%.dvi: %.tex
	$(TEX) $<
	$(BIB) $*
	$(TEX) $<
	$(TEX) $<

%.tex: %.Rnw
	echo "require(MANOR); Sweave(\"$<\")" | R CMD BATCH

vignette.tex: MANOR.tex
	sed -e 's/\\newcommand{\\htmldoc}{}/\\newcommand{\\htmldoc}\[1\]{}/g' -e '/^\\begin{figure}/d' -e '/^\\end{figure}/d' -e '/^\\caption/d' < MANOR.tex > vignette.tex

MANOR-eval.Rnw: MANOR.Rnw
	sed -e 's/Sinput/verbatim/g' -e 's/Soutput/verbatim/g' -e 's/\\SweaveOpts{echo=FALSE, width=10, height=6, eval=FALSE}/\\SweaveOpts{echo=FALSE, width=10, height=6, eval=TRUE}/g' < MANOR.Rnw > MANOR-eval.Rnw

index.tex: MANOR-eval.tex
#	sed -e 's/\\newcommand{\\pdfdoc}{}/\\newcommand{\\pdfdoc}\[1\]{}/g' -e 's/{Schunk}/{verbatim}/g' -e '/{Sinput}/d' -e '/{Soutput}/d' -e '/usepackage{Sweave}/d' -e '/usepackage{graphicx}/d' -e '/usepackage{hyperref}/d' < MANOR-eval.tex > index.tex
	sed -e 's/\\newcommand{\\pdfdoc}{}/\\newcommand{\\pdfdoc}\[1\]{}/g' -e 's/{Schunk}/{verbatim}/g' -e '/{Sinput}/d' -e '/{Soutput}/d' < MANOR-eval.tex > index.tex

clean:
	rm -f vignette*
	rm -rf index/
	rm -f index.dvi index.pdf index.aux index.log index.bbl index.blg index.toc index-*pdf index-*.eps
	rm -f MANOR.dvi MANOR.pdf MANOR.aux MANOR.log MANOR.bbl MANOR.blg MANOR.toc MANOR-*pdf MANOR-*.eps
	rm -f MANOR-eval*
	rm -f Rplots.ps

Rnw-eval:
	sed -e 's/Sinput/verbatim/g' -e 's/Soutput/verbatim/g' -e 's/\\SweaveOpts{echo=FALSE, width=10, height=6, eval=FALSE}/\\SweaveOpts{echo=FALSE, width=10, height=6, eval=TRUE}/g' < MANOR.Rnw > index.Rnw
