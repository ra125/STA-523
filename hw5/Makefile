# This Makefile is used to generate the final document
all:	hw5.html

hw5.html:	hw5.Rmd reject_data.Rdata MHtimes.Rdata slice_data_T.Rdata Rtimes.Rdata
	Rscript -e "library(rmarkdown);render('hw5.Rmd')"

reject_data.Rdata:	reject.R 
	Rscript reject.R

MHtimes.Rdata:	mh_final.R 
	Rscript mh_final.R

slice_data_T.Rdata:	slice.R 
	Rscript slice.R

Rtimes.Rdata:	R.R 
	Rscript R.R

.PHONY:	clean
clean:
	rm hw5.html
	rm *.Rdata
