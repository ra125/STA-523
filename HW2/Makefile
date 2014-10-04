# This Makefile is used to generate the final document

hw2.html: hw2.Rmd lq/lq_data.Rdata dennys/dennys_data.Rdata
	Rscript -e "library(rmarkdown);render('hw2.Rmd')"


# La Quinta files

lq/lq_data.Rdata: lq_hotel_parse.R lq/hotels/1.html
	Rscript lq_hotel_parse.R

lq/hotels/%.html: lq_hotel_download.R lq/list.Rdata
	Rscript lq_hotel_download.R

lq/list.Rdata: lq_list_parse.R lq/list.html
	Rscript lq_list_parse.R

lq/list.html: lq_list_download.R
	Rscript lq_list_download.R


# Denny's files

dennys/dennys_data.Rdata: dennys_parse.R dennys/dc.html dennys/slc.html
	Rscript dennys_parse.R

dennys/dc.html dennys/slc.html: dennys_download.R
	Rscript dennys_download.R


# Helper


.PHONY: clean
clean:
	rm -rf dennys/
	rm -rf lq/
	rm -f hw2.html