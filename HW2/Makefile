# This Makefile is used to generate the final document

hw2.html: hw2.Rmd lq/lq_data.Rdata dennys/dennys_data.Rdata
	Rscript -e "library(rmarkdown);render('hw2.Rmd')"
	touch hw2.html


# La Quinta files

lq/lq_data.Rdata: lq_hotel_parse.R lq/hotels/%.html
	Rscript lq_hotel_parse.R
	touch lq/lq_data.Rdata

lq/hotels/%.html: lq_hotel_download.R lq/list.Rdata
	Rscript lq_hotel_download.R
	touch lq/hotels/%.html

lq/list.Rdata: lq_list_parse.R lq/interactive_map.html
	Rscript lq_list_parse.R
	touch lq/list.Rdata

lq/interactive_map.html: lq_list_download.R 
	Rscript lq_list_download.R
	touch lq/interactive_map.html


# Denny's files

dennys/dennys_data.Rdata: dennys_parse.R dennys/%.html
	Rscript dennys_parse.R
	touch dennys/dennys_data.Rdata

dennys/%.html: dennys_download.R 
	Rscript dennys_download.R
	touch dennys/%.html


# Helper


.PHONY: clean
clean:
	rm -rf dennys/
	rm -rf lq/
	rm -f hw2.html