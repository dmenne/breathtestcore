rscript -e "rhub::check_for_cran()"
ECHO "Window will be closed after 5 seconds"
@PING -n 5 127.0.0.1>nul 
