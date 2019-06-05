rscript -e "rhub::check_on_ubuntu()"
rscript -e "rhub::check(platform = "debian-clang-devel")
ECHO "Window will be closed after 5 seconds"
@PING -n 5 127.0.0.1>nul 
