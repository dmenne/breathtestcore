rem rscript -e "rhub::check(platform = "debian-clang-devel")
rscript -e "rhub::check_on_ubuntu()"
rscript -e "rhub::check_with_rdevel()"
rscript -e "rhub::check_on_macos()"
rscript -e "rhub::check_on_solaris()"
ECHO "Window will be closed after 5 seconds"
@PING -n 5 127.0.0.1>nul 
