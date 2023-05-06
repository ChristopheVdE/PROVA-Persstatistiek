# Load required R PACKAGE ======================================================
# Function to install and load packages ----------------------------------------
load_packages<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}
# Define required packages -----------------------------------------------------
Packages <- c(
"glue", "renv", "readr", "rsconnect"
)
#Load required packages --------------------------------------------------------
load_packages(Packages)
# ==============================================================================

shiny_write_docker = function(
    path = ".", appdir = "app", lockfile = "shiny_renv.lock",
    port = 3838, expose = TRUE, rspm = TRUE
) {
  rspm_env = ifelse(
    rspm,
    "ENV RENV_CONFIG_REPOS_OVERRIDE https://packagemanager.rstudio.com/cran/latest\n",
    ""
  )
  from_shiny_version = glue::glue("FROM rocker/shiny:{getRversion()}")
  renv::snapshot(
    project = path,
    lockfile = lockfile,
    prompt = FALSE,
    force = TRUE
  )
  pkgs = renv::dependencies(appdir)$Package
  sys_reqs = glue_sys_reqs(pkgs)
  copy_renv = glue::glue("COPY ./{lockfile} renv.lock")
  renv_install = 'RUN Rscript -e "install.packages(\'renv\')"'
  renv_restore  = 'RUN Rscript -e "renv::restore()"'
  
  copy_app = "COPY . /srv/shiny-server/"
  expose = ifelse(expose, glue::glue("EXPOSE {port}"), "")
  stdout = 'ENV SHINY_LOG_STDERR=1'
  cmd = 'CMD ["/usr/bin/shiny-server"]'
  
  ret = purrr::compact(list(
    from_shiny_version,
    rspm_env,
    sys_reqs,
    copy_renv,
    renv_install,
    renv_restore,
    copy_app,
    expose,
    stdout, 
    cmd
  ))
  readr::write_lines(ret, file = file.path(path, "Dockerfile"))
}
