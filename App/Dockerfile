FROM rocker/shiny:4.3.0
ENV RENV_CONFIG_REPOS_OVERRIDE https://packagemanager.rstudio.com/cran/latest

RUN apt-get update -qq && \ 
  apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libicu-dev \
    libssl-dev \
    make \
    nano \
    pandoc \
    zlib1g-dev && \
  apt-get clean && \ 
  rm -rf /var/lib/apt/lists/*
COPY ./shiny_renv.lock renv.lock
RUN Rscript -e "install.packages('renv')"
RUN Rscript -e "renv::restore()"
COPY . /srv/shiny-server/
USER shiny
EXPOSE 3838
ENV SHINY_LOG_STDERR=1
CMD ["/usr/bin/shiny-server"]
