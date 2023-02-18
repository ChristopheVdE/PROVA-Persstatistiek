# get shiny server and R from the rocker project
FROM rocker/shiny:4.2.2

# Install system requirements for index.R as needed
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*


# RUN apt-get update && apt-get install -y \
#     --no-install-recommends \
#     sudo \
#     libcairo2-dev \
#     libcurl4-gnutls-dev \
#     curl \
#     libsodium-dev \
#     libxt-dev \
#     libssl-dev \
#     libssh2-1-dev \
#     libxml2-dev \
#     libicu-dev \
#     net-tools \
#     && apt-get clean \
#     && rm -rf /var/lib/apt/lists/*

# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages("shiny", dependencies = TRUE)'
RUN R -e 'install.packages("shinydashboard", dependencies = TRUE)'
RUN R -e 'install.packages("shinywidgets", dependencies = TRUE)'
RUN R -e 'install.packages("knitr", dependencies = TRUE)'
RUN R -e 'install.packages("RColorBrewer", dependencies = TRUE)'
RUN R -e 'install.packages("colourpicker", dependencies = TRUE)'
RUN R -e 'install.packages("scales", dependencies = TRUE)'
RUN R -e 'install.packages("ggplot2", dependencies = TRUE)'
RUN R -e 'install.packages("ISOweek", dependencies = TRUE)'
RUN R -e 'install.packages("DT", dependencies = TRUE)'
RUN R -e 'install.packages("janitor", dependencies = TRUE)'
RUN R -e 'install.packages("readxl", dependencies = TRUE)'
RUN R -e 'install.packages("rmarkdown", dependencies = TRUE)'

# # Create usergroup + user
RUN addgroup --system app \
    && adduser --system --ingroup shiny shiny

# Set working directory
# WORKDIR /home/app

# Sopy the app directory into the image
COPY ./App/* /srv/shiny-server/

# Set permissions
# RUN chown app:app -R /home/app

# Set active user
USER shiny

# Open container-port
# EXPOSE 3838

# run app
CMD ["/usr/bin/shiny-server"]