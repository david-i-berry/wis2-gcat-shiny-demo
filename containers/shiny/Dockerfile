FROM  rocker/shiny


ENV DEBIAN_FRONTEND="noninteractive" \
    TZ="Etc/UTC" \
    ECCODES_DIR=/opt/eccodes \
    PATH="${PATH}:/opt/eccodes/bin"

WORKDIR /tmp

# build r and r studio
# RUN apt-get install -y r-base
RUN apt-get update && \
    apt-get install -y libhiredis-dev

RUN R -e 'install.packages(c("leaflet"))' && \
    R -e 'install.packages(c("leaflet.extras"))' && \
    R -e 'install.packages(c("viridis"))' && \
    R -e 'install.packages(c("leafem"))' &&\
    R -e 'install.packages(c("jsonlite"))' && \
    R -e 'install.packages(c("htmltools"))' && \
    R -e 'install.packages(c("shinyjs"))' && \
    R -e 'install.packages(c("httr"))' && \
    R -e 'install.packages(c("redux"))' && \
    R -e 'install.packages(c("shinydashboard"))' && \
    R -e 'install.packages(c("shinyjs"))'
