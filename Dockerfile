FROM rocker/r-ver:4.1.0
 
# Install dependency libraries

RUN apt-get update && \ 
    apt-get install -y software-properties-common && \
    apt-get update && \
    add-apt-repository ppa:ubuntugis/ubuntugis-unstable && \
    apt-get install -y \
    libxml2-dev \
    libssh2-1-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libsasl2-dev \
	  libv8-dev \
    libudunits2-dev \ 
    libgdal-dev \
    libgeos-dev \ 
    libproj-dev \
    cron && \ 
    R -e "options(Ncpus = 4, repo='https://packagemanager.rstudio.com/all/__linux__/focal/latest'); install.packages(c('pacman', 'firebase', 'DT', 'shinydashboard', 'scales', 'lattice', 'shinyjs', 'shinyBS','RColorBrewer', 'dplyr', 'httr', 'leaflet', 'knitr', 'shiny', 'jsonlite', 'sf', 'devtools', 'lattice', 'plotly', 'leaflet.extras', 'ggthemes'))" && \          
    mkdir -p /home/nepal_app && chmod -R 755 /home/nepal_app && \
    chmod -R 777 /tmp && touch /var/log/cron.log  
 #   (crontab -l ; echo "*/15 * * * * Rscript /home/nepal_app/stream.R  >> /var/log/cron.log") | crontab && \
  #  cron && tail -f /var/log/cron.log
    
COPY .  /home/nepal_app

# expose port on Docker container
EXPOSE 4043

# RUN R -e "library(dplyr); library(RColorBrewer);library(sf);library(jsonlite); library(httr); library(leaflet)"

# run app.R as localhost and on exposed port in Docker container

CMD ["R", "-e", "shiny::runApp('/home/nepal_app/', host='0.0.0.0', launch.browser = F, port=4043)"]
