# To enable ssh & remote debugging on app service change the base image to the one below
# FROM mcr.microsoft.com/azure-functions/node:4-node18-appservice
FROM mcr.microsoft.com/azure-functions/node:4-node18

# Install R
RUN apt-get update && apt-get install -y r-base

# Install necessary R packages
RUN R -e "install.packages(c('here', 'optparse', 'StatsBombR', 'httr', 'later', 'devtools', 'igraph', 'magrittr', 'dplyr', 'ggplot2', 'lattice', 'ggpubr', 'cowplot', 'Hmisc', 'plyr', 'png', 'ggpmisc', 'XML', 'xml2', 'tidyverse', 'lubridate', 'clue', 'stringr', 'readxl', 'ggrepel', 'ggtext', 'factoextra', 'REdaS', 'data.table', 'formattable', 'grid', 'gridExtra', 'patchwork', 'gghighlight', 'jsonlite'), repos='http://cran.rstudio.com/')"

# Copy the R scripts to the container, preserving folder structure
COPY r-script /app

ENV AzureWebJobsScriptRoot=/home/site/wwwroot \
  AzureFunctionsJobHost__Logging__Console__IsEnabled=true

COPY . /home/site/wwwroot

RUN cd /home/site/wwwroot && \
  npm install && \
  npm run build