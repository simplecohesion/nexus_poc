# To enable ssh & remote debugging on app service change the base image to the one below
# FROM mcr.microsoft.com/azure-functions/python:4-python3.9-appservice
FROM mcr.microsoft.com/azure-functions/python:4-python3.9

# Install R
RUN apt-get update && apt-get install -y r-base

# Install necessary R packages
RUN R -e "install.packages(c('here', 'optparse', 'StatsBombR', 'httr', 'later', 'devtools', 'igraph', 'magrittr', 'dplyr', 'ggplot2', 'lattice', 'ggpubr', 'cowplot', 'Hmisc', 'plyr', 'png', 'ggpmisc', 'XML', 'xml2', 'tidyverse', 'lubridate', 'clue', 'stringr', 'readxl', 'ggrepel', 'ggtext', 'factoextra', 'REdaS', 'data.table', 'formattable', 'grid', 'gridExtra', 'patchwork', 'gghighlight', 'jsonlite'), repos='http://cran.rstudio.com/')"

# Copy the R scripts to the container, preserving folder structure
COPY r-script /app

ENV AzureWebJobsScriptRoot=/home/site/wwwroot \
    AzureFunctionsJobHost__Logging__Console__IsEnabled=true

COPY requirements.txt /
RUN pip install -r /requirements.txt

COPY . /home/site/wwwroot

# Ensure the R script has execute permissions
RUN chmod +x /app/**.R

# Set the CMD to start the Azure Functions runtime
# CMD [ "python", "/azure-functions-host/Microsoft.Azure.WebJobs.Script.WebHost.dll" ]