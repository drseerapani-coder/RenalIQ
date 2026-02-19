# Use the official R-Shiny image as a base
FROM rocker/shiny:4.3.1

# 1. Install Linux system dependencies
# Added libfontconfig1 and x11 for rhandsontable/rendering support
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libtesseract-dev \
    tesseract-ocr \
    tesseract-ocr-eng \ 
    libpoppler-cpp-dev \
    libfontconfig1 \
    mesa-common-dev \
    && rm -rf /var/lib/apt/lists/*

# 2. Install R packages in layers
# Layer 1: Core Tidyverse and DB (Stable)
RUN R -e "install.packages(c('shiny', 'bslib', 'pool', 'DBI', 'RPostgres', \
    'dplyr', 'tidyr', 'lubridate', 'stringr', 'DT', 'shinyjs', \
    'rhandsontable', 'jsonlite', 'glue', 'pdftools', 'tesseract', \
    'openai', 'shinycssloaders'), repos='https://cran.rstudio.com/')"
	
# Layer 2: UI Components (rhandsontable can be picky about dependencies)
RUN R -e "install.packages(c('shiny', 'bslib', 'shinyjs', 'DT', 'rhandsontable', 'shinycssloaders'), repos='https://cran.rstudio.com/')"

# Layer 3: AI and OCR (Heavy lifting)
RUN R -e "install.packages(c('pdftools', 'tesseract', 'openai'), repos='https://cran.rstudio.com/')"

# 3. App Setup
# Using /srv/shiny-server is standard, but /app is fine for DigitalOcean 
RUN mkdir /app
WORKDIR /app

# Copy files (Make sure .gitignore excludes large local data folders)
COPY . /app

# 4. Security & Permissions
# DigitalOcean/App Platform runs as a non-root user for security
RUN chown -R shiny:shiny /app
USER shiny

# 5. Runtime Config
EXPOSE 8080

# Run the app - using the 'port' argument to match DigitalOcean's expectation
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=8080)"]