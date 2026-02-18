# Use the official R-Shiny image as a base
FROM rocker/shiny:4.3.1

# Install Linux system dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    # System dependencies for OCR (tesseract) and PDF (pdftools)
    libtesseract-dev \
    tesseract-ocr \
    # CRITICAL: Adds English language support for Tesseract
    tesseract-ocr-eng \ 
    libpoppler-cpp-dev \
    && rm -rf /var/lib/apt/lists/*

# Install the R packages your app needs
# Added 'shinycssloaders' for mobile UI feedback
RUN R -e "install.packages(c('shiny', 'bslib', 'pool', 'DBI', 'RPostgres', 'dplyr', 'lubridate', 'stringr', 'DT', 'shinyjs', 'rhandsontable', 'jsonlite', 'glue', 'pdftools', 'tesseract', 'openai', 'shinycssloaders'), repos='https://cran.rstudio.com/')"

# Create a directory for the app
RUN mkdir /app
WORKDIR /app

# Copy your app files into the container
COPY . /app

# Expose the port DigitalOcean expects (8080)
EXPOSE 8080

# Run the app
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=8080)"]