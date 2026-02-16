# Use the official R-Shiny image as a base
FROM rocker/shiny:4.3.1

# Install Linux system dependencies for RPostgres and other packages
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Install the R packages your app needs
RUN R -e "install.packages(c('shiny', 'bslib', 'pool', 'DBI', 'RPostgres', 'dplyr', 'lubridate', 'stringr', 'DT', 'shinyjs', 'rhandsontable', 'jsonlite', 'glue'), repos='https://cran.rstudio.com/')"

# Create a directory for the app
RUN mkdir /app
WORKDIR /app

# Copy your app files into the container
# This includes app.R and ca-certificate.crt
COPY . /app

# Expose the port DigitalOcean expects (8080)
EXPOSE 8080

# Run the app on port 8080 and bind to all network interfaces
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=8080)"]