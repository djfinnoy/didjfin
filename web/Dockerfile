FROM rocker/tidyverse:latest as GENSITE

# Install R packages
RUN R -e "install.packages(c( \
    'ggthemes', \
    'gridExtra', \
    'lubridate', \
    'DT', \
    'jsonlite', \
    'plotly' \
  ), repos = 'http://cran.rstudio.com/')"

RUN R -e "devtools::install_github('rstudio/blogdown')"
RUN R -e "blogdown::install_hugo()"

# Install Python dependencies
RUN apt-get update && apt-get -y --no-install-recommends install \
  python-pip \
  libpython-dev

RUN pip install requests  # Required to load data from FPL API

# Build site
COPY ./didjfin /web/
RUN cd /web && R -e "blogdown::build_site()"

FROM nginx:stable-alpine
COPY --from=GENSITE /web/public /usr/share/nginx/html
COPY ./nginx.conf /etc/nginx/nginx.conf
EXPOSE 8081
