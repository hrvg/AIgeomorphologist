FROM rocker/geospatial:latest

# APT UTILS
RUN apt-get update && \
    apt-get install -y \
    build-essential \
    apt-utils \
    apt-transport-https \
    ca-certificates \
    software-properties-common \
    pkg-config \
    curl \
    wget \
    unzip \
    gpg-agent \
    sudo \ 
    tzdata \
    locales


# DVC
RUN apt-get install -y python3-pip && \
	pip3 install dvc[s3]

# NODE JS NPM CML
RUN apt-get install -y nodejs npm && \
	apt-get install -y libcairo2-dev libpango1.0-dev libjpeg-dev libgif-dev librsvg2-dev libfontconfig-dev
RUN npm config set user 0 && \
	npm install -g @mapbox/node-pre-gyp && \
	npm install -g canvas vega-cli vega-lite && \
	npm install -g @dvcorg/cml

# TERRAFORM
RUN apt-get update && apt-get install -y gnupg software-properties-common curl && \
	curl -fsSL https://apt.releases.hashicorp.com/gpg | apt-key add - && \
	apt-add-repository "deb [arch=amd64] https://apt.releases.hashicorp.com $(lsb_release -cs) main" && \
	apt-get update && apt-get install -y terraform

# R
RUN R -e "install.packages(c('devtools', 'roxygen2', 'pkgdown', 'covr', 'lintr'), lib = '/usr/local/lib/R/site-library')"

# TEX
RUN tlmgr update --self && \
    tlmgr install collection-latexrecommended collection-fontsrecommended


# R PKG DEPS
RUN mkdir /home/rstudio/project
COPY . /home/rstudio/project/
RUN cd /home/rstudio/project/ && \ 
	R -e "withr::with_libpaths('/usr/local/lib/R/site-library', devtools::install_deps(dependencies = TRUE, build = FALSE))"