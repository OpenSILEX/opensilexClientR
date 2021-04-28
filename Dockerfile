ARG  R_VERSION=latest

FROM rocker/tidyverse:${R_VERSION}

ENV R_REPOS http://cloud.R-project.org/

COPY . /tmp/package

WORKDIR /tmp/package

# for pdf resizing
# RUN apt-get install qpdf -y

RUN R -e 'options(repos =c(CRAN=Sys.getenv("R_REPOS")));install.packages("devtools");devtools::install_deps(build_vignettes=TRUE, dependencies = TRUE, upgrade = TRUE)'

