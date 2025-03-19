FROM ubuntu:jammy

ENV DEBIAN_FRONTEND=noninteractive

# install basic prereqs
RUN apt-get update && \
    apt install -y gdebi-core curl

# install R for ability to build custom R packages 
ARG R_VERSION
RUN curl -O https://cdn.rstudio.com/r/ubuntu-2204/pkgs/r-${R_VERSION}_1_amd64.deb &&  \
    gdebi -n r-${R_VERSION}_1_amd64.deb && \
    rm -f r-${R_VERSION}_1_amd64.deb 


# install Python (and deps) for the ability to build custom Python packages 
ARG PYTHON_VERSION
RUN curl -O https://cdn.rstudio.com/python/ubuntu-2204/pkgs/python-${PYTHON_VERSION}_1_amd64.deb && \
    gdebi -n python-${PYTHON_VERSION}_1_amd64.deb && \
    /opt/python/${PYTHON_VERSION}/bin/pip install virtualenv build && \
    rm -f python-${PYTHON_VERSION}_1_amd64.deb

# install Posit Connect
ARG PSC_VERSION
RUN curl -O https://cdn.posit.co/connect/${PSC_VERSION%.*}/rstudio-connect_${PSC_VERSION}~ubuntu22_amd64.deb && \
    gdebi -n rstudio-connect_${PSC_VERSION}~ubuntu22_amd64.deb && \
    rm -f rstudio-connect_${PSC_VERSION}~ubuntu22_amd64.deb 

# install sudo to allow starting of package manager under service account
RUN apt-get install -y sudo jq

RUN cp /etc/pam.d/login /etc/pam.d/rstudio-connect

RUN useradd -m -s /bin/bash packager 
RUN echo "packager:th1s1awes0me" | chpasswd

RUN apt-get install -y psmisc #killall command

COPY scripts/psc-start.sh /
COPY config/rstudio-connect.gcfg /etc/rstudio-connect

# https://docs.posit.co/connect/admin/programmatic-provisioning/index.html#example-workflow

RUN head -c 32 /dev/random | base64 > /var/lib/rstudio-connect/secret.key




ENTRYPOINT ["/psc-start.sh"]



