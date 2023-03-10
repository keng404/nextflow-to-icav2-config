FROM r-base:4.0.2
RUN apt-get update -y && \
    apt-get install -y curl libxml2-dev libssl-dev libcurl4-openssl-dev python-dev-is-python3 git
### copy in scripts
COPY *.R /usr/local/bin/
COPY testing_pipelines /usr/local/bin/
COPY pipeline_development /usr/local/bin/
COPY design_docs /usr/local/bin/
COPY ica_configure /usr/local/bin/
COPY launch_pipelines /usr/local/bin/
COPY legacy /usr/local/bin/
COPY create_xml /usr/local/bin/
COPY create_pipeline_on_ica /usr/local/bin/
ENV PATH $PATH:/usr/local/bin/testing_pipelines:/usr/local/bin/design_docs:/usr/local/bin/pipeline_development:/usr/local/bin/ica_configure:/usr/local/bin/launch_pipelines:/usr/local/bin/legacy:/usr/local/bin/create_xml:/usr/local/bin/create_pipeline_on_ica
### install R packages 
RUN Rscript /usr/local/bin/install_packages.R
ENV CLI_VERSION "2.2.0"
### install ica CLI
RUN wget -O /usr/local/bin/icav2 "https://stratus-documentation-us-east-1-public.s3.amazonaws.com/cli/${CLI_VERSION}/linux/icav2"  && \
    chmod u+x /usr/local/bin/icav2
#### for automating ICA CLI setup
RUN apt-get install -y tcl8.6 && \
    apt-get install -y expect --fix-missing
#### copy script to automate ICA CLI setup
COPY *.exp /usr/local/bin/
### install nf-core python module and nextflow
RUN apt-get update -y && \
    apt-get install -y openjdk-11-jdk
RUN cd /usr/local/bin && \
    curl -s https://get.nextflow.io | bash
##############################################
ENV NFCORE_TOOLS_VERSION "2.7.2"
###### install nf-core in a virtual env and generate pipeline.JSON file to be used by scripts later.
##################################
RUN wget --no-check-certificate "https://github.com/nf-core/tools/archive/refs/tags/${NFCORE_TOOLS_VERSION}.zip" && \
    unzip ${NFCORE_TOOLS_VERSION}.zip && \
    cd tools-${NFCORE_TOOLS_VERSION} && \
    python3 setup.py install && \
    cd /usr/local/bin/
#RUN pip3 install nf-core && \
#    nf-core list --json > /usr/local/bin/nf-core.pipeline.json
RUN apt-get install -y libssl-dev  ca-certificates build-essential && \
	update-ca-certificates  
### copy nextflow templates for copying intermediate files + logs from ICA
COPY *.txt /usr/local/bin/
COPY *.config /usr/local/bin/
WORKDIR /usr/local/bin