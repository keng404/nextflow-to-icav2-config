FROM r-base:4.3.1
RUN apt-get update -y && \
    apt-get install -y curl libxml2-dev libssl-dev libcurl4-openssl-dev python-dev-is-python3 git
### copy in scripts
ENV SCRIPT_DIR /scripts
WORKDIR ${SCRIPT_DIR}
RUN chmod -R 777 ${SCRIPT_DIR}
COPY *.R ${SCRIPT_DIR}/
COPY testing_pipelines ${SCRIPT_DIR}/testing_pipelines/
COPY pipeline_development ${SCRIPT_DIR}/pipeline_developement/
COPY design_docs ${SCRIPT_DIR}/design_docs/
COPY ica_configure ${SCRIPT_DIR}/ica_configure/
COPY launch_pipelines ${SCRIPT_DIR}/launch_pipelines/
COPY create_xml ${SCRIPT_DIR}/create_xml/
COPY create_pipeline_on_ica ${SCRIPT_DIR}/create_pipeline_on_ica/
ENV PATH $PATH:${SCRIPT_DIR}/testing_pipelines:${SCRIPT_DIR}/design_docs:${SCRIPT_DIR}/pipeline_development:${SCRIPT_DIR}/ica_configure:${SCRIPT_DIR}/launch_pipelines:${SCRIPT_DIR}/create_xml:${SCRIPT_DIR}/create_pipeline_on_ica
### install R packages
RUN apt-get update -y && \
    apt-get install -y libssl-dev  ca-certificates build-essential && \
        update-ca-certificates
RUN Rscript ${SCRIPT_DIR}/install_packages.R
ENV CLI_VERSION "2.27.0"
### install ica CLI
RUN cd ${SCRIPT_DIR} && \
    wget -O ica-linux-amd64.zip "https://stratus-documentation-us-east-1-public.s3.amazonaws.com/cli/${CLI_VERSION}/ica-linux-amd64.zip"  && \
    unzip ica-linux-amd64.zip && \
    chmod u+x ${SCRIPT_DIR}/linux-amd64/icav2
#RUN wget -O ${SCRIPT_DIR}/icav2 "https://stratus-documentation-us-east-1-public.s3.amazonaws.com/cli/${CLI_VERSION}/linux/icav2"  && \
#    chmod u+x ${SCRIPT_DIR}/icav2    
#### for automating ICA CLI setup
RUN apt-get install -y tcl8.6 && \
    apt-get install -y expect --fix-missing
#### copy script to automate ICA CLI setup
COPY *.exp ${SCRIPT_DIR}/
### install nf-core python module and nextflow
RUN apt-get update -y && \
    apt-get install -y openjdk-11-jdk

ENV NF_VERSION  "22.04.3"
RUN cd ${SCRIPT_DIR} && \
    wget "https://github.com/nextflow-io/nextflow/releases/download/v${NF_VERSION}/nextflow"
##    curl -s https://get.sdkman.io | bash 
## RUN sdk install java 17.0.10-tem && \
##    curl -s https://get.nextflow.io | bash
##############################################
ENV NFCORE_TOOLS_VERSION "2.7.2"
###### install nf-core in a virtual env and generate pipeline.JSON file to be used by scripts later.
##################################
RUN wget --no-check-certificate "https://github.com/nf-core/tools/archive/refs/tags/${NFCORE_TOOLS_VERSION}.zip" && \
    unzip ${NFCORE_TOOLS_VERSION}.zip && \
    rm ${NFCORE_TOOLS_VERSION}.zip

#RUN apt-get install -y python3.11-venv && \
#    python3 -m venv /usr/local/bin/myvirt && \
#    /usr/local/bin/myvirt/bin/pip3 install nf-core
##ENV PATH $PATH:/usr/local/bin/myvirt/bin:${SCRIPT_DIR}:${SCRIPT_DIR}/linux-amd64
COPY *.txt ${SCRIPT_DIR}/
COPY *.config ${SCRIPT_DIR}/
WORKDIR /scripts
### ensure all executables and scripts can be run
RUN chmod -R 777 ${SCRIPT_DIR}
RUN apt-get update -y && apt-get -y install openssl
####RUN adduser --disabled-password --gecos '' icauser