FROM r-base:4.0.3
RUN apt-get update -y && \
    apt-get install -y curl libxml2-dev libssl-dev libcurl4-openssl-dev python-dev-is-python3 git
### copy in scripts
COPY *.R /usr/local/bin/
### install R packages 
RUN Rscript /usr/local/bin/install_packages.R
### install ica CLI
RUN wget -O /usr/local/bin/icav2 "https://stratus-documentation-us-east-1-public.s3.amazonaws.com/cli/2.2.0/linux/icav2"  && \
    chmod u+x /usr/local/bin/icav2
#### for automating ICA CLI setup
RUN apt-get install -y expect
#### copy script to automate ICA CLI setup
COPY *exp /usr/local/bin/
### install nf-core python module and nextflow
RUN apt-get update -y && \
    apt-get install -y openjdk-11-jdk
RUN cd /usr/local/bin && \
    curl -s https://get.nextflow.io | bash
RUN apt-get install -y python3-pip
RUN pip3 install nf-core && \
    nf-core list --json > /usr/local/bin/nf-core.pipeline.json
RUN apt-get install -y libssl-dev  ca-certificates build-essential && \
	update-ca-certificates  
### copy nextflow templates for copying intermediate files + logs from ICA
COPY dummy_template.txt /usr/local/bin/
COPY copy_workfiles.nf /usr/local/bin/
WORKDIR /usr/local/bin