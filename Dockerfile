FROM rocker/rstudio:4.1.2

LABEL maintainer="p.c.s.vermeent@gmail.com"
LABEL description="Rstudio container with HDDM Python dependencies"

# Install Renv and Copy Lock File
ENV RENV_VERSION 0.15.5
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"


COPY renv.lock renv.lock

ENV RENV_PATHS_LIBRARY renv/library/R-4.2


# Install base utilities
RUN apt-get update && \
    apt-get install -y && \
    rm -rf /var/lib/apt/lists/*

# Install miniconda
ENV CONDA_DIR /opt/conda
RUN wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh && \
     /bin/bash ~/miniconda.sh -b -p /opt/conda

# Put conda in path so we can use conda activate
ENV PATH=$CONDA_DIR/bin:$PATH

# Create conda environment with python 3.7
RUN conda create --name py37 python=3.7

# Make RUN commands use the new environment:
SHELL ["conda", "run", "-n", "py37", "/bin/bash", "-c"]

# Install HDDM dependencies
RUN pip install cython
RUN conda install pymc==2.3.8
RUN conda install -c anaconda git
RUN pip install git+https://github.com/hddm-devs/kabuki
RUN conda install h5py
RUN conda install -c conda-forge netcdf4
RUN pip install git+https://github.com/hddm-devs/hddm






