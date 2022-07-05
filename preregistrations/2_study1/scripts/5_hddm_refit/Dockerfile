# This Docker file creates a container with HDDM 0.9.5 and all dependencies pre-installed.

docker run -it --rm --cpus=5 -e NB_USER=jovyan -e CHOWN_HOME=yes -e CHOWN_EXTRA_OPTS='-R' -w /home/jovyan/ -v c:/repositories/attention_pilot:/home/jovyan/hddm -p 8888:8888 hcp4715/hddm:0.9.5 jupyter notebook
