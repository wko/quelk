FROM hseeberger/scala-sbt

RUN mkdir myapp
COPY ./project myapp/project
COPY ./build.sbt myapp/build.sbt

RUN \ 
  echo 'export _JAVA_OPTIONS="-Xms1024m -Xmx2G -Xss256m"' >> .bashrc && \
  echo 'export REWRITER_ONTOLOGY_HOME="/root/data/ontologies"' >> /root/.bashrc
  
#RUN \ 
#  service postgresql start && \
#  cd "/" && \ 
#  echo "CREATE USER omqa WITH ENCRYPTED PASSWORD 'snomed'; ALTER USER omqa CREATEDB;" | su postgres -c psql
  
WORKDIR myapp

RUN sbt compile   

#RUN mv /root/myapp/data/ /root/data 

VOLUME ["/root/data/ontologies", "/root/myapp"]

EXPOSE 8080
CMD [sbt, "~;jetty:stop;jetty:start"]
