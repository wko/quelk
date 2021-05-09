FROM hseeberger/scala-sbt:11.0.10_1.5.1_2.12.13

RUN mkdir myapp
COPY ./project myapp/project
COPY ./build.sbt myapp/build.sbt
COPY ./src myapp/src
WORKDIR /root/myapp

RUN sbt compile


#VOLUME ["/root/data/ontologies"]

EXPOSE 8080

ENV _JAVA_OPTIONS="-Xms1024m -Xmx10G -Xss256m"
ENV REWRITER_ONTOLOGY_HOME="/root/data/ontologies"

CMD ["sbt", "~;jetty:stop;jetty:start"]
