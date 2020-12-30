FROM ubuntu:latest


RUN apt-get update \
    && apt-get install -y --no-install-recommends scala

COPY bin/ .

RUN chmod +x ./ScalaMain.scala \
    && scalac ./ScalaMain.scala \
    && chmod +x ./ScalaMain.class

CMD ["scala", "-classpath", "./", "ScalaMain"]
