FROM ocaml/opam:ubuntu-22.04-ocaml-4.14 AS base
RUN opam update
RUN opam depext async.v0.16.0 async_ssl.v0.16.0 camlimages caqti.1.9.0 caqti-driver-sqlite3.1.9.0 cohttp tls
RUN opam install -y 'base64'
RUN opam install -y base.v0.16.3
RUN opam install -y ppx_bench.v0.16.0 ppx_expect.v0.16.0 variantslib.v0.16.0 sexplib.v0.16.0
RUN opam install -y ppx_jane.v0.16.0
RUN opam install -y core.v0.16.0
RUN opam install -y async.v0.16.0
RUN opam install -y async_ssl.v0.16.1
RUN opam install -y re2.v0.16.0
RUN opam install -y textutils.v0.16.0
RUN opam install -y core_extended.v0.16.0
RUN opam install -y tls
RUN opam install -y ssl
RUN opam install -y cohttp-async.5.3.0
RUN opam install -y caqti.1.9.0
RUN opam install -y caqti-driver-sqlite3.1.9.0
RUN opam install -y caqti-async.1.9.0
RUN opam install -y xmlm jsonaf.v0.16.0 ppx_jsonaf_conv.v0.16.0
RUN opam install -y ppx_deriving
RUN sudo apt-get install -y libjpeg-dev libpng-dev
RUN opam install -y camlimages.5.0.4
RUN opam install -y bignum.v0.16.0
RUN sudo apt-get update
RUN sudo apt-get install -y libopencv-dev

FROM base AS builder
RUN mkdir /home/opam/hera
WORKDIR /home/opam/hera
COPY --chown=opam . .
RUN eval `opam config env` && make

FROM ubuntu:22.04
RUN apt-get update
RUN apt-get install -y ca-certificates
RUN apt-get install -y sqlite openssl libjpeg-dev libpng-dev
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get install -qq libopencv-contrib4.5
WORKDIR /app
RUN mkdir /app/assets
ENV TZ=Europe/Rome
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
ENV PORT 8001
EXPOSE 8001
COPY --from=builder /home/opam/hera/hera/module_faces/assets /app/assets
COPY --from=builder /home/opam/hera/_build/install/default/bin/hera /app
VOLUME ["/database"]
CMD ["sh", "-c", "/app/hera"]