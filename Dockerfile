FROM ocaml/opam:ubuntu-22.04 AS binary
RUN opam update
RUN opam depext async async_ssl camlimages caqti caqti-driver-sqlite3 cohttp tls
RUN opam install -y 'base64'
RUN opam install -y base
RUN opam install -y ppx_bench ppx_expect variantslib sexplib
RUN opam install -y ppx_jane
RUN opam install -y core
RUN opam install -y async
RUN opam install -y async_ssl
RUN opam install -y re2
RUN opam install -y textutils
RUN opam install -y core_extended
RUN opam install -y tls
RUN opam install -y ssl
RUN opam install -y cohttp-async
RUN opam install -y caqti
RUN opam install -y caqti-driver-sqlite3
RUN opam install -y caqti-async
RUN opam install -y xmlm jsonaf ppx_jsonaf_conv
RUN opam install -y ppx_deriving
RUN sudo apt-get install -y libjpeg-dev libpng-dev
RUN opam install -y camlimages
RUN opam install -y bignum
RUN sudo apt-get update
RUN sudo apt-get install -y libopencv-dev

FROM binary AS builder
RUN mkdir /home/opam/hera
WORKDIR /home/opam/hera
ARG DUMMY=unknown
RUN DUMMY=${DUMMY} git clone https://github.com/pNre/Hera.git .
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
COPY --from=builder /home/opam/hera/bin/hera /app
VOLUME ["/database"]
CMD ["sh", "-c", "/app/hera"]