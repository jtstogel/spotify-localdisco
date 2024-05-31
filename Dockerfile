FROM debian:bookworm-slim

# Dynamic library for Haskell binaries.
RUN apt-get update -y && apt-get install zip libgmp-dev -y

RUN mkdir -p /data /bin

COPY ./postal-codes.json.zip /data/postal-codes.json.zip
RUN unzip /data/postal-codes.json.zip -d /data/

ARG STACK_INSTALL_PATH
COPY ${STACK_INSTALL_PATH}/bin/localdisco_server /bin/localdisco_server

ENTRYPOINT [ \
    "/bin/localdisco_server", \
    "--port=8080", \
    "--postal_codes_path=/data/postal-codes.json", \
    "--database_path=/data/db.sqlite" \
]
