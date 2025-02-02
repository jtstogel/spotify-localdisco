FROM debian:bookworm-slim

# Dynamic library for Haskell binaries.
RUN apt-get update -y && apt-get install zip libgmp-dev -y

RUN mkdir -p /data /data/database /bin

COPY ./postal-codes.json /data/postal-codes.json

ARG STACK_INSTALL_PATH
COPY ${STACK_INSTALL_PATH}/bin/localdisco_server /bin/localdisco_server

EXPOSE 80

ENTRYPOINT [ \
    "/bin/localdisco_server", \
    "--port=80", \
    "--postal_codes_path=/data/postal-codes.json", \
    "--database_path=/data/database/db.sqlite" \
]
