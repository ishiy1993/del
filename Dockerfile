FROM haskell as builder

COPY . /work
WORKDIR /work

RUN stack setup && stack install --local-bin-path .

FROM ishiy1993/formura-bin

COPY --from=builder /work/mk-sode1 /usr/local/bin/
COPY --from=builder /work/del /usr/local/bin/

WORKDIR /work
CMD ["bash"]
