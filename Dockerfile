FROM racket/racket:8.2
COPY src src
COPY doc doc
COPY test test
RUN raco pkg install --deps search-auto compiler-lib
RUN raco make test/run.rkt
ENTRYPOINT ["racket", "src/main.rkt"]
