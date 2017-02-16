FROM hammerlab/coclobas

RUN opam pin -n add coclobas https://github.com/hammerlab/coclobas.git
RUN opam pin -n add ketrew https://github.com/hammerlab/ketrew.git
RUN opam pin -n add biokepi https://github.com/hammerlab/biokepi.git
RUN opam pin -n add genspio https://github.com/hammerlab/genspio.git
RUN opam pin -n add secotrec https://github.com/hammerlab/secotrec.git
RUN opam upgrade
RUN opam install tls secotrec biokepi
