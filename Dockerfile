FROM haskell:7.10

RUN cabal update
COPY ./lab_certified_software_development.cabal .
RUN cabal install --only-dependencies -j4
COPY . .
RUN cabal build && cabal test
RUN cabal install

CMD ["lab_certified_software_development"]
