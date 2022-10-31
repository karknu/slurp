# slurp

slurp is part of a block propagation delay proof of concept for Cardano.

slurp consumes mqtt messages from blockPerf.sh and inserts them into a
postgresql data base.

## How to Build

cabal build

Tested with ghc 8.10.7

## Configuration Variables

### SLURP_DB

A postgresql connection string, example "host=127.0.0.1 port=5432 dbname=slurpdb"

### MQTT_USER and MQTT_PW

Optional user and password to your mqtt broker.

### MQTT_HOST and MQTT_PORT

Host name and port number for your mqtt broker.

### IPR_KEY
Optional API key for geolocation lookup through https://ipregistry.co .
Results are cached to minimize the number of API calls.

