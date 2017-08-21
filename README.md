# Adhoc Continuous Integration
> Building a CI tool from the ground up with Docker

This project demonstrates how to build a CI system from the ground up using
only Docker. The text can be found here:

<https://sgillespie.github.io/adhoc-continuous-integration>

## Running the Examples
Examples for each of the presented systems are included. For convenience, a
Docker Compose file is included. 

To run the `docker-build` example, run:

    cd docker-build
    docker-compose build
    docker-compose run builder

The demo site should now be running at <http://localhost>.

To run the `container-orchestrator` example, run:

    cd container-orchestrator
    docker-compose build
    docker-compose run orchestrator
    docker-compose run react_boilerplate

Again, the demo site will be running at <http://localhost>. Readers are 
encouraged to play around with the values in the `docker-compose.yml` files.

You can clean up either of these projects by running:

    docker-compose down -v

## Building the Documentation
In order to build the documentation, you must have:

 * GHC (tested on 8.0.x)
 * Shake
 * Pandoc
 * Pandoc-citeproc

After meeting these prerequisites you can build using the script:

    ./build.sh

## Acknowledgements
This project builds on works by a variety of resources:

 * [Docker](https://docker.com)
 * [React Boilerplate](https://www.reactboilerplate.com/)
 * [Martin Fowler](https://martinfowler.com/)
 * [Engine Yard](https://www.engineyard.com/)
 * Jason Sankey

## Author

 * Sean D Gillespie: <sean@mistersg.net>

## License
Copyright 2017 Sean Gillespie. This project is released using the
[MIT license](LICENSE)
