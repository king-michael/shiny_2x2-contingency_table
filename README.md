# Shiny app for a 2x2 contingency table
A simple app that provides you an interface to upload qualitative reference and test data.
It calculates the 2x2 contingency table along with various performance metrics and confidence intervals.


## How to dockerize
* Guide : https://www.r-bloggers.com/2019/02/deploying-an-r-shiny-app-with-docker/

### Build the docker files

build the images
```
docker build -t 2x2contingency_table/shiny_app -f Dockerfile.shinyapp .
docker build -t 2x2contingency_table/shinyproxy shinyproxy
```

Start the docker compose
```
docker-compose build
docker-compose up -d
```

Connect to "127.0.0.1:8888"

### Testing
```
docker run --name=shiny_app --user shiny --rm -p 88:3838 2x2contingency_table/shiny_app
```
