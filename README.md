
## How to dockerize
* Guide : https://www.r-bloggers.com/2019/02/deploying-an-r-shiny-app-with-docker/

### Build the docker files

build the images
```
docker build -t roche/shiny_app -f Dockerfile.shinyapp .
docker build -t roche/shinyproxy shinyproxy
```

Start the docker compose
```
docker-compose build
docker-compose up -d
```

Connect to "127.0.0.1:8888"

### Testing
```
docker run --name=shiny_app --user shiny --rm -p 88:3838 roche/shiny_app
```
