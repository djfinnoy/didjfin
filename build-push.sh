#!/bin/bash

cd web
docker build -t djfinnoy/didjfin-web .
docker push djfinnoy/didjfin-web

cd ../shiny
docker build -t djfinnoy/didjfin-shiny .
docker push djfinnoy/didjfin-shiny
