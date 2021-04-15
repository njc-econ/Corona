#!/usr/bin/env bash
docker run -d -p 4445:4444 selenium/standalone-chrome
Rscript ~/Corona/CollectionScripts/collectBundeslandData.R
Rscript ~/Corona/CollectionScripts/germanyVaccination.R
docker stop $(docker ps -q)
