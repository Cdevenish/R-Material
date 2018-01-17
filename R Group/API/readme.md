---
title: "API intro"
author: "C Devenish"
date: "16 January 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# API work flow

## Common packages

- interface to web  
RCurl 
httr

- handling data  
jsonlite 
XML 

## Issues and tips

Authentication?
Header info?

Try the urls first in a browser

Identify variables to loop through

urlencode text

Paste the url together

Make a vector of urls OR loop through vectors of variables

Is there a maximum page size for results?

Explore outputs
What format is it? XML or JSON?


How to manage data from outputs



## Three examples

- Sunset, daylength times

- IUCN Red List

(- Scopus)
