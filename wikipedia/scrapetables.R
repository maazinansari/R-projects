# from http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html

## rvest ----
library(rvest)

url = "http://en.wikipedia.com/wiki/Barack_Obama"
webpage = read_html(url)
tbls = html_nodes(webpage, "table")
head(tbls)

tbls_ls = webpage %>%
        html_nodes("table") %>%
        .[1] %>%
        html_table(fill = TRUE)

str(tbls_ls[[1]])

## XML ---
library(XML)
library(RCurl)

# read in HTML data
tabs = getURL(url)
tbls_xml = readHTMLTable(tabs)
tbls_xml
