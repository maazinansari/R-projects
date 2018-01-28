library(XML)
library(WikipediR)
library(magrittr)


# infobox ----
get_infobox = function(page_name, class = "infobox vcard") {
    p_content = page_content(language = "en", project =  "wikipedia", page_name = page_name)
    p_text = p_content[["parse"]][["text"]][["*"]]
    p_parse = htmlParse(p_text, asText = TRUE)
    infobox = getNodeSet(doc = p_parse,
                         path = sprintf("//table[@class = '%s']",  class))
    free(p_parse)
    return(infobox[[1]])
}

# TODO:
# - Format into sublists using tr, th, td tags
# xpathApply(p_parse, "//tr/th")[1:10]
# -- as they appear in Wikipedia infobox
# -- sections are tr tags with one th tag, "display:none" before
# -- fix issue with xmlValue() and <br/> tag
# - Find and format dates, years

format_infobox = function(infobox) {
    # get value
    infobox_list = lapply(xmlChildren(infobox), xmlValue)
    # text separated by \n are separate list elements
    infobox_list = sapply(infobox_list, strsplit, split = "\n")
    # name list elements
    names(infobox_list) = lapply(infobox_list, function(x) x[1])
    # remove name from each list element
    infobox_list = lapply(infobox_list, function(x) x[-1])
    # remove "" in each list element
    infobox_list = sapply(infobox_list, function(x) x[x != ""])
    # remove nameless list elements
    infobox_list = infobox_list[names(infobox_list) != ""]
    
    return(infobox_list)
}

get_infobox("Mary, mother of Jesus", "infobox biography vcard")
get_infobox("Barack Obama") %>% format_infobox

# class bday ----
get_bday = function(page_name) {
    p_content = page_content(language = "en", project =  "wikipedia", page_name = page_name)
    p_text = p_content[["parse"]][["text"]][["*"]]
    p_parse = htmlParse(p_text, asText = TRUE, ignoreBlanks = TRUE)
    bday_NodeSet = getNodeSet(p_parse, "//span[@class = 'bday']")
    free(p_parse)
    bday = xmlValue(bday_NodeSet[[1]])
    return(bday)
}

get_bday("Elizabeth II")
