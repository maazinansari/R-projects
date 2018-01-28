source("wiki.R")

president_list = page_content(language = "en",
                              project = "wikipedia",
                              page_name = "List of Presidents of the United_States")
p_parse = htmlParse(president_list, asText = TRUE, ignoreBlanks = TRUE)
president_tables = readHTMLTable(p_parse)
free(p_parse)
president_names = president_tables[[2]][-1,4] %>% na.omit
president_names = sapply(X = president_names,
                         FUN = function(x) grepRaw(pattern = "\\\\n", x = x) - 1) %>% 
                  substr(x = president_names, start = 1, stop = .)
# warning: slow!
bdays = lapply(president_names, get_bday)
bdays_date = strptime(bdays, "%Y-%m-%d")
bday_df = cbind(president_names, bdays)


