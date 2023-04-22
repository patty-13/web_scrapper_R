############################################################################
# Title
# Authors
# Authors affiliations
# correspondence author
# correspondence authors email
# publish date
# abstract 
# keywords
# fullpaper (text format)

# take year as input
# store the links of webpages
# make a list of webpages
# traverse each webpage and then extract info and store in dataframe
# append the info to datatable.
############################################################################

install.packages("writexl")
library(XML)
library(rvest)
library(dplyr)
library(xml2)
library("writexl")



#############################################################################
web_scrapper <- function(year)
{
  year_list = c("2023"="volume=16",
              "2022"="volume=15", 
              "2021"="volume=14", 
              "2020"="volume=13",
              "2019"="volume=12",
              "2018"="volume=11",
              "2017"="volume=10",
              "2016"="volume=9",
              "2015"="volume=8",
              "2014"="volume=7",
              "2013"="volume=6",
              "2012"="volume=5",
              "2011"="volume=4",
              "2010"="volume=3",
              "2009"="volume=2",
              "2008"="volume=1")



master_link = paste(paste("https://epigeneticsandchromatin.biomedcentral.com/articles?query=&",year_list[year],sep=""),"&searchType=&tab=keyword",sep="")
page_1_link = paste(paste("/articles?query=&",year_list[year],sep=""),"&searchType=&tab=keyword",sep="")

#master_link
#read_tx = readLines(master_link, encoding ="UTF-8")

################################################################################################
#CHECKING IF THERE ARE MORE THAN ONE PAGE IN WEBPAGE
################################################################################################

# Checking how many webpages there are
page_cnt = read_html(master_link) %>%
           html_element("p.u-text-sm.u-reset-margin") %>%
           html_text2()

# taking out digits from the extracted string
res <- regmatches(page_cnt, gregexpr("[[:digit:]]+", page_cnt))
page_cnt = as.numeric(unlist(res))
total_pages = page_cnt[length(page_cnt)]

# checking if total_pages are > 1, if > 1 then storing hyperlinks of all the webpages subsequent of
# first webpage and storing all the research paper hyperlinks.
if(total_pages > 1)
{
  extra_webpages = read_html(master_link) %>%
                   html_nodes("a.c-pagination__link")%>%
                   html_attr("href")
  extra_webpage = unique(extra_webpages)
  extra_webpage = append(extra_webpage, page_1_link)
  
  extract_links <- function(links)
  {
      d_link <- paste("https://epigeneticsandchromatin.biomedcentral.com",links,sep="")
      research_paper_links = read_html(d_link) %>% 
                             html_nodes("a") %>%
                             html_attr("href")%>%
                             grep("/articles/10",.,value = TRUE)
      return(research_paper_links)
  }
  research_paper_link = lapply(extra_webpage,extract_links)
  reserach_paper_link = unlist(research_paper_link)
  research_paper_link = unique(reserach_paper_link)
  
} else {
    research_paper_links = read_html(master_link) %>% 
                           html_nodes("a") %>%
                           html_attr("href")%>%
                           grep("/articles/10",.,value = TRUE)
   research_paper_link <- unique(research_paper_links)  
}


########################################################################################
##  MAIN FUNCTION TO EXTRACT INFORMATION SUCH AS TITLE, AUTHOR, ............., FULL_PAPER
########################################################################################
extract_info <-function(link)
  {
    direct_rpl <- paste("https://epigeneticsandchromatin.biomedcentral.com",link,sep="")
    
    # Title 
    title =   read_html(direct_rpl)%>%
              html_element("h1.c-article-title") %>%
              html_text()
    #Author
    Authors = list(read_html(direct_rpl)%>%
                   html_elements("li.c-article-author-list__item")%>%
                   html_element("a")%>%
                   html_text())%>%
                   gsub("[^[:^punct:],]","",.,perl = TRUE)%>%
                   sub('.','',.)
    
    # Author_Affiliations
    Author_affiliations = list(read_html(direct_rpl)%>%
                               html_elements("p.c-article-author-affiliation__address")%>%
                               html_text())%>%
                               gsub("[^[:^punct:],]","",.,perl = TRUE)%>%
                               sub('.','',.)
    
    # Correspondence_author
    correspondence_author = list(read_html(direct_rpl)%>%
                                 html_elements("p#corresponding-author-list")%>%
                                 html_text())%>%
                                 gsub("[^[:^punct:],]","",.,perl = TRUE)%>%
                                 sub('Correspondence to\n                ','',.)
    
    
    # EMAIL
    get_Link = list(read_html(direct_rpl)%>%
                    html_elements("p#corresponding-author-list a")%>%
                    html_attr("href"))%>% gsub('mailto:',"",.,perl = TRUE)%>%
                    gsub("[^[:^punct:],@._]","",.,perl = TRUE)
    
    # publication date
    publication_date = read_html(direct_rpl)%>%
                            html_elements("ul.c-article-identifiers")%>%
                            html_element("time")%>%
                            html_text()
    # abstract 
   Abstract = read_html(direct_rpl)%>%
              html_elements("#Abs1-content.c-article-section__content")%>%
              html_element("p")%>%
              html_text()
              Abstract = ifelse(length(Abstract)==0, NA, Abstract)
    #Keywords
   keywords <-  list(read_html(direct_rpl)%>%
                html_elements("li.c-article-subject-list__subject")%>%
                html_element("span")%>%
                html_text())%>%
                gsub("[^[:^punct:],]","",.,perl = TRUE)%>%
                sub('.','',.)
   
   # FULL PAPER (divided into 4 columns to reduce the total character in each cell in excel below 32k characters)
   full_paper <- list(read_html(direct_rpl) %>%
                        html_element("article")%>%
                        html_text2())%>%
                        gsub("\\n","",.,perl = TRUE)%>%trimws()
   
   str_len = nchar(full_paper)
   print(str_len)
   half_len = str_len/6
   print(half_len)
   full_paper_part_1 = substring(full_paper, 1, as.integer(half_len)) 
   full_paper_part_2 = substring(full_paper, as.integer(half_len + 1),    as.integer( 2*half_len))
   full_paper_part_3 = substring(full_paper, as.integer(2* half_len + 1), as.integer(3 * half_len))
   full_paper_part_4 = substring(full_paper, as.integer(3* half_len + 1), as.integer(4 * half_len))
   full_paper_part_5 = substring(full_paper, as.integer(4* half_len + 1), as.integer(5 * half_len))
   full_paper_part_6 = substring(full_paper, as.integer(5* half_len + 1), str_len)
   # OUTPUT
   output<-data.frame(title,
                      Authors,
                      Author_affiliations,
                      correspondence_author,
                      get_Link,
                      publication_date,
                      Abstract,
                      keywords,
                      full_paper_part_1,
                      full_paper_part_2,
                      full_paper_part_3,
                      full_paper_part_4,
                      full_paper_part_5,
                      full_paper_part_6)
   
   names(output) <-c("title",
                     "author",
                     "author_affiliation",
                     "correspondence_author",
                     "email_id",
                     "publication_date",
                     "abstract",
                     "keywords",
                     "Full_paper_part_1",
                     "Full_paper_part_2",
                     "Full_paper_part_3",
                     "Full_paper_part_4",
                     "Full_paper_part_5",
                     "Full_paper_part_6")
   return(output)
  
}

# storing the result into products data_frame.
out_p<-lapply(research_paper_link, extract_info)
products <- do.call("rbind", out_p)

################################################## NOTE (change the path for your computer) ########################
# storing the result into excel file. 
write_xlsx(products, "C:\\Users\\Praty\\Downloads\\web_scrap1.xlsx")
####################################################################################################################

return(products)
}

#############################################################################
# ##################### # ENTER YEAR # ######################################

prodts <- web_scrapper(year <- readline(prompt = "Enter the year:"))

#############################################################################
# TO VIEW THE RESULT RUN THIS COMMAND AT LAST.
View(prodts)