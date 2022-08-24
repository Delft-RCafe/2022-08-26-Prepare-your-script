#### Prep for RCafe, 26th August 2022
### Clem + Aleks

library(tidyverse)
require('XML')

# microdata catalog: https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/catalogus-microdata/bevolking


download.file(
  "https://www.cbs.nl/-/media/cbs-op-maat/microdatabestanden/documents/2022/13/gbapersoontab.pdf",
  "GBAPERSOONTAB_doc.pdf"
)

# Open the pdf to get info

vars_p <- c("RINPERSOONS", "RINPERSOON", "GBAGEBOORTELAND",
               "GBAGESLACHT", "GBAGEBOORTELANDMOEDER", "GBAGEBOORTELANDVADER",
               "GBAAANTALOUDERSBUITENLAND", "GBAHERKOMSTGROEPERING", "GBAGENERATIE",
               "GBAGEBOORTEJAAR", "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG",
               "GBAGESLACHTMOEDER", "GBAGESLACHTVADER",
               "GBAGEBOORTEJAARMOEDER", "GBAGEBOORTEMAANDMOEDER", "GBAGEBOORTEDAGMOEDER", 
               "GBAGEBOORTEJAARVADER", "GBAGEBOORTEMAANDVADER", "GBAGEBOORTEDAGVADER",
               "GBAIMPUTATIECODE", "GBAHERKOMSTLAND", "GBAGEBOORTELANDNL")

metadata_p <- data.frame(vars_p)

#From page 10: Format 
metadata_p$format <- c("A1", "A9", "A4", "A1", "A4", "A4", "A1", "A4", "A1", "A4", "A2",
                        "A2", "A1", "A1", "A4", "A2", "A2", "A4", "A2", "A2", "A1", "A4", "A1")

# infer type from format and description
metadata_p$type <- c("character", "character", "factor",
                     "factor", "factor", "factor",
                     "numeric", "factor", "numeric",
                     "numeric", "numeric", "numeric",
                     "factor", "factor",
                     "numeric", "numeric", "numeric",
                     "numeric", "numeric", "numeric",
                     "factor", "factor", "factor")

# collect info on unknown values
metadata_p$na <- c(NA, NA,NA,
                   "-", NA,NA,
                   "",NA, "-",
                  "----", "--", "--",
                  "-", "-", 
                  "----", "--", "--",
                  "----", "--", "--",
                  NA, NA, NA)

### Let's do the same for the households:
download.file("https://www.cbs.nl/-/media/cbs-op-maat/microdatabestanden/documents/2022/17/gbahuishoudensbus.pdf",
              "GBAHUISHOUDENTAB_doc.pdf"
)
vars_hh <- c("RINPERSOONS", "RINPERSOON", "DATUMAANVANGHH",
               "DATUMEINDEHH", "HUISHOUDNR", "TYPHH",
               "PLHH", "REFPERSOONHH", "AANTALPERSHH",
               "AANTALKINDHH", "AANTALOVHH", "GEBJAARJONGSTEKINDHH",
               "GEBMAANDJONGSTEKINDHH", "GEBJAAROUDSTEKINDHH","GEBMAANDOUDSTEKINDHH", "IMPUTATIECODEHH")
metadata_hh <- data.frame(vars_hh)
metadata_hh$format <- c("A1", "A9", "A8",
                               "A8", "A12", "A1",
                               "A2", "A1", "F4",
                               "F2", "F2", "A4", 
                               "A2", "A4", "A2", "A1")
metadata_hh$type <- c("character", "character", "Date",
                           "Date", "character", "factor",
                           "factor","factor", "numeric",
                           "numeric", "numeric", "numeric",
                           "numeric", "numeric", "numeric", "factor")
metadata_hh$na <- c(NA, NA,NA,
                        NA, "","",
                        "",NA, NA,
                        NA, NA, NA,
                        "--", NA, "--", NA)

# Create Fake tables
n_rows <- 10000
p_tab <- data.frame(replicate(length(vars_p),sample(0:1,n_rows,rep=TRUE)))
colnames(p_tab) <- vars_p
p_tab$RINPERSOON <- sample(0:n_rows,n_rows,rep=F)
p_tab$GBAGEBOORTEMAAND <- as.factor(sample(1:12,n_rows,rep=T))

hh_tab <- data.frame(replicate(length(vars_hh),sample(0:1,n_rows,rep=TRUE)))
colnames(hh_tab) <- vars_hh
hh_tab$RINPERSOON <- p_tab$RINPERSOON
hh_tab$TYPHH <- as.factor(sample(1:8,n_rows,rep=T))



###### Function to give the right type and NA from the metadata frames
# p <- p_tab %>% mutate_at(vars(), paste0("as.", metadata_p$type))  /!\ does not work!!
# ----> Aleks?


# Creating the sample dataset ---------------------------------------------

# Recreating  the simulating data, so that they include missing values: 
n_rows <- 10000
p_tab <- data.frame(replicate(length(vars_p),sample(0:1,n_rows,rep=TRUE)))
colnames(p_tab) <- vars_p
p_tab$RINPERSOON <- sample(0:n_rows,n_rows,rep=F)
p_tab$GBAGEBOORTEMAAND <- as.factor(sample(1:12,n_rows,rep=T))
p_tab$GBAGESLACHT <- sample(c('0','1','-'),n_rows,rep=TRUE)
p_tab$GBAAANTALOUDERSBUITENLAND <- sample(c('0','1',''),n_rows,rep=TRUE)
p_tab$GBAGENERATIE      <- sample(c('0','1','-'),n_rows,rep=TRUE)


hh_tab <- data.frame(replicate(length(vars_hh),sample(0:1,n_rows,rep=TRUE)))
colnames(hh_tab) <- vars_hh
hh_tab$RINPERSOON <- p_tab$RINPERSOON
hh_tab$TYPHH <- as.factor(sample(c(as.character(1:8),''),n_rows,rep=T))


# Replacing NAs -----------------------------------------------------------

#Function to replace the  giver variables to NAs
char_to_na <- function(x, char){
  out_x <- x
  out_x[which(out_x %in% char)] = NA
  return(out_x)
}

# Data table way
p_dtab <- as.data.table(p_tab)
metadata_pd <- as.data.table(metadata_p) 

# Method 1 
p_dtab[ , c(metadata_pd$vars_p) := mapply(function(x, char) { char_to_na(x, char)}, .SD, metadata_pd$na,  SIMPLIFY=F ), .SDcols = metadata_pd$vars_p ]

# Method 2
p_tab <- map2_dfr(p_tab,metadata_pd$na, char_to_na )
hh_tab <- map2_dfr(hh_tab,metadata_hh$na, char_to_na )



# Adapting the data type  -------------------------------------------------

funs_p <- paste0("as.", metadata_p$type)

p_tab <- as.data.frame(mapply(function(a,b) b(a), p_tab, lapply(funs_p, get), SIMPLIFY = F))
str(p_tab)

funs_hh <- paste0("as.", metadata_hh$type)

hh_tab <- as.data.frame(mapply(function(a,b) b(a), hh_tab, lapply(funs_hh, get), SIMPLIFY = F))
str(hh_tab)


###### Function to join the 2 tables
join_tabs <- function(p_tab, hh_tab){
   single_tab <- left_join(p_tab, hh_tab, by=c("RINPERSOON" = "RINPERSOON"))
   return(single_tab)
}
single_tab <- join_tabs(p_tab, hh_tab)

###### Function to summarise the households by types
summary_hh_typ <- function(single_tab){
  
  # from GBAHUISHOUDEN_doc.pdf, page 11
  levels(single_tab$TYPHH) <- c("Eenpersoonshuishouden",
                                "Niet-gehuwd paar zonder kinderen",
                                "Gehuwd paar zonder kinderen",
                                "Niet-gehuwd paar met kinderen",
                                "Gehuwd paar met kinderen",
                                "Eenouderhuishouden",
                                "Overig huishouden",
                                "Institutioneel huishouden"
                            )
  s <- summary(single_tab$TYPHH)
  return(s)
}

summary_hh_typ(single_tab)


###### Function to visualise the distribution of birthday months
viz_birth_month <- function(single_tab){
  
  levels(single_tab$GBAGEBOORTEMAAND) <- c(
    "January","February", "March", "April", 
    "May", "June", "July", "August",
    "September", "October", "November", "December"
    )
  
  q <- ggplot(data=single_tab, aes(x=GBAGEBOORTEMAAND)) + 
    geom_bar(fill="orange") + 
    ggtitle("Distribution of birthdays by month in register population") +
    coord_flip() + 
    scale_x_discrete(limits = rev(levels(single_tab$GBAGEBOORTEMAAND))) +
    theme_minimal()
  return(q)
}

viz_birth_month(single_tab)
