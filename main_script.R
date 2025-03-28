library(tidyverse)
library(fs)
library(writexl)

directories <- fs::dir_ls(type = "directory")

# single file reading function -------------

### Le but de cette function est de permettre de lire un fichier .txt, de le pivot pour céer une table avec
### les colonnes GH_ID, Date, Heure, Variables, Valeurs (Value), Date_du_fichier (File_Date), Station_Name, 
### Station_Type (venant du nom du fichier), Dir_Type (Type de station venant du nom de repertoire)

# -c(GH_ID, Date, Heure)

file_reader <- function(file_path, max_line = 1 ) {
  
  file_head = read_lines(file = file_path, n_max = 1)
  
  if(str_detect(file_head, ",")) {
    
    data <- read_delim(file = file_path, delim = ",", col_types = cols(.default = col_character()), 
                       locale = locale(decimal_mark = '.'), n_max = max_line, col_select = c(Date, Heure))
    
  } else if (str_detect(file_head, ";")) {
    
    data <- read_delim(file = file_path, delim = ";", col_types = cols(.default = col_character()), 
                       locale = locale(decimal_mark = '.'), n_max = max_line, col_select = c(Date, Heure))
    
  } else {
    
    data <- read_delim(file = file_path, delim = "\t", col_types = cols(.default = col_character()), 
                       locale = locale(decimal_mark = '.'), n_max = max_line, col_select = c(Date, Heure))
  }
  
  data <- data %>% 
    mutate( file_name = basename(file_path) %>% str_replace_all(pattern = "\\.txt", replacement = "") ) %>% 
    separate_wider_delim(cols = file_name, delim = "_", names = c("File_date", "Station_Name", "Station_Type"), too_many = "debug", too_few =  "debug", cols_remove = FALSE)
  
    
  return(data)
  
}




# single file columns reading functions ------------

cols_reader <- function(file_path, max_line = 1) {
  
  data <- read_delim(file = file_path, delim = ";", col_types = cols(.default = col_character()), 
                     locale = locale(decimal_mark = '.'), n_max = max_line)
  
  columns <- names(data)[4:length(names(data))]
  cols_table <- tibble(GH_ID = data$GH_ID %>% unique() %>% .[1],
                       file_name = basename(file_path),
                       col_numbers = paste0("col_", 1:length(columns)),
                       col_names = columns,
                       )
  return(cols_table)

}


# directory files reader --------------

dir_reader <- function(dir = "AERO", reader = file_reader) {
  
  file_list <- list.files(path = glue::glue("{dir}/"), pattern = '.txt$', full.names = TRUE)
  
  dir_data <- map_dfr(.x = file_list, .f = reader)
  dir_data$dir_type = str_to_lower(dir)
  
  return(dir_data)
}

# file cheicker format cheicker -------------

file_verifier <- function(file_path) {
  
  file_head <- read_lines(file = file_path, n_max = 1)
  gh_id_in = str_detect(file_head, "GH_ID")
  delim_ok = str_detect(file_head, ";")
  
  verif_table = tibble(gh_id_in = gh_id_in, delim_ok = delim_ok,
                       file_name = basename(file_path))
  return(verif_table)
  
}



## reading and cheicking all directories data from aero directory -----------

AERO_data <- directories[1] %>% map_dfr(.f = dir_reader)

## unique columns names in the data

unique_cols <- AERO_data %>% pull(var = Variables) %>% unique()
unique_cols

### detection of date and time format ---------

AERO_data %>% select(Date, Heure) %>% 
  mutate(Date_ok = str_detect(Date, pattern = "\\b\\d{4}-\\d{2}-\\d{2}\\b"),
         Heure_ok = str_detect(Heure, pattern = "\\b\\d{2}:\\d{2}\\b")) %>% 
  filter(!Date_ok | !Heure_ok)

# A tibble: 0 × 4
# ℹ 4 variables: Date <chr>, Heure <chr>, Date_ok <lgl>, Heure_ok <lgl>


### detection of the file name format  -------------

AERO_data %>% select(file_name) %>% 
  mutate(file_name_ok = str_detect(file_name, pattern = "\\b\\d{8}_[a-z]+_(pluvio|aero|agro|synop|sol)\\b")) %>% 
  filter(!file_name_ok)

# A tibble: 0 × 2
# ℹ 2 variables: file_name <chr>, file_name_ok <lgl>

### cheicking data type type (aero, synop, ect..) and directory type

AERO_data %>% select(GH_ID, Station_Name, dir_type, Station_Type) %>% 
  filter(dir_type != Station_Type)

# A tibble: 0 × 4
# ℹ 4 variables: GH_ID <chr>, Station_Name <chr>, dir_type <chr>, Station_Type <chr>

### Aero cols order cheicking

AERO_cols <- directories[1]  %>% map_dfr(.f = dir_reader, reader = cols_reader)

AERO_cols %>% group_by(GH_ID, file_name) %>% 
  summarise(n_cols = n()) %>%
  distinct(GH_ID, n_cols, .keep_all = TRUE)


# summarise()` has grouped output by 'GH_ID'. You can override using the `.groups` argument.
# A tibble: 7 × 3
# Groups:   GH_ID [7]
# GH_ID   file_name                    n_cols
# <chr>   <chr>                         <int>
# 1 200035W 20161201_ouahigouya_aero.txt     13
# 2 200054W 20160501_dedougou_aero.txt       14
# 3 200085W 20161201_bogande_aero.txt        14
# 4 200099W 20161201_bobo_aero.txt           14
# 5 200107W 20170801_boromo_aero.txt         14
# 6 200114W 20160501_po_aero.txt             14
# 7 200140W 20160901_gaoua_aero.txt          14


## reading and cheicking all directories data from pluvio directory -----------

PLUVIO_data <- "PLUVIO" %>% map_dfr(.f = dir_reader)




## File format verification ----------------

#file_format_verif_data <- directories %>% map_dfr(.f = dir_reader, reader = file_verifier)

file_format_verif_data <- file_format_verif_data %>% 
  mutate(file_name_ok = str_detect(file_name, pattern = "\\b\\d{8}_[a-z\\-]+_(pluvio|aero|agro|synop|sol)\\b"))


file_format_verif_data %>% filter(!file_name_ok) %>% 
  write_xlsx(path = "File_format_not_ok.xlsx")

file_format_verif_data %>% filter(!gh_id_in) %>% 
  write_xlsx(path = "GH_ID_not_included.xlsx")

file_format_verif_data %>% filter(!delim_ok) %>% 
  write_xlsx(path = "Delimiter_not_ok.xlsx")


## date and hours cheicking ---------------

date_hours_data <- directories %>% map_dfr(.f = dir_reader, reader = file_reader)

date_hour_format_verif <- date_hours_data %>% 
  mutate(Date_ok = str_detect(Date, pattern = "\\b\\d{4}-\\d{2}-\\d{2}\\b"),
         Heure_ok = str_detect(Heure, pattern = "\\b\\d{2}:\\d{2}\\b"))

date_hour_format_verif %>% filter(!Date_ok) %>% 
  write_xlsx(path = "Date_format_not_ok.xlsx")

date_hour_format_verif %>% filter(!Heure_ok) %>% 
  write_xlsx(path = "Hours_format_not_ok.xlsx")


## columns order cheicking  -------------

columns_order_data <- directories %>% map_dfr(.f = dir_reader, reader = cols_reader)

cols_reader(file_path = "AERO/20160501_dedougou_aero.txt")
