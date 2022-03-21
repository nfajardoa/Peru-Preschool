###### FIRST FILTERING (most secure)
###### Identified: 8276748 (unique) + 1606 (repeated)
###### By: ID, father's surname, mother's surname, names, birthday. 

## Step 1: Delete obs with missing ID and birthday from enrollment databases.

# 2013
tbl(db_conn, "siagie2013_raw") %>%
  filter(!is.na(fnac), !is.na(n_doc)) %>%
  group_by(n_doc, paterno, materno, nombres, fnac) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  mutate(n = n-1) %>%
  rename(translado = n) %>%
  compute(name = "siagie2013", 
          temporary = FALSE)

# 2014
tbl(db_conn, "siagie2014_raw") %>%
  filter(!is.na(fnac), !is.na(n_doc)) %>%
  group_by(n_doc, paterno, materno, nombres, fnac) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  mutate(n = n-1) %>%
  rename(translado = n) %>%
  compute(name = "siagie2014", 
          temporary = FALSE)

# 2015
tbl(db_conn, "siagie2015_raw") %>%
  filter(!is.na(fnac), !is.na(n_doc)) %>%
  group_by(n_doc, paterno, materno, nombres, fnac) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  mutate(n = n-1) %>%
  rename(translado = n) %>%
  compute(name = "siagie2015", 
          temporary = FALSE)

## Step 2: Retrieve cleaned tables without repeated values for all years.
# Variables: n_doc, paterno, materno, nombres, fnac, mujer

enrollment_2013 <- tbl(db_conn, "siagie2013") %>%
  select(n_doc, paterno, materno, nombres, fnac, mujer) %>% 
  distinct() %>%
  collect()

enrollment_2014 <- tbl(db_conn, "siagie2014") %>%
  select(n_doc, paterno, materno, nombres, fnac, mujer) %>% 
  distinct() %>%
  collect()

enrollment_2015 <- tbl(db_conn, "siagie2015") %>%
  select(n_doc, paterno, materno, nombres, fnac, mujer) %>% 
  distinct() %>%
  collect()

## Step 3: Bind rows of all enrollment tables and reduce by deleting repeated obs, then:
# 1. If ID has less than 8 ciphers, change it to missing.
# 2. Replace non-ascii characters and clean all string variables (materno, paterno, nombres)
# 3. Count by groups (paterno, materno, nombres, fnac, mujer)

enrollment <- ls(pattern = "enrollment_") %>%
  map(~ rlang::parse_expr(.x) %>% eval) %>%
  reduce(bind_rows) %>%
  distinct() %>%
  arrange() %>%
  group_by(paterno, materno, nombres, fnac, mujer) %>%
  mutate(n_doc = ifelse(n_doc < 10000000, NA, n_doc),
         materno = str_squish(materno) %>% str_trim() %>% replace_non_ascii(),
         paterno = str_squish(paterno) %>% str_trim() %>% replace_non_ascii(),
         nombres = str_squish(nombres) %>% str_trim() %>% replace_non_ascii(),
         n = n())

# Save progress:
save.image("enrollment.RData")
rm(enrollment_2013, enrollment_2014, enrollment_2015)

# Load progress: 
load("enrollment.RData")

# Step 4: Select all unique observations and write resulting table into database.

enrollment_unique <- enrollment %>%
  filter(n == 1) %>%
  select(-n) 
dbWriteTable(db_conn, name = "estudiantes", enrollment_unique)

## Step 5: Select all non-coherent ID observations, turn ID to missing and write resulting table into database.

enrollment_repeated <- enrollment %>%
  filter(n > 1) %>%
  arrange(paterno, materno, nombres) %>% 
  mutate(n_doc = NA) %>% 
  select(-n) %>% 
  distinct()

dbWriteTable(db_conn, name = "estudiantes", enrollment_repeated, append = TRUE)
