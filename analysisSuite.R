library(tidyverse)
library(magrittr)
library(readxl)


# calculate technical replicate means for a single sample
CollapseTechnicalReps <- function(sample_name, plate, sample_wells, blank_wells) {

  plate %>%
    filter(Well %in% sample_wells) %>%
    group_by(Repeat) %>%
    summarise(Mean = mean(`Absorbance @ 600 (A)`)) -> sample_mean

  plate %>%
    filter(Well %in% blank_wells) %>%
    group_by(Repeat) %>%
    summarise(Mean = mean(`Absorbance @ 600 (A)`)) -> blank_mean


  sample_mean %<>% 
    mutate(Mean = Mean - blank_mean$Mean,
           Sample = sample_name) %>% 
    relocate(Sample, .before = Mean)


  return(sample_mean)
}

# reads a plate map from a file 
plateParser<- function(plate_file, plate_map) {

  # Abrir os arquivos que saíram do vetor as linhas da tabela "comprida":
  plate_rawdata <- suppressWarnings(read_delim(file = plate_file, delim = "\t", show_col_types = FALSE))

  #removes as linhas indesejadas a partir da que é diferente do esperado
  maxline <- problems(plate_rawdata) %>%
                        pull(row) %>%
                        min() - 1
  
  plate_rawdata %<>% filter(row_number() < maxline)
  
   # ler o excel com o desenho da placa e criar variáveis
  map <- suppressMessages(read_excel(plate_map)) %>%
    column_to_rownames("...1")

  map_long <- map_df(.x = 1:nrow(map), ~ {
    data <- map[.x,] %>%
      pivot_longer(cols = 1:12,
                   values_to = "samples",
                   names_to = "names") %>%
      mutate(names = paste0(rownames(map)[.x], names) %>% str_remove_all('\"'))
    return(data)
  })

  unique_samples <- map_long %>% select(samples) %>% unique() %>% pull()

sample_to_well <- NULL

  for (sample in unique_samples) {
    wells <- map_long %>%
      filter(samples == sample) %>%
      select(names) %>%
      pull
    
    subset <- tibble(sample = sample, wells = list(wells))
    
    sample_to_well <- bind_rows(sample_to_well, subset)
  }


  all_blanks <- unique_samples %>% na.omit() %>% str_subset("[Bb]lank_", negate = FALSE)
  all_samples <- unique_samples %>% na.omit() %>% str_subset("[Bb]lank_", negate = TRUE)

  map_parsed <- str_split(all_samples, "_", simplify = TRUE) %>%
    set_colnames(c("sample", "condition")) %>%
    as_tibble() %>%
    mutate(blank = map_chr(.x = .$condition, ~ {
                    blank <- str_subset(string = all_blanks, pattern = .x)
                    return(blank)}),
           variable = str_c(sample, condition, sep = "_"))
  
 map_parsed <- left_join(x = map_parsed, y = sample_to_well, by = c("variable" = "sample")) %>% 
                rename(sample_wells = wells) %>%
               left_join(y = sample_to_well, by = c("blank" = "sample")) %>%
                rename(blank_wells = wells)

 # determine the plate name based on the path to the map files
 platename <- str_extract(plate_map, pattern = "(?<=/).+(?=.xlsx)") %>% 
                  str_replace_all(pattern = "/", replacement = "_")
 
 
 message(
   paste0("Finished parsing plate ",
          platename,
          "!\nFound ",
          length(all_samples),
          " unique samples")
    )

  output <- lst(platename = platename,
                rawdata = plate_rawdata,
                map_parsed = map_parsed
                )
  
  return(output)
}
  
# calculates all of the technical replicate means
technicalReps <- function(plate_file, plate_map) {

  data <- plateParser(plate_file, plate_map)

  collapsedTechicalReps <- NULL

  for (sample in 1:nrow(data$map_parsed)) {
    
    wells <- data$map_parsed$sample_wells[[sample]]
    blank <- data$map_parsed$blank_wells[[sample]]
     name <- data$map_parsed$variable[sample]
     
    collapse <- CollapseTechnicalReps(
                               sample_name = name,
                               plate = data$rawdata,
                               sample_wells = wells,
                               blank_wells =  blank
                               )
    collapsedTechicalReps <- bind_rows(collapsedTechicalReps, collapse)

  }
  
  data$collapsedTechicalReps <- collapsedTechicalReps
  
  return(data)
}
