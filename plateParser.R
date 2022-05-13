source("~/Documents/Fran/analise_victor.R")

arquivos_victor <- list.files(".", pattern = ".txt", full.names = T)
arquivos_mapas <- list.files(".", pattern = ".xlsx", full.names = T)

nome_pasta <- basename(getwd())

plates_join <- NULL

for (id in 1:length(arquivos_mapas)) {

placa <- analisar_tecnicas(placa = arquivos_victor[id], mapa_placa = arquivos_mapas[id])
plates_join <- bind_rows(plates_join, placa)

biologicas <- plates_join %>%
                group_by(amostra, Repeat) %>%
                summarise(Media_Biologicas = mean(media), SD_Biologicas = sd(media)) %>%
                na.omit()

}

write_delim(plates_join, file = paste0(nome_pasta, "_plates_summary.tsv"), delim = "\t")
write_delim(biologicas, file = paste0(nome_pasta, "_analysis.tsv"), delim = "\t")

