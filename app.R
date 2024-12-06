library(bs4Dash)
library(bslib)
library(fst)
library(data.table)
library(dplyr)
library(leaflet)
library(sf)
library(waiter)
library(collapse)

rekap_jamban <- as.data.table(read_fst("data/rekap_sasaran_jamban.fst"))
rekap_air_empat_t <- as.data.table(fst::read_fst("data/rekap_sasaran_air_empat_t.fst"))
rekap_desa_verval_krs <- as.data.table(read_fst("data/rekap_desa_verval_krs.fst"))

#sasaran_genting <- fst::read_fst("DATASET KRS SASARAN GENTING/bnba_keluarga_genting_sulawesi barat.fst")
verval_krs_peta <- as.data.table(fst::read_fst("data/verval_krs_dashboard_peta.fst"))

# # Membaca shapefile peta desa
# peta_desa_646 <- st_read("BATAS_DESA_SULBAR/BATAS_DESA__SULAWESI_BARAT.shp") %>%
#   mutate(KECAMATAN = ifelse(KECAMATAN == "SIMBORO DAN KEPULAUAN", "SIMBORO", KECAMATAN)) %>%
#   mutate(KAB_KOTA = ifelse(KAB_KOTA == "MAMUJU UTARA", "PASANGKAYU", KAB_KOTA))
# 
# saveRDS(peta_desa_646, "data/BATAS_DESA__SULAWESI_BARAT.rds")

peta_desa_646 <- readRDS("data/BATAS_DESA__SULAWESI_BARAT.rds")

ui <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    selectInput(
      "jenis_sasaran", label = "Pilih Data", choices = c("Semua", "Jamban", "Sumber Air + 4T")
    ),
    selectInput(
      "kabupaten", label = "Pilih Kabupaten", choices = NULL
    ),
    selectInput(
      "kecamatan", label = "Pilih Kecamatan", choices = NULL
    ),
    input_task_button(
      "cari", "Cari"
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    leafletOutput("map", height = "600")
  )
)

server <- function(input, output, session) {
  
  #input
  #input
  observe({
    if(input$jenis_sasaran == "Semua"){
      pilihan_kab = c("SEMUA KABUPATEN", unique(rekap_desa_verval_krs$nama_kabupaten))
    } else if(input$jenis_sasaran == "Sumber Air + 4T"){
      pilihan_kab = c("SEMUA KABUPATEN", unique(rekap_air_empat_t$nama_kabupaten))
    } else if(input$jenis_sasaran == "Jamban"){
      pilihan_kab = c("SEMUA KABUPATEN", unique(rekap_jamban$nama_kabupaten))
    }
    
    updateSelectInput(session, "kabupaten",
                      choices = pilihan_kab,
                      selected = pilihan_kab[1])
  })
  
  #input
  observe({
    if(input$jenis_sasaran == "Semua"){
      dataset = rekap_desa_verval_krs
    } else if(input$jenis_sasaran == "Sumber Air + 4T"){
      dataset = rekap_air_empat_t
    } else if(input$jenis_sasaran == "Jamban"){
      dataset = rekap_jamban
    }
    
    if(input$kabupaten == "SEMUA KABUPATEN"){
      pilihan_kec = "SEMUA KECAMATAN"
    } else{
      pilihan_kec = dataset[, .(nama_kabupaten, nama_kecamatan)]
      pilihan_kec = dataset[nama_kabupaten == input$kabupaten, .(nama_kabupaten, nama_kecamatan)]
      pilihan_kec = c("SEMUA KECAMATAN", pilihan_kec$nama_kecamatan)
    }
    
    updateSelectInput(session, "kecamatan",
                      choices = pilihan_kec,
                      selected = pilihan_kec[1])
  })
  
  # Reactive untuk memilih kabupaten
  value_filter_kab <- reactiveVal()
  
  observeEvent(input$cari, {
    if(input$jenis_sasaran == "Semua"){
      dataset = rekap_desa_verval_krs
    } else if(input$jenis_sasaran == "Sumber Air + 4T"){
      dataset = rekap_air_empat_t
    } else if(input$jenis_sasaran == "Jamban"){
      dataset = rekap_jamban
    }
    dataset = as.data.table(dataset)
    kondisi_input = input$kabupaten
    
    # Cek apakah yang dipilih adalah 'SEMUA KABUPATEN' atau satu kabupaten tertentu
    if (kondisi_input == "SEMUA KABUPATEN") {
      # Tidak perlu menggunakan unique setiap kali, karena sudah disimpan sebelumnya
      filter_kabupaten <- unique(dataset$nama_kabupaten)
    } else {
      filter_kabupaten <- kondisi_input
    }
    
    # Set nilai reaktif
    value_filter_kab(filter_kabupaten)
  })
  
  # Reactive untuk mendapatkan daftar kecamatan per kabupaten
  daftar_kecamatan_reaktif <- reactive({
    if(input$jenis_sasaran == "Semua"){
      dataset = rekap_desa_verval_krs
    } else if(input$jenis_sasaran == "Sumber Air + 4T"){
      dataset = rekap_air_empat_t
    } else if(input$jenis_sasaran == "Jamban"){
      dataset = rekap_jamban
    }
    dataset = as.data.table(dataset)
    filter_kabupaten = value_filter_kab()  # Mengambil filter kabupaten yang telah diset
    
    # Jika filter kabupaten ada, ambil kecamatan yang sesuai
    if (length(filter_kabupaten) > 0) {
      # Menggunakan data.table untuk filter dan mengambil kecamatan unik
      daftar_kecamatan = dataset[nama_kabupaten %in% filter_kabupaten, unique(nama_kecamatan)]
    } else {
      daftar_kecamatan = character(0)  # Mengembalikan kecamatan kosong jika tidak ada kabupaten yang dipilih
    }
    
    return(daftar_kecamatan)
  })
  
  # Reactive untuk filter kecamatan
  value_filter_kec <- reactiveVal()
  
  observeEvent(input$cari, {
    kondisi_input = input$kecamatan
    daftar_kecamatan = daftar_kecamatan_reaktif()  # Ambil daftar kecamatan berdasarkan kabupaten
    
    if (kondisi_input == "SEMUA KECAMATAN") {
      # Jika "SEMUA KECAMATAN", ambil semua kecamatan yang tersedia
      filter_kecamatan = daftar_kecamatan
    } else {
      # Jika kecamatan dipilih spesifik
      filter_kecamatan = kondisi_input
    }
    
    # Update nilai reaktif untuk kecamatan
    value_filter_kec(filter_kecamatan)
  })
  ##
  
  data_peta <- eventReactive(input$cari,{
    if(input$jenis_sasaran == "Semua"){
      dataset = as.data.table(rekap_desa_verval_krs)
      
      peta_desa_646 <- merge(peta_desa_646, dataset, 
                             by.x = c("KECAMATAN","DESA_KELUR"), by.y = c("nama_kecamatan", "nama_kelurahan"), 
                             all.x = TRUE, all.y = FALSE)
      peta_desa_646 <- peta_desa_646[, c(1:2, 7:8, 164:167)] |>
        fmutate(sasaran_prioritas_genting_1 = ifelse(is.na(sasaran_prioritas_genting_1), 0, sasaran_prioritas_genting_1))
      
    } else if(input$jenis_sasaran == "Sumber Air + 4T"){
      dataset = as.data.table(rekap_air_empat_t)
      # Menggabungkan data dengan peta desa
      peta_desa_646 <- merge(peta_desa_646, dataset, 
            by.x = c("KECAMATAN","DESA_KELUR"), by.y = c("nama_kecamatan", "nama_kelurahan"), 
            all.x = TRUE, all.y = FALSE)
      peta_desa_646 <- peta_desa_646[, c(1:2, 7:8, 164:167)] |>
        fmutate(sasaran_prioritas_genting_1 = ifelse(is.na(sasaran_prioritas_genting_1), 0, sasaran_prioritas_genting_1))
    } else if(input$jenis_sasaran == "Jamban"){
      dataset = as.data.table(rekap_jamban)
      # Menggabungkan data dengan peta desa
      peta_desa_646 <- merge(peta_desa_646, dataset, 
                             by.x = c("KECAMATAN","DESA_KELUR"), by.y = c("nama_kecamatan", "nama_kelurahan"), 
                             all.x = TRUE, all.y = FALSE)
      peta_desa_646 <- peta_desa_646[, c(1:2, 7:8, 164:167)] |>
        fmutate(sasaran_prioritas_genting_1 = ifelse(is.na(sasaran_prioritas_genting_1), 0, sasaran_prioritas_genting_1))
    }
  })
  
  data_titik <- eventReactive(input$cari,{
    if(input$jenis_sasaran == "Semua"){
      
      data_titik <- verval_krs_peta 
      
    } else if(input$jenis_sasaran == "Sumber Air + 4T"){
      data_titik <- verval_krs_peta %>%
        fsubset(
          kondisi_sumber_air_minum %in% c(2) &
          kondisi_fasilitas_bab %in% c(1) &
          terlalu_muda == 2 |
            terlalu_tua == 2 |
            terlalu_dekat == 2 |
            terlalu_banyak == 2 &
          !is.na(longitude) &
          !is.na(latitude)
        ) %>%
        fmutate(longitude = as.numeric(longitude),
               latitude = as.numeric(latitude))
    } else if(input$jenis_sasaran == "Jamban"){
      data_titik <- verval_krs_peta %>%
        fsubset(
          kondisi_sumber_air_minum == 1 &
          kondisi_fasilitas_bab == 2 &
          terlalu_muda %in% c(1,3) &
          terlalu_tua %in% c(1,3) &
          terlalu_dekat %in% c(1,3) &
          terlalu_banyak %in% c(1,3) &
          !is.na(longitude) &
          !is.na(latitude)
        ) |>
        fmutate(longitude = as.numeric(longitude),
                latitude = as.numeric(latitude))
    }
  })
  
  output$map <- renderLeaflet({
    # Tentukan palet warna berdasarkan data
    peta_desa_646 <- data_peta()
    peta_desa_646 <- peta_desa_646 |>
      fsubset(KAB_KOTA %in% value_filter_kab() & 
                           KECAMATAN %in% value_filter_kec())
    pal <- colorNumeric(
      palette = "YlOrRd",  # Palet warna (contoh: Kuning-Oranye-Merah)
      domain = peta_desa_646$sasaran_prioritas_genting_1 # Kolom data untuk diwarnai
    )
    
    labels <- sprintf(
      "Kecamatan <strong>%s</strong> <br/> Desa/Kelurahan <strong>%s</strong><br/> KRS: %g ",
      peta_desa_646$KECAMATAN, peta_desa_646$DESA_KELUR, peta_desa_646$sasaran_prioritas_genting_1
    ) %>% lapply(htmltools::HTML)
    
    data_titik <- data_titik()
    data_titik <-  data_titik%>%
      fsubset(nama_kabupaten %in% value_filter_kab() & 
               nama_kecamatan %in% value_filter_kec())
    
    # poput_titik <- sprintf(
    #   "Kecamatan <strong>%s</strong> <br/> Desa/Kelurahan <strong>%s</strong><br/> Kode: %g ",
    #   data_titik$nama_kecamatan, data_titik$nama_kecamatan_kelurahan, data_titik$kode_keluarga
    # ) %>% lapply(htmltools::HTML)
    
    leaflet(data = peta_desa_646) %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topografi") %>%
      addProviderTiles(providers$OpenStreetMap, group = "Alamat Jalan") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Citra Satelit") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo dan Jalan") %>%
      addLayersControl(
        baseGroups = c("Topografi", "Alamat Jalan", "Citra Satelit", "Topo dan Jalan"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addPolygons(
        fillColor = ~pal(sasaran_prioritas_genting_1),
        weight = 1,
        color = "black",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "red",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels
        #popup = ~paste0("Total KRS: ", round(Total, 2), "\n Desa/Kelurahan:", DESA_KELUR)
      ) %>%
      addLegend(
        pal = pal,
        values = ~sasaran_prioritas_genting_1,
        position = "bottomright",
        title = "Jumlah KRS"
      ) %>%
      addMarkers(lng = data_titik$longitude, lat = data_titik$latitude, # Longitude dan Latitude
                 popup = ~paste(
                   "<b>Kecamatan:</b>", data_titik$nama_kecamatan, "<br>",
                   "<b>Desa/Kel:</b>", data_titik$nama_kelurahan, "<br>",
                   "<b>Kode:</b>", data_titik$kode_keluarga, "<br>"
                 ), # Popup yang menampilkan nama lokasi
                 clusterOptions = markerClusterOptions() # Mengaktifkan clustering
      )
  })

}

shinyApp(ui, server)