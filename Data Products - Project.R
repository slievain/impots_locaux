library(openxlsx)
library(RColorBrewer)
library(leaflet)
library(raster)

inputs <- list(
        'Ain'='AIN', 'Aisne'='AISNE', 'Allier'='ALLIER', 'Alpes De Hte-Provence'='ALPES-DE-HTE-PROVENCE', 'Alpes-Maritimes'='ALPES-MARITIMES', 'Ardèche'='ARDECHE', 'Ardennes'='ARDENNES', 'Ariège'='ARIEGE', 'Aube'='AUBE', 'Aude'='AUDE', 'Aveyron'='AVEYRON',
        'Bas-Rhin'='BAS-RHIN', 'Bouches-Du-Rhône'='BOUCHES-DU-RHONE', 'Calvados'='CALVADOS', 'Cantal'='CANTAL', 'Charente'='CHARENTE', 'Charente-Maritime'='CHARENTE-MARITIME', 'Cher'='CHER', 'Corrèze'='CORREZE', 'Corse Du Sud'='CORSE-DU-SUD', 'Côte-D\'Or'='COTE-D-OR',
        'Côtes-D\'Armor'='COTES-D-ARMOR', 'Creuse'='CREUSE', 'Deux-Sèvres'='DEUX-SEVRES', 'Dordogne'='DORDOGNE', 'Doubs'='DOUBS', 'Drôme'='DROME', 'Essonne'='ESSONNE', 'Eure'='EURE', 'Eure-Et-Loir'='EURE-ET-LOIR', 'Finistère'='FINISTERE',
        'Gard'='GARD', 'Gers'='GERS', 'Gironde'='GIRONDE', 'Haute-Corse'='HTE-CORSE', 'Haute-Garonne'='HTE-GARONNE', 'Haute-Loire'='HTE-LOIRE', 'Haute-Marne'='HTE-MARNE', 'Hautes-Alpes'='HTES-ALPES', 'Haute-Saône'='HTE-SAONE', 'Haute-Savoie'='HTE-SAVOIE',
        'Hautes-Pyrénées'='HTES-PYRENEES', 'Haute-Vienne'='HTE-VIENNE', 'Haut-Rhin'='HT-RHIN', 'Hauts-De-Seine'='HTS-DE-SEINE', 'Hérault'='HERAULT', 'Ille-Et-Vilaine'='ILLE-ET-VILAINE', 'Indre'='INDRE', 'Indre-Et-Loire'='INDRE-ET-LOIRE', 'Isère'='ISERE', 'Jura'='JURA',
        'Landes'='LANDES', 'Loire'='LOIRE', 'Loire-Atlantique'='LOIRE-ATLANTIQUE', 'Loiret'='LOIRET', 'Loir-Et-Cher'='LOIR-ET-CHER', 'Lot'='LOT', 'Lot-Et-Garonne'='LOT-ET-GARONNE', 'Lozère'='LOZERE', 'Maine-Et-Loire'='MAINE-ET-LOIRE', 'Manche'='MANCHE',
        'Marne'='MARNE', 'Mayenne'='MAYENNE', 'Meurthe-Et-Moselle'='MEURTHE-ET-MOSELLE', 'Meuse'='MEUSE', 'Morbihan'='MORBIHAN', 'Moselle'='MOSELLE', 'Nièvre'='NIEVRE', 'Nord'='NORD', 'Oise'='OISE', 'Orne'='ORNE',
        'Paris'='PARIS', 'Pas-De-Calais'='PAS-DE-CALAIS', 'Puy-De-Dôme'='PUY-DE-DOME', 'Pyrénées-Atlantiques'='PYRENEES-ATLANTIQUES', 'Pyrénées-Orientales'='PYRENEES-ORIENTALES', 'Rhône'='RHONE', 'Saône-Et-Loire'='SAONE-ET-LOIRE', 'Sarthe'='SARTHE', 'Savoie'='SAVOIE', 'Seine-Et-Marne'='SEINE-ET-MARNE',
        'Seine-Maritime'='SEINE-MARITIME', 'Seine-Saint-Denis'='SEINE-ST-DENIS', 'Somme'='SOMME', 'Tarn'='TARN', 'Tarn-Et-Garonne'='TARN-ET-GARONNE', 'Territoire-De-Belfort'='TERRITOIRE-DE-BELFORT', 'Val D\'Oise'='VAL-D-OISE', 'Val-De-Marne'='VAL-DE-MARNE', 'Var'='VAR', 'Vaucluse'='VAUCLUSE',
        'Vendée'='VENDEE', 'Vienne'='VIENNE', 'Vosges'='VOSGES', 'Yonne'='YONNE', 'Yvelines'='YVELINES')

recipient <- list("City" = "city",
                  "Federation" = "federation",
                  "Intercommunal" = "intercommunal",
                  "Special Equipment Taxes" = "set",
                  "Total" = "total")
type <- list("Absolute" = "abs",
             "Quantiles" = "quantiles")

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

removeAccents <- function(names) {
    unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E', 'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U', 'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c', 'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o', 'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
    
    chartr(
        paste(names(unwanted_array), collapse = ''),
        paste(unwanted_array, collapse = ''),
        names)
}

cleanNames <- function(names) {
    names %>% 
        trim() %>%
        toupper() %>%
        gsub(pattern = " ", replacement = "-") %>%
        gsub(pattern = "'", replacement = "-") %>%
        gsub(pattern = "SAINT", replacement = "ST") %>%
        gsub(pattern = "SAINTE", replacement = "STE") %>%
        gsub(pattern = "HAUTE", replacement = "HTE") %>%
        gsub(pattern = "HAUT", replacement = "HT") %>%
        gsub(pattern = "Œ", replacement = "OE") %>%
        removeAccents()
}

matchDept <- function(dpt, adm_sub, recip) {
    filename <- list.files("data/", paste("^", dpt, "_fichedescriptive_*", sep = ""))
    path <- paste("data/", filename, sep = "")
    impots <- read.xlsx(path, startRow = 4)
    for (i in 1:ncol(impots)) {
        impots[impots[, i] %in% c("- ", "    .     ", "      .   "), i] <- NA
    }
    names(impots) <- c("Code", "Commune", "City_Base.nette", "City_Taux", "City_Produit", "Fed_Base.nette", "Fed_Taux", "Fed_Produit", "Ic_Base.nette", "Ic_Taux", "Ic_Produit", "Set_Base.nette", "Set_Taux", "Set_Produit")
    impots$City_Produit <- as.numeric(impots$City_Produit)
    impots$Fed_Produit <- as.numeric(impots$Fed_Produit)
    impots$Ic_Produit <- as.numeric(impots$Ic_Produit)
    impots$Set_Produit <- as.numeric(impots$Set_Produit)
    
    # Prepare commune matching
    adm_sub@data$NAME_5 <- cleanNames(adm_sub@data$NAME_5)
    impots$Commune <- cleanNames(impots$Commune)
    
    #matching commune
    adm_sub@data$Value <- as.numeric(sapply(adm_sub@data$NAME_5, function(name, dpt, recip) {
        if(dpt == "PARIS") {
            comm <- dpt
        } else {
            comm <- name
        }
        value <- NA
        switch(recip, 
            city = {
                value <- impots[impots$Commune == comm, ]$City_Produit 
            },
            federation = {
                value <- impots[impots$Commune == comm, ]$Fed_Produit
            },
            intercommunal = {
                value <- impots[impots$Commune == comm, ]$Ic_Produit
            },
            set = {
                value <- impots[impots$Commune == comm, ]$Set_Produit
            },
            {
                sum <- rowSums(cbind(
                    impots[impots$Commune == comm, ]$City_Produit,
                    impots[impots$Commune == comm, ]$Fed_Produit,
                    impots[impots$Commune == comm, ]$Ic_Produit, 
                    impots[impots$Commune == comm, ]$Set_Produit), na.rm = TRUE)
                value <- if(length(sum) == 0) NA else sum
            }
        )
        value
          
    }, dpt, recip))
    
    adm_sub
}

displayDept <- function(dpt.code, recip = "city", palette = "quantiles") {
    dpt.name = names(inputs)[inputs == dpt.code]
    
    adm <- getData('GADM', country='FRA', level=5)
    adm@data$Commune <- adm@data$NAME_5
        
    # Prepare department matching with input
    adm@data$NAME_2 <- cleanNames(adm@data$NAME_2)
    adm@data$Value <- NA
    
    matchedDpt <- matchDept(dpt.code, adm[adm@data$NAME_2 == dpt.code, ], recip)
    
    mismatch_rate <- round(100 * length(matchedDpt@data[is.na(matchedDpt@data$Value), ]$NAME_5) / length(matchedDpt@data$NAME_5), 0)
    data.frame("dpt" = dpt.name, "rate" = mismatch_rate)
    
    #display
    title = paste0("Council Taxes - ", names(recipient)[recipient == recip], " (", names(type)[type == palette], ")")
    popupLabel = paste0("<strong>", matchedDpt@data$Commune, "</strong> : ", format(round(matchedDpt@data$Value, 0), big.mark=","), " €")
        
    palette_rev <- rev(brewer.pal(6, "RdYlGn"))
    if(palette == "abs") {
        pal <- colorNumeric(palette = palette_rev , domain = matchedDpt@data$Value)
        
        leaflet() %>% 
            addTiles() %>% 
            addPolygons(data = matchedDpt, weight = 0, fillColor = ~pal(matchedDpt@data$Value), fillOpacity = 0.7, popup = popupLabel) %>%
            addLegend("bottomleft", pal = pal, values = matchedDpt@data$Value, title = title, opacity = 1, labFormat = labelFormat(suffix = " €"))
    } else {
        pal <- colorQuantile(palette = palette_rev , domain = matchedDpt@data$Value)
        
        leaflet() %>% 
            addTiles() %>% 
            addPolygons(data = matchedDpt, weight = 0, fillColor = ~pal(matchedDpt@data$Value), fillOpacity = 0.7, popup = popupLabel) %>%
            addLegend("bottomleft", pal = pal, values = matchedDpt@data$Value, title = title, opacity = 1)
    }
}

displayAllDepts <- function(recip = "city", palette = "quantiles") {
    adm <- getData('GADM', country='FRA', level=5)
    adm@data$Commune <- adm@data$NAME_5
    
    # Prepare department matching with input
    adm@data$NAME_2 <- cleanNames(adm@data$NAME_2)
    adm@data$Value <- NA
    
    mismatch <- data.frame("dpt" = character(), "rate" = numeric(), stringsAsFactors = FALSE)

    for(input in inputs) {
        dpt.code = input
        dpt.name = names(inputs)[inputs == dpt.code]
        
        adm@data[adm@data$NAME_2 == dpt.code, ] <- matchDept(dpt.code, adm[adm@data$NAME_2 == dpt.code, ], recip)@data
        
        mismatch_rate <- round(100 * length(adm@data[adm@data$NAME_2 == dpt.code & is.na(adm@data$Value), ]$NAME_5) / length(adm@data[adm@data$NAME_2 == dpt.code, ]$NAME_5), 0)
        mismatch <- rbind(mismatch, data.frame("dpt" = dpt.name, "rate" = mismatch_rate))
        
    }
    
    print(mismatch[order(mismatch$rate), ])
    
    #display
    title = paste0("Council Taxes - ", names(recipient)[recipient == recip], " (", names(type)[type == palette], ")")
    popupLabel = paste0("<strong>", adm@data$Commune, "</strong> : ", format(round(adm@data$Value, 0), big.mark=","), " €")
    
    palette_rev <- rev(brewer.pal(6, "RdYlGn"))
    if(palette == "abs") {
        pal <- colorNumeric(palette = palette_rev , domain = adm@data$Value)
        
        leaflet() %>% 
            addTiles() %>% 
            addPolygons(data = adm, weight = 0, fillColor = ~pal(adm@data$Value), fillOpacity = 0.7, popup = popupLabel) %>%
            addLegend("bottomleft", pal = pal, values = adm@data$Value, title = title, opacity = 1, labFormat = labelFormat(suffix = " €"))
    } else {
        pal <- colorQuantile(palette = palette_rev , domain = adm@data$Value)
        
        leaflet() %>% 
            addTiles() %>% 
            addPolygons(data = adm, weight = 0, fillColor = ~pal(adm@data$Value), fillOpacity = 0.7, popup = popupLabel) %>%
            addLegend("bottomleft", pal = pal, values = adm@data$Value, title = title, opacity = 1)
    }
}