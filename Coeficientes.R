
# Cargar librerías

library(readxl)
library(dplyr)

# Leer datos desde un archivo xlsx

datos <- read_excel("Bases/SAIC_2003.xlsx")

# Crear vector subsec_mun

subsec_mun <- datos %>% group_by(cve_geo, cve_sub, cve_zm, nom_zm) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                                        re = sum(re, na.rm = TRUE),
                                                                        pb = sum(as.numeric(pb), na.rm = TRUE), 
                                                                        va = sum(as.numeric(va), na.rm = TRUE),
                                                                        fb = sum(as.numeric(fb), na.rm = TRUE),
                                                                        af = sum(as.numeric(af), na.rm = TRUE),
                                                                        po = sum(as.numeric(po), na.rm = TRUE),
                                                                        po_m = sum(po_m, na.rm = TRUE), 
                                                                        po_h = sum(po_h, na.rm = TRUE),  
                                                                        pre = sum(pre, na.rm = TRUE),
                                                                        pre_m = sum(pre_m, na.rm = TRUE),  
                                                                        pre_h = sum(pre_h, na.rm = TRUE), 
                                                                        pho = sum(pho, na.rm = TRUE), 
                                                                        pho_m = sum(pho_m, na.rm = TRUE), 
                                                                        pho_h = sum(pho_h, na.rm = TRUE))
# Crear vector tot_mun

tot_mun <- datos %>% group_by(cve_geo, cve_zm, nom_zm) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                                     re = sum(re, na.rm = TRUE),
                                                                     pb = sum(as.numeric(pb), na.rm = TRUE), 
                                                                     va = sum(as.numeric(va), na.rm = TRUE),
                                                                     fb = sum(as.numeric(fb), na.rm = TRUE),
                                                                     af = sum(as.numeric(af), na.rm = TRUE),
                                                                     po = sum(as.numeric(po), na.rm = TRUE),
                                                                     po_m = sum(po_m, na.rm = TRUE), 
                                                                     po_h = sum(po_h, na.rm = TRUE),  
                                                                     pre = sum(pre, na.rm = TRUE),
                                                                     pre_m = sum(pre_m, na.rm = TRUE),  
                                                                     pre_h = sum(pre_h, na.rm = TRUE), 
                                                                     pho = sum(pho, na.rm = TRUE), 
                                                                     pho_m = sum(pho_m, na.rm = TRUE), 
                                                                     pho_h = sum(pho_h, na.rm = TRUE))
View(tot_mun)

# Crear vector subsec_zm

subsec_zm <- datos %>% group_by(cve_zm, nom_zm, cve_sub) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                                       re = sum(re, na.rm = TRUE),
                                                                       pb = sum(as.numeric(pb), na.rm = TRUE), 
                                                                       va = sum(as.numeric(va), na.rm = TRUE),
                                                                       fb = sum(as.numeric(fb), na.rm = TRUE),
                                                                       af = sum(as.numeric(af), na.rm = TRUE),
                                                                       po = sum(as.numeric(po), na.rm = TRUE),
                                                                       po_m = sum(po_m, na.rm = TRUE), 
                                                                       po_h = sum(po_h, na.rm = TRUE),  
                                                                       pre = sum(pre, na.rm = TRUE),
                                                                       pre_m = sum(pre_m, na.rm = TRUE),  
                                                                       pre_h = sum(pre_h, na.rm = TRUE), 
                                                                       pho = sum(pho, na.rm = TRUE), 
                                                                       pho_m = sum(pho_m, na.rm = TRUE), 
                                                                       pho_h = sum(pho_h, na.rm = TRUE))
View(subsec_zm)

# Crear vector tot_zm

tot_zm <- datos %>% group_by(cve_zm, nom_zm) %>% summarize(ue = sum(ue, na.rm = TRUE), 
                                                           re = sum(re, na.rm = TRUE),
                                                           pb = sum(as.numeric(pb), na.rm = TRUE), 
                                                           va = sum(as.numeric(va), na.rm = TRUE),
                                                           fb = sum(as.numeric(fb), na.rm = TRUE),
                                                           af = sum(as.numeric(af), na.rm = TRUE),
                                                           po = sum(as.numeric(po), na.rm = TRUE),
                                                           po_m = sum(po_m, na.rm = TRUE), 
                                                           po_h = sum(po_h, na.rm = TRUE),  
                                                           pre = sum(pre, na.rm = TRUE),
                                                           pre_m = sum(pre_m, na.rm = TRUE),  
                                                           pre_h = sum(pre_h, na.rm = TRUE), 
                                                           pho = sum(pho, na.rm = TRUE), 
                                                           pho_m = sum(pho_m, na.rm = TRUE), 
                                                           pho_h = sum(pho_h, na.rm = TRUE))




## Coeficiente de localización económica (QL)

# Numerador

subsec_mun_div <- left_join(subsec_mun, tot_mun, by = c("cve_geo" = "cve_geo", "cve_zm" = "cve_zm", "nom_zm" = "nom_zm")) %>% 
  mutate(ue = ue.x/ue.y,
         re = re.x/re.y,
         pb = pb.x/pb.y,
         va = va.x/va.y,
         fb = fb.x/fb.y,
         af = af.x/af.y,
         po = po.x/po.y,
         po_m = po_m.x/po_m.y,
         po_h = po_h.x/po_h.y,
         pre = pre.x/pre.y,
         pre_m = pre_m.x/pre_m.y,
         pre_h = pre_h.x/pre_h.y,
         pho = pho.x/pho.y,
         pho_m = pho_m.x/pho_m.y,
         pho_h = pho_h.x/pho_h.y) %>% 
  select(-ue.x, -ue.y, -po_h.x, -po_h.y, -po_m.x, -po_m.y, -re.x, -re.y, -pb.x, -pb.y, -pre.x, -pre.y,
         -pre_h.x, -pre_h.y, -pre_m.x, -pre_m.y, -pho.x, -pho.y, -pho_h.x, -pho_h.y, -pho_m.x, -pho_m.y,
         -va.x, -va.y, -fb.x, -fb.y, -af.x, -af.y, -po.x, -po.y)

View(subsec_mun_div)
# Denominador

# Unir vectores subsec_zm y tot_zm por cve_zm

subsec_tot_zm <- left_join(subsec_zm, tot_zm, by = "cve_zm", "nom_zm")

View(subsec_tot_zm)

# Dividir los valores de subsec_zm entre los valores de tot_zm por cve_zm

subsec_tot_zm_div <- subsec_tot_zm %>%
  mutate(ue.div = ue.x/ue.y,
         re.div = re.x/re.y,
         pb.div = pb.x/pb.y,
         va.div = va.x/va.y,
         fb.div = fb.x/fb.y,
         af.div = af.x/af.y,
         po.div = po.x/po.y,
         po_m.div = po_m.x/po_m.y,
         po_h.div = po_h.x/po_h.y,
         pre.div = pre.x/pre.y,
         pre_m.div = pre_m.x/pre_m.y,
         pre_h.div = pre_h.x/pre_h.y,
         pho.div = pho.x/pho.y,
         pho_m.div = pho_m.x/pho_m.y,
         pho_h.div = pho_h.x/pho_h.y) %>% 
  select(-ue.x, -ue.y, -po_h.x, -po_h.y, -po_m.x, -po_m.y, -re.x, -re.y, -pb.x, -pb.y, -pre.x, -pre.y,
         -pre_h.x, -pre_h.y, -pre_m.x, -pre_m.y, -pho.x, -pho.y, -pho_h.x, -pho_h.y, -pho_m.x, -pho_m.y,
         -va.x, -va.y, -fb.x, -fb.y, -af.x, -af.y, -po.x, -po.y)

View(subsec_tot_zm_div)

# Resultado final QL

# Unir subsec_mun_div y subsec_tot_zm_div por cve_zm y cve_sub

QL <- left_join(subsec_mun_div, subsec_tot_zm_div, by = c("cve_zm", "cve_sub"))

View(QL)

# Dividir cada variable de subsec_mun_div entre la variable correspondiente de subsec_tot_zm_div

QL <- QL %>% 
  mutate(QLue = ue / ue.div,
         QLre = re / re.div,
         QLpb = pb / pb.div,
         QLva = va / va.div,
         QLfb = fb / fb.div,
         QLaf = af / af.div,
         QLpo = po / po.div,
         QLpo_m = po_m / po_m.div,
         QLpo_h = po_h / po_h.div,
         QLpre = pre / pre.div,
         QLpre_m = pre_m / pre_m.div,
         QLpre_h = pre_h / pre_h.div,
         QLpho = pho / pho.div,
         QLpho_m = pho_m / pho_m.div,
         QLpho_h = pho_h / pho_h.div) %>% 
  select(-ue, -ue.div, -po_h, -po_h.div, -po_m, -po_m.div, -re, -re.div, -pb, -pb.div, 
         -pre, -pre.div, -pre_h, -pre_h.div, -pre_m, -pre_m.div, -pho, -pho.div, -pho_h, -pho_h.div, 
         -pho_m, -pho_m.div, -va, -va.div, -fb, -fb.div, -af, -af.div, -nom_zm.x, -nom_zm.y, -po, -po.div)

View(QL)

# Estimar coeficiente PR

PR <- subsec_mun %>% 
  left_join(subsec_zm, by = c("cve_sub", "cve_zm")) %>% 
  mutate(PRue = ue.x / ue.y,
         PRre = re.x / re.y,
         PRpb = pb.x / pb.y,
         PRva = va.x / va.y,
         PRfb = fb.x / fb.y,
         PRaf = af.x / af.y,
         PRpo = po.x / po.y,
         PRpo_m = po_m.x / po_m.y,
         PRpo_h = po_h.x / po_h.y,
         PRpre = pre.x / pre.y,
         PRpre_m = pre_m.x / pre_m.y,
         PRpre_h = pre_h.x / pre_h.y,
         PRpho = pho.x / pho.y,
         PRpho_m = pho_m.x / pho_m.y,
         PRpho_h = pho_h.x / pho_h.y) %>% 
  select(cve_geo, cve_sub, cve_zm,  PRue, PRre, PRpb, PRva, PRfb, PRaf, PRpo, PRpo_m, PRpo_h, PRpre, PRpre_m, PRpre_h, PRpho, PRpho_m, PRpho_h)

View(PR)

# Estimar coeficiente HH

# Estimar la parte que se resta

resta <- tot_mun %>% 
  left_join(tot_zm, by = c("cve_zm")) %>% 
  mutate(Rue = ue.x / ue.y,
         Rpo_h = po_h.x / po_h.y,
         Rpo_m = po_m.x / po_m.y,
         Rre = re.x / re.y,
         Rpb = pb.x / pb.y,
         Rpre = pre.x / pre.y,
         Rpre_h = pre_h.x / pre_h.y,
         Rpre_m = pre_m.x / pre_m.y,
         Rpho = pho.x / pho.y,
         Rpho_h = pho_h.x / pho_h.y,
         Rpho_m = pho_m.x / pho_m.y,
         Rva = va.x / va.y,
         Rfb = fb.x / fb.y,
         Raf = af.x / af.y,
         Rpo = po.x/po.y)%>% 
  select(cve_geo, cve_zm, Rue, Rre, Rpb, Rva, Rfb, Raf, Rpo, Rpo_m, Rpo_h, Rpre, Rpre_m, Rpre_h, Rpho, Rpho_m, Rpho_h,)


View(resta)
View(tot_mun)
View(tot_zm)
# Estimar HH

HH <- PR %>% 
  left_join(resta, by = c("cve_geo", "cve_zm")) %>% 
  mutate(HHue = PRue - Rue,
         HHpo_h = PRpo_h - Rpo_h,
         HHpo_m = PRpo_m - Rpo_m,
         HHre = PRre - Rre,
         HHpb = PRpb - Rpb,
         HHpre = PRpre - Rpre,
         HHpre_h = PRpre_h - Rpre_h,
         HHpre_m = PRpre_m - Rpre_m,
         HHpho = PRpho - Rpho,
         HHpho_h = PRpho_h - Rpho_h,
         HHpho_m = PRpho_m - Rpho_m,
         HHva = PRva - Rva,
         HHfb = PRfb - Rfb,
         HHaf = PRaf - Raf,
         HHpo = PRpo - Rpo) %>% 
  select(cve_geo, cve_sub, cve_zm, HHue, HHre, HHpb, HHva, HHfb, HHaf, HHpo, HHpo_m, HHpo_h, HHpre, HHpre_m, HHpre_h, HHpho, HHpho_m, HHpho_h)


View(HH)

# Estimar IHH

IHH <- HH %>%
  mutate_at(vars(HHue, HHre, HHpb, HHva, HHfb, HHaf, HHpo, HHpo_m, HHpo_h, HHpre, HHpre_m, HHpre_h, HHpho, HHpho_m, HHpho_h), ~ 1 - .) %>%
  rename_with(~ paste0("IHH", gsub("HH", "", .)), starts_with("HH"))


View(IHH)

# Unir datos 

BLzm99_final <- left_join(datos, QL, by = c("cve_geo", "cve_sub", "cve_zm")) %>%
  left_join(PR, by = c("cve_geo", "cve_sub", "cve_zm")) %>%
  left_join(HH, by = c("cve_geo", "cve_sub", "cve_zm")) %>%
  left_join(IHH, by = c("cve_geo", "cve_sub", "cve_zm"))%>%
  rename(nom_zm = nom_zm.x)%>%
  select(año, ent,mun, cve_geo, cve_zm, nom_zm, cve_sec, cve_sub,ae,
         ue, re, pb, va, fb, af, po, po_m, po_h, pre, pre_m, pre_h, pho, pho_m, pho_h,
         QLue, QLre, QLpb, QLva, QLfb, QLaf, QLpo, QLpo_m, QLpo_h, QLpre, QLpre_m, QLpre_h, QLpho, QLpho_m, QLpho_h,
         PRue, PRre, PRpb, PRva, PRfb, PRaf, PRpo, PRpo_m, PRpo_h, PRpre, PRpre_m, PRpre_h, PRpho, PRpho_m, PRpho_h,
         HHue, HHre, HHpb, HHva, HHfb, HHaf, HHpo, HHpo_m, HHpo_h, HHpre, HHpre_m, HHpre_h, HHpho, HHpho_m, HHpho_h,
         IHHue, IHHre, IHHpb, IHHva, IHHfb, IHHaf, IHHpo, IHHpo_m, IHHpo_h, IHHpre, IHHpre_m, IHHpre_h, IHHpho, IHHpho_m, IHHpho_h)

View(BLzm99_final)

# Guardar archivo

library(openxlsx)

write.xlsx(BLzm99_final, "BLzm03_final.xlsx")



