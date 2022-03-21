labels <- read_dta("sit_final_cons_2015.dta", n_max = 0) %>%
  select(where(is.labelled)) %>%
  map2(names(.), print_labels)

# $grado
# <labelled<double>[0]>
#   
#   Labels:
#   value                label
# 2        0 a 2 a\xf1os
# 3            3 a\xf1os
# 4            4 a\xf1os
# 5            5 a\xf1os
# 6   Primero - Primaria
# 7   Segundo - Primaria
# 8   Tercero - Primaria
# 9    Cuarto - Primaria
# 10    Quinto - Primaria
# 11     Sexto - Primaria
# 12 Primero - Secundaria
# 13 Segundo - Secundaria
# 14 Tercero - Secundaria
# 15  Cuarto - Secundaria
# 16  Quinto - Secundaria
# 
# $estado_mat
# <labelled<double>[0]>
#   
#   Labels:
#   value        label
# 1   DEFINITIVA
# 2   TRASLADADO
# 3   EN PROCESO
# 4 PREMATRICULA
# 5      ANULADO
# 
# $sit_mat
# <labelled<double>[0]>
#   
#   Labels:
#   value        label
# 1    PROMOVIDO
# 2       REPITE
# 3   INGRESANTE
# 4   REENTRANTE
# 5 REINGRESANTE
# 
# $sit_final
# <labelled<double>[0]>
#   
#   Labels:
#   value                            label
# 1                         Aprobado
# 2                      Desaprobado
# 3                       Fallecidos
# 4                     Matriculados
# 5 Postergaci\xf3n de Evaluaci\xf3n
# 6 Requieren Recuperaci\xf3n Pedag.
# 7                         Retirado
# 8               Sin Evaluar Callao
# 9                       Trasladado
# 
# $sexo
# <labelled<double>[0]>
#   
#   Labels:
#   value label
# 1     M
# 2     V
# 
# $validado_reniec
# <labelled<double>[0]>
#   
#   Labels:
#   value              label
# 0 NO VALIDADO RENIEC
# 1    VALIDADO RENIEC
# 9            SIN DNI
# 
# $discapacidad
# <labelled<double>[0]>
#   
#   Labels:
#   value label
# 1    AU
# 2    DA
# 3    DI
# 4    DM
# 5    DT
# 6    DV
# 7    OT
# 8     P
# 9     S
# 10    SC
# 11    SI
# 
# $lengua_materna
# <labelled<double>[0]>
#   
#   Labels:
#   value              label
# 0            NINGUNO
# 1         CASTELLANO
# 2            QUECHUA
# 3             AIMARA
# 4          ASHANINKA
# 5          AWAJ\xdaN
# 6     SHIPIBO-KONIBO
# 7       MURUI-MUINAI
# 8   KUKAMA-KUKAMIRIA
# 9         MATSIGENKA
# 10               YINE
# 11         CASHINAHUA
# 12          KAKATAIBO
# 13          YAMINAHUA
# 14             CULINA
# 15         SHARANAHUA
# 16         MASTANAHUA
# 17           AMAHUACA
# 18             WAMPIS
# 19             ACHUAR
# 20               BORA
# 21              SHAWI
# 22            YANESHA
# 23            ARABELA
# 24          CAPANAHUA
# 25 KAKINTE (CAQUINTE)
# 26             CAUQUI
# 27          CHAMICURO
# 28          CHOL\xd3N
# 29            ESE EJA
# 30           HARAKBUT
# 31             IQUITU
# 32          ISCONAHUA
# 33             JAQARU
# 34            SHIWILU
# 35     KANDOZI-CHAPRA
# 36          MARINAHUA
# 37             MATSES
# 38            MOCHICA
# 39            MUNICHE
# 40              NANTI
# 41       NOMATSIGENGA
# 42             OCAINA
# 43             OMAGUA
# 44             OREJON
# 45               REMO
# 46        RES\xcdGARO
# 47             SECOYA
# 48           TAUSHIRO
# 49    TIKUNA (TICUNA)
# 50            URARINA
# 51              YAGUA
# 59         I\xd1APARI
# 60    MADIJA (CULINA)
# 61            MAIJUNA
# 62       YORA (NAHUA)
# 63             HEBREO
# 90             INGLES
# 91            FRANCES
# 92           ITALIANO
# 93    OTRO EXTRANJERO
# 94      OTRO NACIONAL
# 95          ALEM\xc1N
# 96         JAPON\xc9S
# 97              CHINO
# 98       PORTUGU\xc9S

labels <- read_dta("barrido_censal.dta", n_max = 0) %>%
  select(where(is.labelled)) %>%
  map2(names(.), print_labels)

# Labels:ptesJefe
# value                label
# 1                 Jefe
# 2              Cónyuge
# 3               Hijo/a
# 4          Yerno/nuera
# 5              Nieto/a
# 6       Padres/suegros
# 7            Hermano/a
# 8 Trabajador del hogar
# 9          Pensionista
# 10      Otros parientes
# 11   Otros no parientes
# 
# Labels:sexo
# value  label
# 1 Hombre
# 2  Mujer
# 
# Labels:estCivil
# value        label
# 1    Soltero/a
# 2     Casado/a
# 3  Conviviente
# 4   Separado/a
# 5 Divorciado/a
# 6      Viudo/a
# 
# Labels:seguro1
# value   label
# 0    Pase
# 1 Essalud
# 
# Labels:seguro2
# value        label
# 0         Pase
# 1 FF.A.-P.N.P.
# 
# Labels:seguro3
# value          label
# 0           Pase
# 1 Seguro privado
# 
# Labels:seguro4
# value                          label
# 0                           Pase
# 1 Seguro integral de salud (SIS)
# 
# Labels:seguro5
# value label
# 0  Pase
# 1  Otro
# 
# Labels:seguro6
# value    label
# 0     Pase
# 1 No tiene
# 
# Labels:alfab
# value label
# 1    Sí
# 2    No
# 
# Labels:eduNiv
# value                     label
# 1                   Ninguno
# 2                   Inicial
# 3                  Primaria
# 4                Secundaria
# 5 Superior no universitaria
# 6    Superior universitaria
# 7   Posgrado u otro similar
# 
# Labels:ocu
# value                               label
# 1              Trabajador dependiente
# 2            Trabajador independiente
# 3                           Empleador
# 4                Trabajador del hogar
# 5   Trabajador familiar no remunerado
# 6                         Desempleado
# 7 Dedicado a los quehaceres del hogar
# 8                          Estudiante
# 9                            Jubilado
# 10                       Sin actividad
# 
# Labels:sector
# value         label
# 1      Agrícola
# 2      Pecuaria
# 3      Forestal
# 4      Pesquera
# 5        Minera
# 6     Artesanal
# 7     Comercial
# 8     Servicios
# 9         Otros
# 10 Estado (Gob.)
# 
# Labels:prog1
# value         label
# 0          Pase
# 1 Vaso de leche
# 
# Labels:prog2
# value           label
# 0            Pase
# 1 Comedor popular
# 
# Labels:prog3
# value                       label
# 0                        Pase
# 1 Desayuno o almuerzo escolar
# 
# Labels:prog4
# value                      label
# 0                       Pase
# 1 Papilla o 'Yapita' (PACFO)
# 
# Labels:prog5
# value                        label
# 0                         Pase
# 1 Canasta alimentaria (PANFAR)
# 
# Labels:prog6
# value  label
# 0   Pase
# 1 Juntos
# 
# Labels:prog7
# value                      label
# 0                       Pase
# 1 Techo propio o Mi vivienda
# 
# Labels:prog8
# value label
# 0  Pase
# 1 Otros
# 
# Labels:prog9
# value   label
# 0    Pase
# 1 Ninguno
# 
# Labels:matPared
# value                             label
# 1      Ladrillo o bloque de cemento
# 2 Piedra o sillar con cal o cemento
# 3                     Adobe o tapia
# 4          Quincha (Caña con barro)
# 5                  Piedra con barro
# 6                            Madera
# 7                            Estera
# 8                              Otro
# 
# Labels:matTecho
# value                                              label
# 1                                    Concreto armado
# 2                                             Madera
# 3                                              Tejas
# 4 Planchas de calamina, fibra de cemento o similares
# 5                   Caña o estera con torta de barro
# 6                                             Estera
# 7                              Paja, hoja de palmera
# 8                                               Otro
# 
# Labels:matPiso
# value                                     label
# 1                   Parquet o madera pulida
# 2 Láminas asfálticas, vinílicos o similares
# 3             Losetas, terrazos o similares
# 4                       Madera (Entablados)
# 5                                   Cemento
# 6                                    Tierra
# 7                                      Otro
# 
# Labels:alumbrado
# value                       label
# 1                Electricidad
# 2 Kerosene (Mechero/Lamparín)
# 3      Petróleo/Gas (Lámpara)
# 4                        Vela
# 5                        Otro
# 6                    No tiene
# 
# Labels:agua
# value                                                      label
# 1                          Red pública dentro de la vivienda
# 2 Red pública fuera de la vivienda, pero dentro del edificio
# 3                                       Pilón de uso público
# 4                             Camión-cisterna u otro similar
# 5                                                       Pozo
# 6                          Río, acequia, manantial o similar
# 7                                                       Otro
# 
# Labels:serHig
# value                                                      label
# 1                          Red pública dentro de la vivienda
# 2 Red pública fuera de la vivienda, pero dentro del edificio
# 3                                               Pozo séptico
# 4                                 Pozo ciego o negro/letrina
# 5                                       Río, acequia o canal
# 6                                                   No tiene
# 
# Labels:combust
# value             label
# 1      Electricidad
# 2               Gas
# 3          Kerosene
# 4            Carbón
# 5              Leña
# 6 Bosta o estiércol
# 7              Otro
# 8         No cocina
# 
# Labels:bien1
# value            label
# 0             Pase
# 1 Equipo de sonido
# 
# Labels:bien2
# value             label
# 0              Pase
# 1 Televisor a color
# 
# Labels:bien3
# value label
# 0  Pase
# 1   DVD
# 
# Labels:bien4
# value     label
# 0      Pase
# 1 Licuadora
# 
# Labels:bien5
# value                     label
# 0                      Pase
# 1 Refrigeradora/congeladora
# 
# Labels:bien6
# value        label
# 0         Pase
# 1 Cocina a gas
# 
# Labels:bien7
# value         label
# 0          Pase
# 1 Teléfono fijo
# 
# Labels:bien8
# value             label
# 0              Pase
# 1 Plancha eléctrica
# 
# Labels:bien9
# value    label
# 0     Pase
# 1 Lavadora
# 
# Labels:bien10
# value       label
# 0        Pase
# 1 Computadora
# 
# Labels:bien11
# value            label
# 0             Pase
# 1 Horno microondas
# 
# Labels:bien12
# value    label
# 0     Pase
# 1 Internet
# 
# Labels:bien13
# value label
# 0  Pase
# 1 Cable
# 
# Labels:bien14
# value   label
# 0    Pase
# 1 Celular
# 
# Labels:bien15
# value   label
# 0    Pase
# 1 Ninguno
