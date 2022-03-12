# RADIOBRIDGE
Documentación RadioBridge 

## BUGS HARDWARE
En el header de programación JP1 hay que intercambiar los pines de programación JP1-5 y JP1-6 que corresponden a las líneas RXD0 y TXD0 para que sea compatible con la configuración estándar. Se soluciono este bug con un adaptador que intercambaio estas dos líneas.
El mosfet Q1 (SI2301CDS) es de tipo P. Se cambia por un mosfet N (FDN357).
El conector CN5 esta invertido (el footprint en JLCPCB no sigue asignación utilizada). Se desmonta y se suelda uno de cinco pines.
Se montan CN2, CN3 y CN6 (no había stock en JLCPCB).
<img width="400" alt="Bugs HW" src="https://github.com/Ferivas/RADIOBRIDGE/blob/main/DOCS/Bug_HW.jpg">

