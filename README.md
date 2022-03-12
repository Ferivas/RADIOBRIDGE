# RADIOBRIDGE
Documentación RadioBridge 

## BUGS HARDWARE
En el header de programación JP1 hay que intercambiar los pines de programación JP1-5 y JP1-6 que corresponden a las líneas RXD0 y TXD0 para que sea compatible con la configuración estándar. Se soluciono este bug con un adaptador que intercambaio estas dos líneas.
El mosfet Q1 (SI2301CDS) es de tipo P. Se cambia por un mosfet N (FDN357).

El conector CN5 esta invertido (el footprint en JLCPCB no sigue asignación utilizada). Se desmonta y se suelda uno de cinco pines.

Se montan CN2, CN3 y CN6 (no había stock en JLCPCB). Hay también que soldar un zocalo DIP de 18 pines (para el DTMF HT9170B) y montar el intergado en este zócalo.

<img width="800" alt="Bugs HW" src="https://github.com/Ferivas/RADIOBRIDGE/blob/main/DOCS/Bug_HW.jpg">

## PROGRAMACION BOOTLOADER
Utilizando un programador ISP compatibles con USBASP https://www.fischl.de/usbasp/  se carga este programa teniendo en cuenta que los pines de RXDO y TXDO se encuentran ionvertidos por los que se uyiliza un adapatador como el mostrado en https://github.com/Ferivas/RADIOBRIDGE/blob/main/DOCS/Bug_HW.jpg

El programa que hay que utilizar es https://github.com/Ferivas/RADIOBRIDGE/blob/main/BASCOM/Bootloader/BootLoaderATMEGA128A_PC5_16MHz.hex el cual es el bootloader de MCS electronics (AVR BAscom) configurado a 38400 bps.
En el BASCOM se escoge la opción de "USBASP programmer" y cuando se programa el micro también es necesario programar los fusibles como se indica en la figura siguiente:

<img width="800" alt="Bugs HW" src="https://github.com/Ferivas/RADIOBRIDGE/blob/main/BASCOM/Bootloader/Fuses_USBASP.jpg">


## PROGRAMACION NORMAL
Las actualizaciones de programa se instalan por el puerto serial con un  cable USB micro en el conector USB1 de la tarjeta. Se utiliza el BASCOM o el programa Bootloader de MCSelectronics ( https://www.mcselec.com/index.php?option=com_docman&task=doc_download&gid=153&Itemid=54 ) 
Se escoge el puerto detectado en la PC a la velocidad de 39400bps y se descarga este programa https://github.com/Ferivas/RADIOBRIDGE/blob/main/BASCOM/MAINBOARD/RadioBridge_M128.hex

## CONFIGURQACION INICIAL
Es necesario inicializar el equipo a los valores por defecto. Para est se utiliza el puerto serial configurado a 9600, 8,N,1 y con un programa terminal (se recomienda Hterm) se ingresa el siguiente comando<br>
*$RSTVAR*

## CONFIGURACION DEL NUMERO DE ESTACION
Una vez programada la tarjeta el número de estación se puede configurar por el puerto serial configurado a 9600, 8,N,1 con <br>

*$setsta,Numestacion*

El número de estación actual se puede consultar con <br>

*$leesta*

