######################    Introducción a R    #########################
#                                                                     #
#     Herramientas para el análisis de información de biodiversidad   #
#                       Christian Devenish                            #
#                                                                     #
#                     Abril 2014, Lima, Perú                          #
#                 Centro de Sostenbilidad Ambiental                   #
#                                                                     #
#######################################################################

#### Clase 1. ####

####  Actividad 1. R como una calculadora ####

# 1.1 Practica operaciones con todos los operadores, fijándote en el orden de los operadores. Calcula el resultado en tu cabeza antes de hacerlo en R. Preguntale a tu compañero

1 + 1
10 - 5
10 / 2
2 * 5  # sin o con espacios - no importa
2*5
    2*5
# Los espacios pueden facilitar la lectura del código. Pero mejor uno solo.

2^3  # potenciación

10 %% 5 # módulo (resto de la división entera)
10 %% 6

10 %/% 5  # división entera (resultado de la division sin decimal)
10 %/% 6

## ver para ayuda
? Arithmetic #  para todos

# o con la funcion
help(Arithmetic)

## Ojo con el orden de operadores (igual que en 3o de primaria...)
# %...%, ^, * /, + -  etc...

#ver 
? Syntax

# por ejemplo

10 + 10 / 2 - 1

(10 + 10) /2 - 1

8 - 4 * 4 + 2 #  En RStudio puedes seleccionar una parte del código en el script y ejecutarlo con ctrl + enter. ¡Pruebalo!

# si no te acuerdas del orden, o tienes dudas, usa parentesis....
10 + (10 / 2) - 1
10 + 10 / 2 - 1


# 1.2 Resolver... 

# Completa estas operaciones usando cualquier de los operadores y parentesis

# Ejemplos

#i#  10  5   2   8   = 3.2
10 / 5 ^ 2 * 8 

#ii#  10 5  5  2 = 2 
(10 - 5) / 5 * 2 

# ¡Practica!
#iii#  2  2  4  5  10 = 51

#iv#  3  3  12  3  10 = 30

#v# Escribe una operación que dé como resultado 50, usando estos operadores *  /  ^  
#EJEMPLO
2 * 100 / 2^2

#vi#  Escribe una operación que dé 39, usando estos operadores ^  /  *  +


#vii# Escribe una operación que dé 120, usando por lo menos 5 operadores


#viii#  Construye el resultado de 10/6, usando estos operadores %%  +  %/%  /


