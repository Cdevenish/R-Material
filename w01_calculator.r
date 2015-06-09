#################    Introduction to R Programming   ##################
#                                                                     #
#                           MMU FUTURES                               #
#                       Christian Devenish                            #
#                                                                     #
#                     June 2015, Manchester, UK                       #
#                 Manchester Metropolitan University                  #
#                                                                     #
#######################################################################

####  Activity 1. Using R as a calculator ####

# 1.1 Practise these operations with all the operators (plus, minus, multiplication etc), take care with the order of operators. Calculate the result in your head before you do it in R. Compare with your neighbour

1 + 1
10 - 5
10 / 2
2 * 5  # try it with or without spaces. Is there a difference
2*5
    2*5
# Los espacios pueden facilitar la lectura del código. Pero mejor uno solo.

2^3  # 2 to the power of three

10 %% 5 # modulo (remainder of integer division)
10 %% 6

10 %/% 5  # integer division (result of the division to the nearest integer)
10 %/% 6

## See this for more help:
? Arithmetic #  for all operators

# or with the help() function
help(Arithmetic)

## Careful with the order of the operators - which operators go first? 
#  ^, * /, + -  etc...

# see this for the order (ignore the operators you are not familiar with for the moment)
? Syntax

# Is there a difference between these sums?

10 + 10 / 2 - 1

(10 + 10) /2 - 1

8 - 4 * 4 + 2 #  Remember that in RStudio you can select your code in the scrip and execute it with ctrl + enter. Try it!

# If you're not sure about the order, use parenthesis...
10 + (10 / 2) - 1
10 + 10 / 2 - 1

12/24*100
(12/24)*100

# 1.2 Calculate... 

# Complete these operations using any operators and/or parenthesis ()

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


