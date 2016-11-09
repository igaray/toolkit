#!/bin/bash
# Obtener contraseña de un rar con fuerza bruta en Linux
# Este es un método de fuerza bruta bastante tosco, pero que denota lo facil 
# que puede ser implementar un método de fuerza bruta para obtener la clave. 
# El procedimiento va probando todas las claves de un diccionario hasta que da 
# con la buena:

for i in $(cat $1)
do
    unrar e -p$i $2
    if [ $? = 0 ]
    then
        echo “Passwd Found: $i”
        break
    fi
done

# Que el resultado sea un exito, depende de nuestra paciencia y de la potencia 
# del diccionario que usemos.
