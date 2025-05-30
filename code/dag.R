pacman::p_load(
  dagitty
)

# Create a DAG

dag <- dagitty('
dag {
"Acudir al CCSS" [exposure,pos="-1.229,-0.154"]
"Comunidad nativa" [selected,pos="0.402,0.319"]
"Estado civil" [adjusted,pos="-0.506,0.764"]
"Nivel educativo" [adjusted,pos="-0.036,-1.232"]
"Uso ACO" [outcome,pos="0.241,-0.170"]
Edad [adjusted,pos="-0.770,-1.176"]
"Acudir al CCSS" -> "Uso ACO"
"Estado civil" -> "Acudir al CCSS"
"Estado civil" -> "Uso ACO"
"Nivel educativo" -> "Acudir al CCSS"
"Nivel educativo" -> "Uso ACO"
Edad -> "Acudir al CCSS"
Edad -> "Uso ACO"
}
')

# Plot the DAG
plot(dag)

