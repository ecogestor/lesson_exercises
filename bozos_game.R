full_house <- 0
sequencia <- 0
quadra <- 0
general <- 0
i <- 1
simulacao <- 1000000

while(i < simulacao) {
    lance <- sample(1:6, size = 5, replace = TRUE) |> 
        sort() |> 
        diff()
    
    # Para se obter um general a soma do lance deve ser igual a zero
    if (sum(lance) == 0){
        general <- general + 1
    }
    
    # Para se obter um full house a soma do dois primeiros valores  
    # e ultimo valor do lance devem ser zero ou inverso e conforme o
    # caso o terceiro ou segundo valor deve ser diferente de zero
    else if ((sum(lance[1:2]) == 0 & 
              lance[4] == 0 &
              lance[3] != 0)|
             (sum(lance[3:4]) == 0 &
              lance[1] == 0 & 
              lance[2] != 0))
    {
        full_house <- full_house + 1
    }
    
    # Para se obter um quadra ou quadrada os três primeiros ou três 
    # ultimos valores devem somar zero e o primeiro ou ultimo valor 
    # conforme o caso deve ser diferente de zero
    else if ((sum(lance[1:3]) == 0 & lance[4] != 0) |
             (sum(lance[2:4]) == 0 & lance[1] != 0)) {
        quadra <- quadra + 1
    }

    # Para se obter uma sequência todos os valores do diff do lance 
    # devem ser 1
    else if (all(lance == c(1, 1, 1, 1))){
        sequencia <- sequencia + 1
    }
    
i <- i + 1
}

p_general <- round(general/simulacao, 4)*100
p_quadra <- round(quadra/simulacao, 4)*100
p_full_house <- round(full_house/simulacao, 4)*100
p_sequencia <- round(sequencia/simulacao, 4)*100

cat("\n",
    "Em", simulacao, "simulações foram obtidas as probabilidades:","\n",
    p_general,"% para o General","\n",
    p_quadra,"% para a Quadra","\n",
    p_full_house,"% para o Full House","\n",
    p_sequencia,"% para o Sequência")
