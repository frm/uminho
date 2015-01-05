use `habitat`;

INSERT INTO Funcionario
    (Nome, DataNascimento, Morada, NIF, NIB,
        Escolaridade, Nacionalidade, Naturalidade, EstadoCivil)
    VALUES ("Habitat Employee", MAKEDATE(1980, 215), "Braga", "123456789", "1234-5678-9012-34",
        "BSc", "Portuguese", "Fanecas de Cima", "Single");

INSERT INTO Atividade
    (Nome)
    VALUES ('Graphic Designer');

INSERT INTO Equipa
    (Nome, FuncionarioResp)
    VALUES ("DEFAULT", 1);

INSERT INTO Voluntario
    (Nome, Morada, NIF, NIB, DataNascimento, Nacionalidade, Naturalidade,
        EstadoCivil, Observacoes, Escolaridade, Atividade, EquipaAtual)
    VALUES("Alastair Reilly", "32nd Avenue, Bristol", "987654321", "0987-6543-2109-87", MAKEDATE(1984, 200),
       "British", "London", "Married", "I found his name on Reddit", "MSc", 1, 1);

