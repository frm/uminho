-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

-- -----------------------------------------------------
-- Schema habitat
-- -----------------------------------------------------

-- -----------------------------------------------------
-- Schema habitat
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `habitat` DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci ;
USE `habitat` ;

-- -----------------------------------------------------
-- Table `habitat`.`Familia`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Familia` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Nome` VARCHAR(100) NOT NULL,
  `Morada` VARCHAR(100) NOT NULL,
  `Aprovada` TINYINT(1) NOT NULL,
  `HorasVoluntariado` INT NOT NULL DEFAULT 0,
  `Rendimento` DECIMAL(10,2) NULL,
  `Observacoes` VARCHAR(700) NULL,
  PRIMARY KEY (`id`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Atividade`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Atividade` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Nome` VARCHAR(100) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE INDEX `Nome` (`Nome` ASC))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Representante`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Representante` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Nome` VARCHAR(100) NOT NULL,
  `DataNascimento` DATE NOT NULL,
  `NIF` VARCHAR(10) NULL,
  `NIB` VARCHAR(30) NULL,
  `Nacionalidade` VARCHAR(45) NULL,
  `Naturalidade` VARCHAR(45) NULL,
  `Escolaridade` VARCHAR(45) NULL,
  `EstadoCivil` VARCHAR(45) NULL,
  `Atividade` INT NOT NULL,
  `Familia` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_Representante_Atividade1_idx` (`Atividade` ASC),
  INDEX `fk_Representante_Familia1_idx` (`Familia` ASC),
  CONSTRAINT `fk_Representante_Atividade1`
    FOREIGN KEY (`Atividade`)
    REFERENCES `habitat`.`Atividade` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Representante_Familia1`
    FOREIGN KEY (`Familia`)
    REFERENCES `habitat`.`Familia` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Membro`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Membro` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Nome` VARCHAR(100) NOT NULL,
  `DataNascimento` DATE NOT NULL,
  `GrauParentesco` VARCHAR(45) NOT NULL,
  `Familia` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_Membro_Familia1_idx` (`Familia` ASC),
  CONSTRAINT `fk_Membro_Familia1`
    FOREIGN KEY (`Familia`)
    REFERENCES `habitat`.`Familia` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Funcionario`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Funcionario` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Nome` VARCHAR(100) NOT NULL,
  `DataNascimento` DATE NOT NULL,
  `Morada` VARCHAR(100) NOT NULL,
  `NIF` VARCHAR(10) NULL,
  `NIB` VARCHAR(30) NULL,
  `Escolaridade` VARCHAR(100) NULL,
  `Nacionalidade` VARCHAR(100) NULL,
  `Naturalidade` VARCHAR(100) NULL,
  `EstadoCivil` VARCHAR(45) NULL,
  `Salario` DECIMAL(10,2) NULL,
  PRIMARY KEY (`id`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Candidatura`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Candidatura` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `DataCandidatura` DATE NOT NULL,
  `TerrenoConstrucao` VARCHAR(100) NOT NULL,
  `Prioridade` INT NOT NULL,
  `Estado` TINYINT(1) NOT NULL,
  `DataAprovacao` DATE NULL,
  `Ficheiro` BLOB NULL,
  `Observacoes` VARCHAR(500) NULL,
  `FuncionarioResp` INT NOT NULL,
  `Familia` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_Candidatura_Familia1_idx` (`Familia` ASC),
  INDEX `fk_Candidatura_Funcionario1_idx` (`FuncionarioResp` ASC),
  CONSTRAINT `fk_Candidatura_Familia1`
    FOREIGN KEY (`Familia`)
    REFERENCES `habitat`.`Familia` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Candidatura_Funcionario1`
    FOREIGN KEY (`FuncionarioResp`)
    REFERENCES `habitat`.`Funcionario` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Pergunta`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Pergunta` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `PergTexto` VARCHAR(200) NOT NULL,
  `Ativa` TINYINT(1) NOT NULL,
  PRIMARY KEY (`id`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`CandidaturaPergunta`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`CandidaturaPergunta` (
  `Candidatura` INT NOT NULL,
  `Pergunta` INT NOT NULL,
  `RespTexto` VARCHAR(300) NOT NULL,
  PRIMARY KEY (`Candidatura`, `Pergunta`),
  INDEX `fk_Resposta_Candidatura_idx` (`Candidatura` ASC),
  INDEX `fk_Resposta_Perguta1_idx` (`Pergunta` ASC),
  CONSTRAINT `fk_Resposta_Candidatura`
    FOREIGN KEY (`Candidatura`)
    REFERENCES `habitat`.`Candidatura` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Resposta_Perguta1`
    FOREIGN KEY (`Pergunta`)
    REFERENCES `habitat`.`Pergunta` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Projeto`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Projeto` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Nome` VARCHAR(100) NOT NULL,
  `DataInicio` DATE NOT NULL,
  `Orcamento` DECIMAL(10,2) NULL,
  `DataFinalPrevista` DATE NULL,
  `DataFinal` DATE NULL,
  `DataAssinContr` DATE NULL,
  `DataEntrgChave` DATE NULL,
  `CustoFinal` DECIMAL(10,2) NULL,
  `Observacoes` VARCHAR(500) NULL,
  `Candidatura` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_Projeto_Candidatura1_idx` (`Candidatura` ASC),
  CONSTRAINT `fk_Projeto_Candidatura1`
    FOREIGN KEY (`Candidatura`)
    REFERENCES `habitat`.`Candidatura` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`PlanoPagamentos`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`PlanoPagamentos` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `ProximaPrestacao` DECIMAL(10,2) NOT NULL,
  `Observacoes` VARCHAR(500) NULL,
  `Projeto` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_PlanoPagamentos_Projeto1_idx` (`Projeto` ASC),
  CONSTRAINT `fk_PlanoPagamentos_Projeto1`
    FOREIGN KEY (`Projeto`)
    REFERENCES `habitat`.`Projeto` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Prestacao`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Prestacao` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `DataLimite` DATE NOT NULL,
  `DataPagamento` DATE NULL,
  `Valor` INT NOT NULL,
  `PlanoPagamentos` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_Prestacao_PlanoPagamentos1_idx` (`PlanoPagamentos` ASC),
  CONSTRAINT `fk_Prestacao_PlanoPagamentos1`
    FOREIGN KEY (`PlanoPagamentos`)
    REFERENCES `habitat`.`PlanoPagamentos` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Doador`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Doador` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Nome` VARCHAR(100) NOT NULL,
  `Tipo` TINYINT(1) NOT NULL,
  `Morada` VARCHAR(100) NOT NULL,
  `DataUltimoDonativo` DATE NULL,
  `NIB` VARCHAR(30) NULL,
  `NIF` VARCHAR(10) NULL,
  `Observacoes` VARCHAR(500) NULL,
  `Atividade` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_Doador_Atividade1_idx` (`Atividade` ASC),
  CONSTRAINT `fk_Doador_Atividade1`
    FOREIGN KEY (`Atividade`)
    REFERENCES `habitat`.`Atividade` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`NomeMaterial`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`NomeMaterial` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Nome` VARCHAR(100) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE INDEX `Nome` (`Nome` ASC))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Donativo`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Donativo` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Data` DATE NOT NULL,
  `Tipo` INT NOT NULL,
  `Valor` DECIMAL(10,2) NOT NULL,
  `Quantidade` INT NOT NULL,
  `Usado` TINYINT(1) NOT NULL,
  `Observacoes` VARCHAR(500) NULL,
  `Projeto` INT NULL,
  `NomeMaterial` INT NULL,
  `Doador` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_Donativo_Doador1_idx` (`Doador` ASC),
  INDEX `fk_Donativo_Projeto1_idx` (`Projeto` ASC),
  INDEX `fk_Donativo_NomeMaterial1_idx` (`NomeMaterial` ASC),
  CONSTRAINT `fk_Donativo_Doador1`
    FOREIGN KEY (`Doador`)
    REFERENCES `habitat`.`Doador` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Donativo_Projeto1`
    FOREIGN KEY (`Projeto`)
    REFERENCES `habitat`.`Projeto` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Donativo_NomeMaterial1`
    FOREIGN KEY (`NomeMaterial`)
    REFERENCES `habitat`.`NomeMaterial` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Material`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Material` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Quantidade` INT NOT NULL,
  `NomeMaterial` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_Material_NomeMaterial1_idx` (`NomeMaterial` ASC),
  CONSTRAINT `fk_Material_NomeMaterial1`
    FOREIGN KEY (`NomeMaterial`)
    REFERENCES `habitat`.`NomeMaterial` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Equipa`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Equipa` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Nome` VARCHAR(100) NOT NULL,
  `FuncionarioResp` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_Equipa_Funcionario1_idx` (`FuncionarioResp` ASC),
  CONSTRAINT `fk_Equipa_Funcionario1`
    FOREIGN KEY (`FuncionarioResp`)
    REFERENCES `habitat`.`Funcionario` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Voluntario`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Voluntario` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Nome` VARCHAR(100) NOT NULL,
  `Morada` VARCHAR(100) NOT NULL,
  `NIF` VARCHAR(10) NULL,
  `NIB` VARCHAR(30) NULL,
  `DataNascimento` DATE NOT NULL,
  `Nacionalidade` VARCHAR(100) NULL,
  `Naturalidade` VARCHAR(100) NULL,
  `EstadoCivil` VARCHAR(45) NULL,
  `Observacoes` VARCHAR(500) NULL,
  `Escolaridade` VARCHAR(45) NULL,
  `Ficheiro` BLOB NULL,
  `Doador` INT NULL,
  `Atividade` INT NOT NULL,
  `EquipaAtual` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_Voluntario_Atividade1_idx` (`Atividade` ASC),
  INDEX `fk_Voluntario_Equipa1_idx` (`EquipaAtual` ASC),
  INDEX `fk_Voluntario_Doador1_idx` (`Doador` ASC),
  CONSTRAINT `fk_Voluntario_Atividade1`
    FOREIGN KEY (`Atividade`)
    REFERENCES `habitat`.`Atividade` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Voluntario_Equipa1`
    FOREIGN KEY (`EquipaAtual`)
    REFERENCES `habitat`.`Equipa` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Voluntario_Doador1`
    FOREIGN KEY (`Doador`)
    REFERENCES `habitat`.`Doador` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Evento`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Evento` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Data` DATE NOT NULL,
  `Local` VARCHAR(100) NOT NULL,
  `ValorObtido` DECIMAL(10,2) NULL,
  `NrParticipantes` INT NULL,
  `Observacoes` VARCHAR(500) NULL,
  PRIMARY KEY (`id`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Tarefa`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Tarefa` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Nome` VARCHAR(50) NOT NULL,
  `DataInicio` DECIMAL(10,2) NULL,
  `DataFinal` DECIMAL(10,2) NULL,
  `Terminada` TINYINT(1) NOT NULL,
  `Projeto` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_Tarefa_Projeto1_idx` (`Projeto` ASC),
  CONSTRAINT `fk_Tarefa_Projeto1`
    FOREIGN KEY (`Projeto`)
    REFERENCES `habitat`.`Projeto` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`VoluntarioTarefa`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`VoluntarioTarefa` (
  `Voluntario` INT NOT NULL,
  `Tarefa` INT NOT NULL,
  `NrHoras` INT NOT NULL DEFAULT 0,
  PRIMARY KEY (`Voluntario`, `Tarefa`),
  INDEX `fk_Voluntario_has_Tarefa_Tarefa1_idx` (`Tarefa` ASC),
  INDEX `fk_Voluntario_has_Tarefa_Voluntario1_idx` (`Voluntario` ASC),
  CONSTRAINT `fk_Voluntario_has_Tarefa_Voluntario1`
    FOREIGN KEY (`Voluntario`)
    REFERENCES `habitat`.`Voluntario` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Voluntario_has_Tarefa_Tarefa1`
    FOREIGN KEY (`Tarefa`)
    REFERENCES `habitat`.`Tarefa` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`DonativoProjeto`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`DonativoProjeto` (
  `Donativo` INT NOT NULL,
  `Projeto` INT NOT NULL,
  `Usado` TINYINT(1) NULL,
  PRIMARY KEY (`Donativo`, `Projeto`),
  INDEX `fk_Donativo_has_Projeto_Projeto1_idx` (`Projeto` ASC),
  INDEX `fk_Donativo_has_Projeto_Donativo1_idx` (`Donativo` ASC),
  CONSTRAINT `fk_Donativo_has_Projeto_Donativo1`
    FOREIGN KEY (`Donativo`)
    REFERENCES `habitat`.`Donativo` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Donativo_has_Projeto_Projeto1`
    FOREIGN KEY (`Projeto`)
    REFERENCES `habitat`.`Projeto` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Contacto`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Contacto` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `Tipo` INT NOT NULL,
  `Valor` VARCHAR(30) NOT NULL,
  `TipoDono` VARCHAR(45) NOT NULL,
  `Dono` INT NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_Contacto_Doador1_idx` (`Dono` ASC),
  CONSTRAINT `fk_Contacto_Doador1`
    FOREIGN KEY (`Dono`)
    REFERENCES `habitat`.`Doador` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Contacto_Voluntario1`
    FOREIGN KEY (`Dono`)
    REFERENCES `habitat`.`Voluntario` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Contacto_Representante1`
    FOREIGN KEY (`Dono`)
    REFERENCES `habitat`.`Representante` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Contacto_Funcionario1`
    FOREIGN KEY (`Dono`)
    REFERENCES `habitat`.`Funcionario` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`EventoVoluntario`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`EventoVoluntario` (
  `Evento` INT NOT NULL,
  `Voluntario` INT NOT NULL,
  PRIMARY KEY (`Evento`, `Voluntario`),
  INDEX `fk_Evento_has_Voluntario_Voluntario1_idx` (`Voluntario` ASC),
  INDEX `fk_Evento_has_Voluntario_Evento1_idx` (`Evento` ASC),
  CONSTRAINT `fk_Evento_has_Voluntario_Evento1`
    FOREIGN KEY (`Evento`)
    REFERENCES `habitat`.`Evento` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Evento_has_Voluntario_Voluntario1`
    FOREIGN KEY (`Voluntario`)
    REFERENCES `habitat`.`Voluntario` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`VoluntarioEquipa`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`VoluntarioEquipa` (
  `Voluntario` INT NOT NULL,
  `Equipa` INT NOT NULL,
  PRIMARY KEY (`Voluntario`, `Equipa`),
  INDEX `fk_Voluntario_has_Equipa_Equipa1_idx` (`Equipa` ASC),
  INDEX `fk_Voluntario_has_Equipa_Voluntario1_idx` (`Voluntario` ASC),
  CONSTRAINT `fk_Voluntario_has_Equipa_Voluntario1`
    FOREIGN KEY (`Voluntario`)
    REFERENCES `habitat`.`Voluntario` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Voluntario_has_Equipa_Equipa1`
    FOREIGN KEY (`Equipa`)
    REFERENCES `habitat`.`Equipa` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`MaterialProjeto`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`MaterialProjeto` (
  `Material` INT NOT NULL,
  `Projeto` INT NOT NULL,
  `Quantidade` INT NOT NULL DEFAULT 0,
  PRIMARY KEY (`Material`, `Projeto`),
  INDEX `fk_Material_has_Projeto_Projeto1_idx` (`Projeto` ASC),
  INDEX `fk_Material_has_Projeto_Material1_idx` (`Material` ASC),
  CONSTRAINT `fk_Material_has_Projeto_Material1`
    FOREIGN KEY (`Material`)
    REFERENCES `habitat`.`Material` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Material_has_Projeto_Projeto1`
    FOREIGN KEY (`Projeto`)
    REFERENCES `habitat`.`Projeto` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
