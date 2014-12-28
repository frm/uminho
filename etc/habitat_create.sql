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
-- Table `habitat`.`Atividade`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Atividade` (
  `IDAtividade` INT NOT NULL,
  `Nome` VARCHAR(100) NOT NULL,
  PRIMARY KEY (`IDAtividade`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Representante`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Representante` (
  `IDRepresentante` INT NOT NULL,
  `Nome` VARCHAR(100) NOT NULL,
  `DataNascimento` DATE NOT NULL,
  `NIF` INT NOT NULL,
  `NIB` INT NOT NULL,
  `Nacionalidade` VARCHAR(45) NULL,
  `Naturalidade` VARCHAR(45) NULL,
  `Escolaridade` VARCHAR(45) NULL,
  `EstadoCivil` VARCHAR(45) NULL,
  `Atividade` INT NOT NULL,
  PRIMARY KEY (`IDRepresentante`),
  INDEX `fk_Representante_Atividade1_idx` (`Atividade` ASC),
  CONSTRAINT `fk_Representante_Atividade1`
    FOREIGN KEY (`Atividade`)
    REFERENCES `habitat`.`Atividade` (`IDAtividade`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Familia`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Familia` (
  `IDFamilia` INT NOT NULL,
  `Nome` VARCHAR(100) NOT NULL,
  `Morada` VARCHAR(100) NOT NULL,
  `Aprovada` TINYINT(1) NOT NULL,
  `Rendimento` DECIMAL(10,2) NULL,
  `Observacoes` VARCHAR(700) NULL,
  `Representante` INT NOT NULL,
  PRIMARY KEY (`IDFamilia`),
  INDEX `fk_Familia_Representante1_idx` (`Representante` ASC),
  CONSTRAINT `fk_Familia_Representante1`
    FOREIGN KEY (`Representante`)
    REFERENCES `habitat`.`Representante` (`IDRepresentante`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Membro`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Membro` (
  `IDMembro` INT NOT NULL,
  `DataNascimento` DATE NOT NULL,
  `GrauParentesco` VARCHAR(45) NOT NULL,
  `Familia` INT NOT NULL,
  PRIMARY KEY (`IDMembro`),
  INDEX `fk_Membro_Familia1_idx` (`Familia` ASC),
  CONSTRAINT `fk_Membro_Familia1`
    FOREIGN KEY (`Familia`)
    REFERENCES `habitat`.`Familia` (`IDFamilia`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Funcionario`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Funcionario` (
  `IDFuncionario` INT NOT NULL,
  `Username` VARCHAR(100) NOT NULL,
  `Password` VARCHAR(100) NOT NULL,
  `Nome` VARCHAR(100) NOT NULL,
  `DataNascimento` DATE NOT NULL,
  `Morada` VARCHAR(100) NOT NULL,
  `NIF` VARCHAR(45) NOT NULL,
  `Escolaridade` VARCHAR(100) NULL,
  `Nacionalidade` VARCHAR(100) NULL,
  `Naturalidade` VARCHAR(100) NULL,
  `EstadoCivil` VARCHAR(45) NULL,
  PRIMARY KEY (`IDFuncionario`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Candidatura`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Candidatura` (
  `IDCandidatura` INT NOT NULL,
  `DataAplicacao` DATE NOT NULL,
  `TerrenoConstrucao` VARCHAR(100) NOT NULL,
  `Prioridade` INT NOT NULL,
  `Estado` TINYINT(1) NOT NULL,
  `DataAprovacao` DATE NULL,
  `Ficheiro` BLOB NULL,
  `Observacoes` VARCHAR(500) NULL,
  `FuncionarioResp` INT NOT NULL,
  `Familia` INT NOT NULL,
  PRIMARY KEY (`IDCandidatura`),
  INDEX `fk_Candidatura_Familia1_idx` (`Familia` ASC),
  INDEX `fk_Candidatura_Funcionario1_idx` (`FuncionarioResp` ASC),
  CONSTRAINT `fk_Candidatura_Familia1`
    FOREIGN KEY (`Familia`)
    REFERENCES `habitat`.`Familia` (`IDFamilia`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Candidatura_Funcionario1`
    FOREIGN KEY (`FuncionarioResp`)
    REFERENCES `habitat`.`Funcionario` (`IDFuncionario`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Perguta`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Perguta` (
  `IDPerguta` INT NOT NULL,
  `PergTexto` VARCHAR(200) NOT NULL,
  `Ativa` TINYINT(1) NOT NULL,
  PRIMARY KEY (`IDPerguta`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Resposta`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Resposta` (
  `IDResposta` INT NOT NULL,
  `RespTexto` VARCHAR(200) NOT NULL,
  `Candidatura` INT NOT NULL,
  `Perguta` INT NOT NULL,
  PRIMARY KEY (`IDResposta`),
  INDEX `fk_Resposta_Candidatura_idx` (`Candidatura` ASC),
  INDEX `fk_Resposta_Perguta1_idx` (`Perguta` ASC),
  CONSTRAINT `fk_Resposta_Candidatura`
    FOREIGN KEY (`Candidatura`)
    REFERENCES `habitat`.`Candidatura` (`IDCandidatura`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Resposta_Perguta1`
    FOREIGN KEY (`Perguta`)
    REFERENCES `habitat`.`Perguta` (`IDPerguta`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`PlanoPagamentos`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`PlanoPagamentos` (
  `IDPlanoPagamentos` INT NOT NULL,
  `PrestacaoAtual` DECIMAL(10,2) NOT NULL,
  `Observacoes` VARCHAR(500) NULL,
  PRIMARY KEY (`IDPlanoPagamentos`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Projeto`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Projeto` (
  `IDProjeto` INT NOT NULL,
  `Nome` VARCHAR(100) NOT NULL,
  `DataInicio` DATE NOT NULL,
  `Orcamento` DECIMAL(10,2) NULL,
  `DataFinalPrevista` DATE NULL,
  `DataFinal` DATE NULL,
  `DataAssinContr` DATE NULL,
  `DataEntrgChave` DATE NULL,
  `CustoFinal` DECIMAL(10,2) NULL,
  `Observacoes` VARCHAR(700) NULL,
  `Candidatura` INT NOT NULL,
  `PlanoPagamentos` INT NOT NULL,
  PRIMARY KEY (`IDProjeto`),
  INDEX `fk_Projeto_Candidatura1_idx` (`Candidatura` ASC),
  INDEX `fk_Projeto_PlanoPagamentos1_idx` (`PlanoPagamentos` ASC),
  CONSTRAINT `fk_Projeto_Candidatura1`
    FOREIGN KEY (`Candidatura`)
    REFERENCES `habitat`.`Candidatura` (`IDCandidatura`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Projeto_PlanoPagamentos1`
    FOREIGN KEY (`PlanoPagamentos`)
    REFERENCES `habitat`.`PlanoPagamentos` (`IDPlanoPagamentos`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Prestacao`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Prestacao` (
  `IDPrestacao` INT NOT NULL,
  `Data` DATE NOT NULL,
  `Valor` INT NOT NULL,
  `Estado` INT NOT NULL,
  `PlanoPagamentos` INT NOT NULL,
  PRIMARY KEY (`IDPrestacao`),
  INDEX `fk_Prestacao_PlanoPagamentos1_idx` (`PlanoPagamentos` ASC),
  CONSTRAINT `fk_Prestacao_PlanoPagamentos1`
    FOREIGN KEY (`PlanoPagamentos`)
    REFERENCES `habitat`.`PlanoPagamentos` (`IDPlanoPagamentos`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`NomeMaterial`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`NomeMaterial` (
  `IDNomeMaterial` INT NOT NULL,
  `Nome` VARCHAR(100) NOT NULL,
  PRIMARY KEY (`IDNomeMaterial`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Material`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Material` (
  `IDMaterial` INT NOT NULL,
  `Quantidade` INT NOT NULL,
  `NomeMaterial` INT NOT NULL,
  PRIMARY KEY (`IDMaterial`),
  INDEX `fk_Material_NomeMaterial1_idx` (`NomeMaterial` ASC),
  CONSTRAINT `fk_Material_NomeMaterial1`
    FOREIGN KEY (`NomeMaterial`)
    REFERENCES `habitat`.`NomeMaterial` (`IDNomeMaterial`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Evento`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Evento` (
  `IDEvento` INT NOT NULL,
  `Data` DATE NOT NULL,
  `Local` VARCHAR(100) NOT NULL,
  `ValorObtido` DECIMAL(10,2) NOT NULL,
  `NrParticipantes` INT NULL,
  `Observacoes` VARCHAR(500) NULL,
  PRIMARY KEY (`IDEvento`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Voluntario`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Voluntario` (
  `IDVoluntario` INT NOT NULL,
  `Nome` VARCHAR(100) NOT NULL,
  `Morada` VARCHAR(100) NOT NULL,
  `NIF` INT NOT NULL,
  `NIB` INT NOT NULL,
  `DataNascimento` DATE NOT NULL,
  `Nacionalidade` VARCHAR(100) NULL,
  `Naturalidade` VARCHAR(100) NULL,
  `EstadoCivil` VARCHAR(45) NULL,
  `Observacoes` VARCHAR(500) NULL,
  `Ficheiro` BLOB NULL,
  `Atividade` INT NOT NULL,
  `Evento` INT NOT NULL,
  PRIMARY KEY (`IDVoluntario`),
  INDEX `fk_Voluntario_Atividade1_idx` (`Atividade` ASC),
  INDEX `fk_Voluntario_Evento1_idx` (`Evento` ASC),
  CONSTRAINT `fk_Voluntario_Atividade1`
    FOREIGN KEY (`Atividade`)
    REFERENCES `habitat`.`Atividade` (`IDAtividade`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Voluntario_Evento1`
    FOREIGN KEY (`Evento`)
    REFERENCES `habitat`.`Evento` (`IDEvento`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Doador`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Doador` (
  `IDDoador` INT NOT NULL,
  `Nome` VARCHAR(100) NOT NULL,
  `Tipo` TINYINT(1) NOT NULL,
  `Morada` VARCHAR(100) NOT NULL,
  `NIB` INT NOT NULL,
  `NIF` INT NOT NULL,
  `Observacoes` VARCHAR(500) NULL,
  `Atividade` INT NOT NULL,
  `Voluntario` INT NULL,
  PRIMARY KEY (`IDDoador`),
  INDEX `fk_Doador_Atividade1_idx` (`Atividade` ASC),
  INDEX `fk_Doador_Voluntario1_idx` (`Voluntario` ASC),
  CONSTRAINT `fk_Doador_Atividade1`
    FOREIGN KEY (`Atividade`)
    REFERENCES `habitat`.`Atividade` (`IDAtividade`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Doador_Voluntario1`
    FOREIGN KEY (`Voluntario`)
    REFERENCES `habitat`.`Voluntario` (`IDVoluntario`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Donativo`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Donativo` (
  `IDDonativo` INT NOT NULL,
  `Data` DATE NOT NULL,
  `Tipo` INT NOT NULL,
  `Valor` DECIMAL(10,2) NOT NULL,
  `Quantia` INT NOT NULL,
  `Material` INT NOT NULL,
  `Doador` INT NOT NULL,
  PRIMARY KEY (`IDDonativo`),
  INDEX `fk_Donativo_Material1_idx` (`Material` ASC),
  INDEX `fk_Donativo_Doador1_idx` (`Doador` ASC),
  CONSTRAINT `fk_Donativo_Material1`
    FOREIGN KEY (`Material`)
    REFERENCES `habitat`.`Material` (`IDMaterial`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Donativo_Doador1`
    FOREIGN KEY (`Doador`)
    REFERENCES `habitat`.`Doador` (`IDDoador`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Tarefa`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Tarefa` (
  `IDTarefa` INT NOT NULL,
  `Nome` VARCHAR(50) NOT NULL,
  `DataInicio` DECIMAL(10,2) NOT NULL,
  `DataFinal` DECIMAL(10,2) NOT NULL,
  `Estado` INT NOT NULL,
  `Projeto` INT NOT NULL,
  PRIMARY KEY (`IDTarefa`),
  INDEX `fk_Tarefa_Projeto1_idx` (`Projeto` ASC),
  CONSTRAINT `fk_Tarefa_Projeto1`
    FOREIGN KEY (`Projeto`)
    REFERENCES `habitat`.`Projeto` (`IDProjeto`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`VoluntarioTarefa`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`VoluntarioTarefa` (
  `Voluntario` INT NOT NULL,
  `Tarefa` INT NOT NULL,
  `NrHoras` INT NULL,
  PRIMARY KEY (`Voluntario`, `Tarefa`),
  INDEX `fk_Voluntario_has_Tarefa_Tarefa1_idx` (`Tarefa` ASC),
  INDEX `fk_Voluntario_has_Tarefa_Voluntario1_idx` (`Voluntario` ASC),
  CONSTRAINT `fk_Voluntario_has_Tarefa_Voluntario1`
    FOREIGN KEY (`Voluntario`)
    REFERENCES `habitat`.`Voluntario` (`IDVoluntario`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Voluntario_has_Tarefa_Tarefa1`
    FOREIGN KEY (`Tarefa`)
    REFERENCES `habitat`.`Tarefa` (`IDTarefa`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Equipa`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Equipa` (
  `IDEquipa` INT NOT NULL,
  `Nome` VARCHAR(100) NOT NULL,
  `Voluntario` INT NOT NULL,
  `FuncionarioResp` INT NOT NULL,
  PRIMARY KEY (`IDEquipa`),
  INDEX `fk_Equipa_Voluntario1_idx` (`Voluntario` ASC),
  INDEX `fk_Equipa_Funcionario1_idx` (`FuncionarioResp` ASC),
  CONSTRAINT `fk_Equipa_Voluntario1`
    FOREIGN KEY (`Voluntario`)
    REFERENCES `habitat`.`Voluntario` (`IDVoluntario`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Equipa_Funcionario1`
    FOREIGN KEY (`FuncionarioResp`)
    REFERENCES `habitat`.`Funcionario` (`IDFuncionario`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`DonativoProjeto`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`DonativoProjeto` (
  `Donativo` INT NOT NULL,
  `Projeto` INT NOT NULL,
  PRIMARY KEY (`Donativo`, `Projeto`),
  INDEX `fk_Donativo_has_Projeto_Projeto1_idx` (`Projeto` ASC),
  INDEX `fk_Donativo_has_Projeto_Donativo1_idx` (`Donativo` ASC),
  CONSTRAINT `fk_Donativo_has_Projeto_Donativo1`
    FOREIGN KEY (`Donativo`)
    REFERENCES `habitat`.`Donativo` (`IDDonativo`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Donativo_has_Projeto_Projeto1`
    FOREIGN KEY (`Projeto`)
    REFERENCES `habitat`.`Projeto` (`IDProjeto`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `habitat`.`Contacto`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `habitat`.`Contacto` (
  `IDContacto` INT NOT NULL,
  `Nome` INT NOT NULL,
  `Valor` VARCHAR(30) NOT NULL,
  `idDono` INT NOT NULL,
  `TipoDono` VARCHAR(45) NULL,
  `Representante_IDRepresentante` INT NOT NULL,
  PRIMARY KEY (`IDContacto`),
  INDEX `fk_Contacto_Doador1_idx` (`idDono` ASC),
  INDEX `fk_Contacto_Representante1_idx` (`Representante_IDRepresentante` ASC),
  CONSTRAINT `fk_Contacto_Doador1`
    FOREIGN KEY (`idDono`)
    REFERENCES `habitat`.`Doador` (`IDDoador`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Contacto_Voluntario1`
    FOREIGN KEY (`idDono`)
    REFERENCES `habitat`.`Voluntario` (`IDVoluntario`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Contacto_Representante1`
    FOREIGN KEY (`Representante_IDRepresentante`)
    REFERENCES `habitat`.`Representante` (`IDRepresentante`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
