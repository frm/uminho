/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.DataException;
import data.QuestionRepository;
import data.RepositoryFactory;
import java.util.Map;
import models.Question;

/**
 *
 * @author mendes
 */
public class QuestionsController extends AbstractController<Question> {
    QuestionRepository repo;
    
    QuestionsController() {
        this.repo = RepositoryFactory.getQuestionRepository();
    }
    
    @Override
    public Question newInstance(Map<String, Object> params) throws DataException {
        return new Question(
                (String)params.get("text"),
                (Boolean)params.get("enabled")
        );
    }
    
    @Override
    protected QuestionRepository getRepository() {
        return RepositoryFactory.getQuestionRepository();
    }
}

