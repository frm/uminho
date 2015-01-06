/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.DataException;
import data.RepositoryFactory;
import data.TaskRepository;
import java.util.Map;
import models.Task;

/**
 *
 * @author Tiago
 */
public class TaskController extends AbstractController<Task> {
    TaskRepository repo;
    
    TaskController() {
        this.repo = RepositoryFactory.getTaskRepository();
    }
    
    @Override
    public Task newInstance(Map<String, Object> params) throws DataException {
        return new Task(
                (String)params.get("name"),
                (String)params.get("startDate"),
                (String)params.get("endDate"),
                (String)params.get("status"),
                (int)params.get("project")
        );
    }
    
    protected TaskRepository getRepository() {
        return RepositoryFactory.getTaskRepository();
    }
}
