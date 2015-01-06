/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.DataException;
import data.RepositoryFactory;
import data.ProjectRepository;
import java.util.Map;
import models.Project;

/**
 *
 * @author Tiago
 */
public class ProjectController extends AbstractController<Project> {
    ProjectRepository repo;
    
    ProjectController() {
        this.repo = RepositoryFactory.getProjectRepository();
    }
    
    @Override
    public Project newInstance(Map<String, Object> params) throws DataException {
        return new Project(
                (String)params.get("name"),
                (Float)params.get("budget"),
                (String)params.get("eta"),
                (String)params.get("notes"),
                (int)params.get("applicationId"),
                (int)params.get("paymentPlanId")  
        );
    }
    
    protected ProjectRepository getRepository() {
        return RepositoryFactory.getProjectRepository();
    }
}
