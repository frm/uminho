/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.DataException;
import data.ProjectRepository;
import data.Repository;
import data.RepositoryFactory;
import java.util.Map;
import models.Project;

/**
 *
 * @author Tiago
 */
public class ProjectsController extends AbstractController<Project> {
    Repository repo;
    
    ProjectsController() {
        this.repo = RepositoryFactory.getProjectRepository();
    }
    
    @Override
    public Project newInstance(Map<String, Object> params) throws DataException {
        return new Project(
                (String)params.get("name"),
                (String)params.get("startDate"),
                (Float)params.get("budget"),
                (String)params.get("eta"),
                (String)params.get("notes"),
                (int)params.get("applicationId"),
                (int)params.get("paymentPlanId")  
        );
    }
    
    protected Repository getRepository() {
        return RepositoryFactory.getProjectRepository();
    }
}
