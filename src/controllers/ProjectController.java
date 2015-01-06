/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.DataException;
import data.RepositoryFactory;
import data.ProjectRepository;
import java.util.HashSet;
import java.util.Map;
import models.Contact;
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
                (String)params.get("startDate"),
                (String)params.get("budget"),
                (String)params.get("eta"),
                (String)params.get("endDate"),
                (String)params.get("signDate"),
                (String)params.get("deliveryDate"),
                (String)params.get("finalCost"),
                (String)params.get("notes"),
                (String)params.get("paymentPlan"),
                (String)params.get("tasks")                
        );
    }
    
    protected ProjectRepository getRepository() {
        return RepositoryFactory.getProjectRepository();
    }
}
