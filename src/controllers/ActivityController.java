/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.ActivityRepository;
import data.DataException;
import data.RepositoryFactory;
import java.util.Map;
import models.Activity;

/**
 *
 * @author mendes
 */
public class ActivityController implements Controller<Activity> {
    ActivityRepository repo;
    
    ActivityController() {
        this.repo = RepositoryFactory.getActivityRepository();
    }
    
    @Override
    public Activity add(Map<String, Object> params) throws DataException {
        Activity a = newInstance(params);
        repo.save(a);
        return a;
    }
    
    @Override
    public Activity newInstance(Map<String, Object> params) {
        return new Activity( (String)params.get("name") );
    }
    
    @Override
    public Activity find(int id) throws DataException {
        return repo.find(id);
    }
}
