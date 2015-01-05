/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.DataException;
import data.RepositoryFactory;
import data.VolunteersRepository;
import java.sql.Date;
import java.util.HashSet;
import java.util.Map;
import models.Activity;
import models.Contact;
import models.Volunteer;

/**
 *
 * @author mendes
 */
public class VolunteersController implements Controller<Volunteer> {
    VolunteersRepository repo;
    
    VolunteersController() {
        this.repo = RepositoryFactory.getVolunteersRepository();
    }
    
    @Override
    public Volunteer add(Map<String, Object> params) throws DataException {
        Volunteer v = newInstance(params);
        repo.save(v);
        return v;
    }
    
    @Override
    public Volunteer newInstance(Map<String, Object> params) throws DataException {
        return new Volunteer(
                (String)params.get("name"),
                (String)params.get("address"),
                (String)params.get("nif"),
                (String)params.get("nib"),
                (Activity)ControllerFactory.getActivityController().find( (Integer)params.get("activity") ),
                (HashSet<Contact>)params.get("contacts"),
                (String)params.get("birthDate"),
                (String)params.get("education"),
                (String)params.get("nationality"),
                (String)params.get("citizenship"),
                (String)params.get("maritalStatus"),
                (String)params.get("observations"),
                (String)params.get("file")                
        );
    }
    
    @Override
    public Volunteer find(int id) throws DataException {
        return repo.find(id);
    }
}
