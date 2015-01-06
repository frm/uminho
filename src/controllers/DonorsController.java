/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.DataException;
import data.DonorRepository;
import data.RepositoryFactory;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import models.Activity;
import models.Contact;
import models.Donor;

/**
 *
 * @author pc14
 */
public class DonorsController extends AbstractController<Donor>{
    DonorRepository repo;
    
    DonorsController() {
        this.repo = RepositoryFactory.getDonorRepository();
    }
    
    @Override
    protected DonorRepository getRepository() {
        return RepositoryFactory.getDonorRepository();        
    }

    @Override
    public Donor newInstance(final Map<String, Object> params) throws DataException {
        return new Donor(
                (String)params.get("name"),
                (String)params.get("address"),
                (String)params.get("nif"),
                (String)params.get("nib"),
                (Activity)ControllerFactory.getActivityController().findBy(new HashMap<String, Object>() {{
                    put("name", params.get("activity"));
                }}).get(0),
                (HashSet<Contact>) params.get("contacts"),
                (Boolean)params.get("dType"),
                (String)params.get("observations")                
        );
    }
    
}
