/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.DataException;
import data.Repository;
import data.RepositoryFactory;
import java.util.Map;
import models.Family;

/**
 *
 * @author mendes
 */
public class FamiliesController extends AbstractController<Family> {
    Repository repo;
    
    FamiliesController() {
        this.repo = RepositoryFactory.getFamilyRepository();
    }
    
    @Override
    protected Repository getRepository() {
        return RepositoryFactory.getFamilyRepository();
    }
    
    @Override
    public Family newInstance(final Map<String, Object> params) throws DataException {
        return new Family(
                (String)params.get("name"),
                (String)params.get("address"),
                (Float)params.get("income"),
                (String)params.get("observations")
        );
    }
}


