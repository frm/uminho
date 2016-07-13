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
import models.Activity;
import models.Representative;

/**
 *
 * @author mendes
 */
public class RepresentativesController extends AbstractController<Representative> {
    Repository repo;

    RepresentativesController() {
        this.repo = RepositoryFactory.getRepresentativeRepository();
    }

    @Override
    protected Repository getRepository() {
        return RepositoryFactory.getRepresentativeRepository();
    }

    @Override
    public Representative newInstance(final Map<String, Object> params) throws DataException {
        return new Representative(
                (String)params.get("name"),
                (String)params.get("birthDate"),
                (Integer)params.get("familyID"),
                (String)params.get("nib"),
                (String)params.get("nif"),
                (String)params.get("maritalStatus"),
                (String)params.get("education"),
                (String)params.get("nationality"),
                (String)params.get("birthPlace"),
                (Activity)params.get("activity")
        );
    }



}
