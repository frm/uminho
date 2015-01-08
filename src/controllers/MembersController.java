/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.DataException;
import data.MemberRepository;
import data.Repository;
import data.RepositoryFactory;
import java.util.Map;
import models.SimpleMember;

/**
 *
 * @author mendes
 */
public class MembersController extends AbstractController<SimpleMember> {
    Repository repo;

    MembersController() {
        this.repo = RepositoryFactory.getMemberRepository();
    }

    @Override
    protected Repository getRepository() {
        return RepositoryFactory.getMemberRepository();
    }

    @Override
    public SimpleMember newInstance(final Map<String, Object> params) throws DataException {
        return new SimpleMember(
                (String)params.get("name"),
                (String)params.get("birthDate"),
                (String)params.get("kinship"),
                (Integer)params.get("familyID")
        );
    }
}

