/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.ContactRepository;
import data.DataException;
import data.RepositoryFactory;
import java.util.Map;
import models.Contact;

/**
 *
 * @author mendes
 */
public class ContactsController extends AbstractController<Contact> {
    ContactRepository repo;
    
    ContactsController() {
        this.repo = RepositoryFactory.getContactRepository();
    }
    
    @Override
    public Contact newInstance(Map<String, Object> params) throws DataException {
        return new Contact(
                (String)params.get("type"),
                (String)params.get("value"),
                (Integer)params.get("owner"),
                (String)params.get("ownerType")            
        );
    }
    
    @Override
    protected ContactRepository getRepository() {
        return RepositoryFactory.getContactRepository();
    }
}
