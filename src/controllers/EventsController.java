/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.DataException;
import data.EventsRepository;
import data.Repository;
import data.RepositoryFactory;
import java.util.Map;
import models.Event;

/**
 *
 * @author paulo
 */
public class EventsController extends AbstractController<Event>{
    Repository repo;
    
    EventsController() {
        this.repo = RepositoryFactory.getEventsRepository();
    }

    @Override
    public Event newInstance(Map<String, Object> params) throws DataException {
         return new Event(
                (String)params.get("date"),
                (Float)params.get("amountRaised"),
                (Integer)params.get("participantsNr"),
                (String)params.get("location"),
                (String)params.get("observations")     
        );
    } 
    
    
    
    protected Repository getRepository() {
        return RepositoryFactory.getEventsRepository();
    }
    
    
    
       
}